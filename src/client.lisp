(in-package :cl-java-sound-client)

;;
;; Frames-Builder
;;

(defun make-frames-builder ()
  (let ((os (flexi-streams:make-in-memory-output-stream
	     :element-type '(unsigned-byte 8))))
    (list
     (lambda (sample)
       (cl-java-sound-client-message:write-sample os sample))
     (lambda()
       (flexi-streams:get-output-stream-sequence os)))))

(defun write-sample (frames-builder sample)
  (funcall (first frames-builder) sample))

(defun get-samples (frames-builder)
  (funcall (second frames-builder)))

;;
;; Connection
;;

(defclass connection ()
  ((port :initarg :port :initform 9000)
   (host :initarg :host :initform "localhost")
   (controller :initarg :controller)
   (socket :initform nil)
   (stream :initform nil)
   (send-message-lock :initform (bt:make-lock))))

(defgeneric start-message-loop (connection))
(defgeneric handle-message (connection message))
(defgeneric send-start-message (connection))
(defgeneric send-stop-message (connection))
(defgeneric send-frames-message (connection sample-data))
(defgeneric send-init-message (connection &key sample-rate channel-count buffer-size))
(defgeneric close-connection (connection))
(defgeneric send-init-session-data (connection))

;;
;; Controller
;;

(defclass controller ()
  ((connection :initform nil)
   (channel-count :initarg :channel-count)
   (sample-width :initarg :sample-width :initform 2)
   (buffer-size ::initarg :buffer-size :initform 0)
   (sample-rate :initarg :sample-rate :initform 44100)
   (requested-frame-count :initform 0)))

(defgeneric stop (controller)
  (:documentation
   "Pause playback. Sends a Stop message to the server."))

(defgeneric start (controller)
  (:documentation
   "Continue playback. Sends a Start message to the server."))

(defgeneric close-connection (controller)
  (:documentation
   "Sends a close message to the server in order to indicate that the connection shall be closed."))

(defgeneric frames (controller)
  (:documentation
   "Sends audio data to the server."))

(defgeneric notify-frames-requested (controller)
  (:documentation
   "Is called when controller is supposed to send audio data to the server. Must be implemented
    by a controller."))

(defgeneric notify-frames-requested-impl (controller frame-count))

(defgeneric render-frame (controller sample-buffer)
  (:documentation
   "Render a frame. Must be implemented by a controller.
    sample-buffer: Transfer object for the rendered samples. An array of length channel count.
    Return value: t on success."))

(defgeneric notify-connection-closed (controller)
  (:documentation
   "Is called when the connection to the server has been closed."))

(defgeneric notify-connection-established (controller)
  (:documentation
   "Is called when the connection to the server has been established
    and the server is ready to receive audio data."))

(defgeneric run (controller)
  (:documentation
   "Starts the event processing loop. Returns when connection has been closed."))

(defun get-controller-connection (controller)
  "Returns the connection belonging to the given controller."
  (slot-value controller 'connection))

(defun get-channel-count (controller)
  (slot-value controller 'channel-count))

(defun get-sample-width (controller)
  (slot-value controller 'sample-width))

(defun get-buffer-size (controller)
  (slot-value controller 'buffer-size))

(defun get-sample-rate (controller)
  (slot-value controller 'sample-rate))

;;
;; Connection Impl
;;

(defun expect-ack (stream)
  (let ((message (cl-java-sound-client-message:read-message stream)))
    (if (not (cl-java-sound-client-message:ack-message-p message))
	(error 'simple-error
	       :format-control "Expected AckMessage but got ~a"
	       :format-arguments (list message)))))

(defmethod initialize-instance :after ((instance connection) &rest args)
  (declare (ignore args))
  (setf (slot-value instance 'socket)
	(usocket:socket-connect
	 (slot-value instance 'host)
	 (slot-value instance 'port)
	 :element-type '(unsigned-byte 8)))
  (setf (slot-value instance 'stream)
	(usocket:socket-stream (slot-value instance 'socket)))  
  (send-init-session-data instance)
  (let ((controller (slot-value instance 'controller)))
    (setf (slot-value controller 'connection) instance)))

(defmethod send-start-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-start-message
     (slot-value instance 'stream))))

(defmethod send-stop-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-stop-message
     (slot-value instance 'stream))))

(defmethod send-frames-message ((instance connection) sample-data)
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-frames-message
     (slot-value instance 'stream)
     :sample-data sample-data)))

(defmethod send-init-message ((instance connection) &key sample-rate channel-count buffer-size)
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-init-message
     (slot-value instance 'stream)
     :sample-rate sample-rate
     :channel-count channel-count
     :buffer-size buffer-size)))
  
(defmethod send-close-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-close-message
     (slot-value instance 'stream))))

(defmethod send-init-session-data ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (let ((stream (slot-value instance 'stream)))
      (let ((data (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
	(setf (aref data 0) 1)
	(setf (aref data 1) 1)
	(write-sequence data stream)
	(force-output stream)))))

(defmethod start-message-loop ((instance connection))
  (let ((stream (slot-value instance 'stream))
	(controller (slot-value instance 'controller)))
    (handler-case
	(progn
	  (send-init-message
	   instance
	   :sample-rate (get-sample-rate controller)
	   :channel-count (get-channel-count controller)
	   :buffer-size (get-buffer-size controller))
	  (expect-ack stream)
	  (notify-connection-established controller)
	  (send-start-message instance)
	  (loop
	    (handle-message
	     instance
	     (cl-java-sound-client-message:read-message stream))))
      (condition (c)
	(format t "~%Catched error: ~a" c)))
    (format t "~%Closing socket")
    (usocket:socket-close (slot-value instance 'socket))
    (setf (slot-value instance 'socket) nil)
    (setf (slot-value instance 'stream) nil)
    (notify-connection-closed controller))
  nil)

(defmethod handle-message ((instance connection) message)
  (cond
    ((cl-java-sound-client-message:get-frames-message-p message)
     (notify-frames-requested-impl
      (slot-value instance 'controller)
      (getf message :frame-count)))
    (t
     (error 'simple-error
	    :format-control "Dont know how to handle message: ~a"
	    :format-arguments (list message)))))

;;
;; Controller Impl
;;

(defmethod start ((instance controller))
  (send-start-message (slot-value instance 'connection)))

(defmethod stop ((instance controller))
  (send-stop-message (slot-value instance 'connection)))

(defmethod close-connection ((instance controller))
  (send-close-message (slot-value instance 'connection)))

(defmethod frames ((instance controller))
  (let* ((frames-builder (make-frames-builder))
	 (channel-count (get-channel-count instance))
	 (sample-buffer (make-array channel-count)))
    (dotimes (i (slot-value instance 'requested-frame-count))
      (if (not (render-frame instance sample-buffer))
	  (return)
	  (dotimes (i channel-count)
	    (write-sample frames-builder (elt sample-buffer i)))))
    (send-frames-message
     (get-controller-connection instance)
     (get-samples frames-builder))))

(defmethod notify-connection-closed ((instance controller)))

(defmethod notify-connection-established ((instance controller)))
  
(defmethod run ((instance controller))
  (start-message-loop (get-controller-connection instance)))

(defmethod notify-frames-requested-impl ((instance controller) frame-count)
  (setf (slot-value instance 'requested-frame-count) frame-count)
  (notify-frames-requested instance))

  
