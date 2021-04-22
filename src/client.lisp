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
   (controller :initform nil)
   (socket :initform nil)
   (stream :initform nil)
   (send-message-lock :initform (bt:make-lock))
   (requested-frame-count :initform 0)))

(defgeneric start-message-loop (connection))
(defgeneric handle-message (connection message))
(defgeneric send-start-message (connection))
(defgeneric send-stop-message (connection))
(defgeneric send-frames-message (connection))
(defgeneric send-init-message (connection &key sample-rate channel-count buffer-size))
(defgeneric close-connection (connection))
(defgeneric send-init-session-data (connection))
(defun get-controller (connection)
  (slot-value connection 'controller))


;;
;; Controller
;;

(defclass controller ()
  ((connection :initform nil)
   (channel-count :initarg :channel-count)
   (sample-width :initarg :sample-width :initform 2)
   (buffer-size ::initarg :buffer-size :initform 0)
   (sample-rate :initarg :sample-rate :initform 44100)))

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

(defgeneric connect (controller &key host port &allow-other-keys)
  (:documentation
   "Connect with server"))

(defgeneric run (controller)
  (:documentation
   "Starts the event processing loop. Returns when connection has been closed."))

(defun get-channel-count (controller)
  (slot-value controller 'channel-count))

(defun get-sample-width (controller)
  (slot-value controller 'sample-width))

(defun get-buffer-size (controller)
  (slot-value controller 'buffer-size))

(defun get-sample-rate (controller)
  (slot-value controller 'sample-rate))

(defun get-connection (controller)
  (slot-value controller 'connection))

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
  (send-init-session-data instance))

(defmethod send-start-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-start-message
     (slot-value instance 'stream))))

(defmethod send-stop-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-stop-message
     (slot-value instance 'stream))))

(defmethod send-frames-message ((instance connection))
  ;; Deadlock if controller tries to send a message
  ;; in render-frame method
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (let* ((frames-builder (make-frames-builder))
	   (controller (get-controller instance))
	   (channel-count (get-channel-count controller))
	   (sample-buffer (make-array channel-count)))
      (dotimes (i (slot-value instance 'requested-frame-count))
	(if (not (render-frame controller sample-buffer))
	    (return)
	    (dotimes (i channel-count)
	      (write-sample frames-builder (elt sample-buffer i)))))
      (cl-java-sound-client-message:write-frames-message
       (slot-value instance 'stream)
       :sample-data (get-samples frames-builder)))))

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
     (setf (slot-value instance 'requested-frame-count)
	   (getf message :frame-count))
     (notify-frames-requested (slot-value instance 'controller)))
    (t
     (error 'simple-error
	    :format-control "Dont know how to handle message: ~a"
	    :format-arguments (list message)))))

;;
;; Controller Impl
;;

(defmethod start ((instance controller))
  (send-start-message (get-connection instance)))

(defmethod stop ((instance controller))
  (send-stop-message (get-connection instance)))

(defmethod close-connection ((instance controller))
  (send-close-message (get-connection instance)))

(defmethod frames ((instance controller))
  (send-frames-message (get-connection instance)))

(defmethod notify-connection-closed ((instance controller)))

(defmethod notify-connection-established ((instance controller)))
  
(defmethod run ((instance controller))
  (start-message-loop (get-connection instance)))

(defmethod connect ((instance controller) &key host port &allow-other-keys)
  (let ((connection (make-instance 'connection :port port :host host)))
    (setf (slot-value instance 'connection) connection)
    (setf (slot-value connection 'controller) instance)))
  
