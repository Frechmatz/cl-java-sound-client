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

(defgeneric start-event-loop
    (connection
     &key
       channel-count
       sample-rate
       sample-width
       buffer-size
     &allow-other-keys))

(defgeneric handle-message (connection message))
(defgeneric send-start-message (connection))
(defgeneric send-stop-message (connection))
(defgeneric send-frames-message (connection frames-builder))
(defgeneric send-init-message (connection &key sample-rate channel-count buffer-size))
(defgeneric close-connection (connection))
(defgeneric send-init-session-data (connection))


;;
;; Controller
;;

(defclass controller ()
  ((connection :initarg nil)))

(defgeneric stop (controller)
  (:documentation
   "Pause playback. Sends a Stop message to the server."))

(defgeneric start (controller)
  (:documentation
   "Continue playback. Sends a Start message to the server."))

(defgeneric close-connection (controller)
  (:documentation
   "Sends a close message to the server in order to indicate that the connection shall be closed."))

(defgeneric frames (controller frames-builder)
  (:documentation
   "Sends audio data to the server."))

(defgeneric notify-frames-requested (controller frame-count frames-builder)
  (:documentation
   "Is called when controller is supposed to send audio data to the server.
    frame-count represents a recommended frame count to be sent back to the server."))

(defgeneric notify-connection-closed (controller)
  (:documentation
   "Is called when the connection to the server has been closed."))

(defgeneric notify-connection-established (controller)
  (:documentation
   "Is called when the connection to the server has been established
    and the server is ready to receive audio data."))

(defgeneric run (controller)
  (:documentation
   "Starts the event processing loop. Implementation must call 
    connection::start-event-loop. Returns when connection has been closed."))

(defun get-controller-connection (controller)
  "Returns the connection belonging to the given controller."
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

(defmethod send-frames-message ((instance connection) frames-builder)
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-frames-message
     (slot-value instance 'stream)
     :sample-data (get-samples frames-builder))))

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

(defmethod start-event-loop
    ((instance connection)
     &key
       channel-count
       sample-rate
       sample-width
       (buffer-size 0)
     &allow-other-keys)
  (declare (ignore sample-width))
  (let ((stream (slot-value instance 'stream)))
    (handler-case
	(progn
	  (send-init-message
	   instance
	   :sample-rate sample-rate
	   :channel-count channel-count
	   :buffer-size buffer-size)
	  (expect-ack stream)
	  (notify-connection-established (slot-value instance 'controller))
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
    (notify-connection-closed (slot-value instance 'controller)))
  nil)

(defmethod handle-message ((instance connection) message)
  (cond
    ((cl-java-sound-client-message:get-frames-message-p message)
     (notify-frames-requested
      (slot-value instance 'controller)
      (getf message :frame-count)
      (make-frames-builder)))
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

(defmethod frames ((instance controller) frames-builder)
  (send-frames-message (slot-value instance 'connection) frames-builder))

(defmethod notify-connection-closed ((instance controller)))

(defmethod notify-connection-established ((instance controller)))
  
(defmethod notify-frames-requested ((instance controller) frame-count frames-builder)
  (declare (ignore instance frame-count frames-builder))
  (error "Controller must implement method notify-frames-requested"))

(defmethod run ((instance controller))
  (declare (ignore instance))
  (error "Controller must implement method run"))
  
