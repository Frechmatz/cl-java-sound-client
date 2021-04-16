(in-package :cl-java-sound-client)


;;
;; Frames-Builder
;;

(defun make-frames-builder ()
  (let ((os (flexi-streams:make-in-memory-output-stream
	     :element-type '(unsigned-byte 8))))
    (list
     :write-sample
     (lambda (sample)
       (cl-java-sound-client-message:write-sample os sample))
     :get-samples
     (lambda()
       (flexi-streams:get-output-stream-sequence os)))))

(defun write-sample (frames-builder sample)
  "TODO Rework"
  (funcall (second frames-builder) sample))

(defun get-samples (frames-builder)
  "TODO Rework"
  (funcall (fourth frames-builder)))


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

;;
;; Controller
;;

(defclass controller ()
  ((connection :initarg nil)))

(defgeneric stop (controller) (:documentation "Pause playback"))
(defgeneric start (controller) (:documentation "Continue playback"))
(defgeneric close-connection (controller))
(defgeneric frames (controller frames-builder))
(defgeneric notify-frames-requested (controller frame-count frames-builder))
(defgeneric notify-connection-closed (controller))
(defgeneric notify-connection-established (controller))
(defgeneric run (controller) (:documentation "Mandatory"))

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
	    :format-control "Unsupported message sent by server: ~a"
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
  
;;
;; Example
;;

(defun make-phase-generator (sample-rate)
  (let ((phi 0.0))
    (lambda (frequency)
      (declare (type single-float frequency))
      (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
      phi)))

(defclass example-controller (controller)
  ((max-frame-count :initform nil)
   (cur-frame-count :initform 0)
   (channel-count :initform 2)
   (sample-width :initform 2)
   (duration-seconds :initarg :duration-seconds)
   (buffer-size :initform 0)
   (sample-rate :initform 44100)
   (phase-generator :initform nil)))

(defmethod initialize-instance :after ((instance example-controller) &rest rest)
  (declare (ignore rest))
  (setf (slot-value instance 'max-frame-count)
	(* (slot-value instance 'duration-seconds)
	   (slot-value instance 'sample-rate)))
  (setf (slot-value instance 'phase-generator)
	(make-phase-generator (slot-value instance 'sample-rate))))

(defmethod run ((instance example-controller))
  (start-event-loop
   (slot-value instance 'connection)
   :channel-count (slot-value instance 'channel-count)
   :sample-rate (slot-value instance 'sample-rate)
   :sample-width (slot-value instance 'sample-width)
   :buffer-size (slot-value instance 'buffer-size)))

(defmethod notify-frames-requested ((instance example-controller) frame-count frames-builder)
  (if (<= (slot-value instance 'max-frame-count) (slot-value instance 'cur-frame-count))
      (close-connection instance)
      (progn
	(setf (slot-value instance 'cur-frame-count)
	      (+ (slot-value instance 'cur-frame-count) frame-count))
	(let ((pg (slot-value instance 'phase-generator))
	      (channel-count (slot-value instance 'channel-count)))
	  (dotimes (i frame-count)
	    (let ((sample (sin (funcall pg 440.0))))
	      (dotimes (i channel-count)
		(write-sample frames-builder sample))))
	  (frames instance frames-builder)))))

(defun main ()
  (let ((my-controller (make-instance 'example-controller :duration-seconds 5)))
    (make-instance 'connection :controller my-controller)
    (run my-controller))
  "DONE")

;;(main)
