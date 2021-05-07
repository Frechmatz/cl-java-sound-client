(in-package :cl-java-sound-client)

(defclass connection ()
  ((port :initarg :port :initform 9000)
   (host :initarg :host :initform "localhost")
   (controller :initform nil)
   (socket :initform nil)
   (stream :initform nil)
   (send-message-lock :initform (bt:make-lock))
   (buffer-size-frames :initarg :buffer-size-frames :initform 10001)
   (sample-buffer :initform nil)
   (omit-audio-output :initarg :omit-audio-output)))

(defgeneric start-message-loop (connection))
(defgeneric send-start-message (connection))
(defgeneric send-stop-message (connection))
(defgeneric send-frames-message (connection))
(defgeneric send-init-message
    (connection
     &key
       sample-rate
       channel-count
       buffer-size-frames
       omit-audio-output))
(defgeneric send-init-session-data (connection))
(defgeneric init-audio (connection controller))
(defun get-controller (connection)
  (slot-value connection 'controller))
(defun get-sample-buffer (connection)
  (slot-value connection 'sample-buffer))
(defun get-buffer-size-frames (controller)
  (slot-value controller 'buffer-size-frames))
(defun get-omit-audio-output (controller)
  (slot-value controller 'omit-audio-output))

