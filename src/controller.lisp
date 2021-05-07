(in-package :cl-java-sound-client)

(defclass controller ()
  ((connection :initform nil)
   (channel-count :initarg :channel-count)
   (sample-width :initarg :sample-width :initform :16Bit)
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

(defgeneric render-frames (controller frame-count sample-buffer)
  (:documentation
   "Render frames. Must be implemented by a controller.
    sample-buffer: Transfer object for the rendered samples. 
    An array of length (frame-count * channel-count). 
    A sample is represented by a float -1.0 >= sample <= +1.0
    Returns number of rendered frames."))

(defgeneric notify-connection-closed (controller)
  (:documentation
   "Is called when the connection to the server has been closed."))

(defgeneric notify-connection-established (controller)
  (:documentation
   "Is called when the connection to the server has been established
    and the server is ready to receive audio data."))

(defgeneric connect (controller &key host port buffer-size-frames &allow-other-keys)
  (:documentation
   "Connect with server"))

(defgeneric run (controller)
  (:documentation
   "Starts the event processing loop. Returns when connection has been closed."))

(defun get-channel-count (controller)
  (slot-value controller 'channel-count))

(defun get-sample-width (controller)
  (slot-value controller 'sample-width))

(defun get-sample-rate (controller)
  (slot-value controller 'sample-rate))

(defun get-connection (controller)
  (slot-value controller 'connection))

