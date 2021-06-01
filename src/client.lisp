(in-package :cl-java-sound-client)

;;
;; Connection
;;

(defun sample-width-to-sample-width-bytes (sample-width)
  (cond
    ((eq :8Bit sample-width) 1)
    ((eq :16Bit sample-width) 2)
    ((eq :24Bit sample-width) 3)
    (t
     (error 'simple-error
	    :format-control "Unsupported sample-width: ~a (must be one of :8Bit :16Bit :24Bit)"
	    :format-arguments (list sample-width)))))
    

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
  (log-info "Connected with server \"~a\" port ~a"
	    (slot-value instance 'host)
	    (slot-value instance 'port)))

(defmethod send-start-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-start-message
     (slot-value instance 'stream))))

(defmethod send-stop-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-stop-message
     (slot-value instance 'stream))))

(defun dispatch-send-frames (stream sample-width sample-buffer sample-count)
  "Dispatch frames writing to the appropriate message writer function"
  (cond
    ((eq :16Bit sample-width)
     (cl-java-sound-client-message:write-frames-message-float-to-16bit-signed
      stream
      :samples sample-buffer
      :sample-count sample-count))
    (t
     (error 'simple-error
	    :format-control "Unsupported sample-width: ~a"
	    :format-arguments (list sample-width)))))

(defmethod send-frames-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (let ((controller (get-controller instance)))
      (let ((rendered-frame-count
	 (render-frames
	  controller
	  (get-buffer-size-frames instance)
	  (get-sample-buffer instance))))
	(dispatch-send-frames
	 (slot-value instance 'stream)
	 (get-sample-width controller)
	 (get-sample-buffer instance)
	 (* (get-channel-count controller) rendered-frame-count))))))
  
(defmethod send-close-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-close-message
     (slot-value instance 'stream))))

(defmethod send-init-session-data ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (let ((stream (slot-value instance 'stream)))
      (let ((data (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
	;; Version 1.0.0
	(setf (aref data 0) 1)
	(setf (aref data 1) 0)
	(setf (aref data 2) 0)
	(write-sequence data stream)
	(force-output stream))
      (let ((b (read-byte stream)))
	(if (not (eql 0 b))
	    (error 'simple-error
		   :format-control "Session initialization data rejected by server: ~a"
		   :format-arguments (list b)))))))

(defmethod send-init-message ((instance connection))
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (let ((controller (slot-value instance 'controller)))
      (cl-java-sound-client-message:write-init-message
       (slot-value instance 'stream)
       :sample-rate (get-sample-rate controller)
       :sample-width (sample-width-to-sample-width-bytes (get-sample-width controller))
       :channel-count (get-channel-count controller)
       :buffer-size-frames (get-buffer-size-frames instance)
       :omit-audio-output (get-omit-audio-output instance)))))

(defmethod init-audio ((instance connection))
  (let ((stream (slot-value instance 'stream))
	(controller (slot-value instance 'controller)))
    (send-init-message instance)
    (let ((message (cl-java-sound-client-message:read-message stream)))
      (if (not (cl-java-sound-client-message:ackinit-message-p message))
	  (error 'simple-error
		 :format-control "Expected AckInitMessage but got ~a"
		 :format-arguments (list message)))
      ;; Server returns new buffer size frames
      (setf (slot-value instance 'buffer-size-frames)
	    (getf message :buffer-size-frames))
      (setf (slot-value instance 'sample-buffer)
	    (make-array (* (get-buffer-size-frames instance)
			   (get-channel-count controller)))))
    (log-info "Server has accepted InitMessage")))
  
(defmethod start-message-loop ((instance connection))
  (let ((stream (slot-value instance 'stream))
	(controller (slot-value instance 'controller)))
    (handler-case
	(progn
	  (init-audio instance)
	  (notify-connection-established controller)
	  (send-start-message instance)
	  (loop
	    (let ((message (cl-java-sound-client-message:read-message stream)))
	      (cond
		((cl-java-sound-client-message:get-frames-message-p message)
		 (notify-frames-requested (slot-value instance 'controller)))
		(t
		 (error 'simple-error
			:format-control "Dont know how to handle message: ~a"
			:format-arguments (list message)))))))
      (end-of-file (c)
	(declare (ignore c))
	(log-info "Connection closed"))
      (condition (c)
	(format t "~%Catched error: ~a" c)))
    (log-info "Closing socket")
    (usocket:socket-close (slot-value instance 'socket))
    (setf (slot-value instance 'socket) nil)
    (setf (slot-value instance 'stream) nil)
    (notify-connection-closed controller))
  nil)

;;
;; Controller
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

(defmethod connect ((instance controller) &key host port buffer-size-frames
		    (omit-audio-output nil) &allow-other-keys)
  (let ((connection
	  (make-instance
	   'connection
	   :port port
	   :host host
	   :buffer-size-frames buffer-size-frames
	   :omit-audio-output omit-audio-output)))
    (setf (slot-value instance 'connection) connection)
    (setf (slot-value connection 'controller) instance)))
