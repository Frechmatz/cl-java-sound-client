(in-package :cl-java-sound-client)

;;
;; Connection
;;

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
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (let ((controller (get-controller instance)))
      (let ((rendered-frame-count
	 (render-frames
	  controller
	  (get-buffer-size-frames instance)
	  (get-sample-buffer instance))))
	(cl-java-sound-client-message:write-frames-message
	 (slot-value instance 'stream)
	 :samples (get-sample-buffer instance)
	 :sample-width (get-sample-width controller)
	 :sample-count (* (get-channel-count controller)
			  rendered-frame-count))))))

(defmethod send-init-message ((instance connection)
			      &key
				sample-rate
				channel-count
				buffer-size-frames
				omit-audio-output)
  (bt:with-lock-held ((slot-value instance 'send-message-lock))
    (cl-java-sound-client-message:write-init-message
     (slot-value instance 'stream)
     :sample-rate sample-rate
     :channel-count channel-count
     :buffer-size-frames buffer-size-frames
     :omit-audio-output omit-audio-output)))
  
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

(defmethod init-audio ((instance connection) controller)
  (let ((stream (slot-value instance 'stream)))
    (send-init-message
     instance
     :sample-rate (get-sample-rate controller)
     :channel-count (get-channel-count controller)
     :buffer-size-frames (get-buffer-size-frames instance)
     :omit-audio-output (get-omit-audio-output instance))
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
			   (get-channel-count controller)))))))
  
(defmethod start-message-loop ((instance connection))
  (let ((stream (slot-value instance 'stream))
	(controller (slot-value instance 'controller)))
    (handler-case
	(progn
	  (init-audio instance controller)
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
	(format t "~%Connection closed"))
      (condition (c)
	(format t "~%Catched error: ~a" c)))
    (format t "~%Closing socket")
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
