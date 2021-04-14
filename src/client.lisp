(in-package :cl-java-sound-client)

(defun make-phase-generator (sample-rate)
  (let ((phi 0.0))
    (lambda (frequency)
      (declare (type single-float frequency))
      (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
      phi)))


(defun expect-ack (stream)
  (let ((message (cl-java-sound-client-message:read-message stream)))
    (if (not (cl-java-sound-client-message:ack-message-p message))
	(error 'simple-error
	       :format-control "Expected AckMessage but got ~a"
	       :format-arguments (list message)))))

(defun main()
  (let* ((channel-count 2)
	 (sample-rate 44100)
;;	 (sample-width 2)
	 (duration-seconds 5)
	 (max-frame-count (* duration-seconds sample-rate))
	 (phase-generator (make-phase-generator sample-rate))
	 (socket (usocket:socket-connect "localhost" 9000 :element-type '(unsigned-byte 8))))
    (format t "~%Connected")
    (let ((stream (usocket:socket-stream socket)) (cur-frame-count 0))
      (labels ((write-samples (frame-count)
		 (let ((os (flexi-streams:make-in-memory-output-stream
			    :element-type '(unsigned-byte 8))))
		   (dotimes (i frame-count)
		     (let ((sample (sin (funcall phase-generator 440.0))))
		       (dotimes (i channel-count)
			 (cl-java-sound-client-message:write-sample os sample))))
		   (flexi-streams:get-output-stream-sequence os)))
	     (process-message ()
	       (let ((message (cl-java-sound-client-message:read-message stream)))
		 (cond
		   ((cl-java-sound-client-message:get-frames-message-p message)
		    (if (>= cur-frame-count max-frame-count)
			(progn
			  (cl-java-sound-client-message:write-close-message stream)
			  (error 'simple-error
				 :format-control "Max frame count of ~a reached"
				 :format-arguments (list max-frame-count))))
		    (let ((frame-count (getf message :frame-count)))
		      (setf cur-frame-count (+ cur-frame-count frame-count))
		      (let ((bytes (write-samples frame-count)))
			(cl-java-sound-client-message:write-frames-message
			 stream :sample-data bytes)))
		    (force-output stream))
		   (t
		    (error 'simple-error
			   :format-control "Unsupported message sent from server ~a"
			   :format-arguments (list message)))))))
	(handler-case
	    (progn
	      ;; Init Speaker
	      (cl-java-sound-client-message:write-init-message
	       stream
	       :sample-rate sample-rate
	       :channel-count channel-count
	       :buffer-size 0)
	      (expect-ack stream)
	      (cl-java-sound-client-message:write-start-message stream)
	      (loop
		(process-message)))
	  (condition (c)
	    (format t "~%Closing socket (~a)" c)
	    (usocket:socket-close socket)))
	(format t "~%DONE")))))

;;(main)

#|

(defclass client ()
  ((initialized :initform nil)
   (port :initarg :port)
   (sample-rate :initarg :sample-rate)
   (channel-count :initarg :channel-count)
   (buffer-size :initarg :buffer-size :initform 0)
   (host :initarg :host :initform "localhost")
   (socket :initform nil)
   (stream :initform nil)
   (marked-for-close :initform nil)
   (closed :initform nil)
   ))

(defgeneric start (client))
(defgeneric write-samples (client frame-count))
(defgeneric close-client (client))

(defun assert-initialized (client)
  (if (not (slot-value client 'initialized))
      (error 'simple-error
	     :format-control "Client not initialized"
	     :format-arguments (list))))

(defun assert-not-closed (client)
  (if (slot-value client 'closed)
      (error 'simple-error
	     :format-control "Client already closed"
	     :format-arguments (list))))

(defmethod close-client ((client-instance client))
  (assert-not-closed client-instance)
  (setf (slot-value client-instance 'marked-for-close) t)
  nil)

  
(defun init (client)
  (assert-not-closed client-instance)
  (setf (slot-value client-instance 'socket)
	(usocket:socket-connect
	 (slot-value client-instance 'host)
	 (slot-value client-instance 'port)
	 :element-type '(unsigned-byte 8)))
  (setf (slot-value client-instance 'stream)
	(usocket:socket-stream (slot-value client-instance 'socket)))
  ;; Init Speaker
  (cl-java-sound-client-message:write-init-message
   (slot-value client-instance 'stream)
   :sample-rate (slot-value client-instance 'sample-rate)
   :channel-count (slot-value client-instance 'channel-count)
   :buffer-size (slot-value client-instance 'buffer-size))
  (expect-ack (slot-value client-instance 'stream))
  (setf (slot-value client-instance 'initialized) t)
  nil)


(defun close-connection (client)
  )

(defun process-message (client message)
  )


(defmethod start ((client-instance client))
  (assert-not-closed client-instance)
  (cl-java-sound-client-message:write-start-message (slot-value client-instance 'stream))
  ;;
  ;; Message Loop
  ;;
  (handler-case
      (loop
	(let ((message (cl-java-sound-client-message:read-message
       			(slot-value client-instance 'stream))))
	  (if (not message)
	      (return))
	  (process-message client-instance message)))
    (condition (c)
      (format t "~%Closing socket (~a)" c)
      (usocket:socket-close (slot-value client-instance 'socket))
      )
    )
      
    nil)

|#











