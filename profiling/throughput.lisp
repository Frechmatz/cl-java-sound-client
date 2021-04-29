
(in-package :cl-java-sound-client-profiling)

(defclass throughput-controller (controller)
  ((cur-frame-count :initform 0)
   (max-frame-count :initform 0)
   (sample-generator :initform nil)))

(defun done-p (controller)
  (<= (slot-value controller 'max-frame-count)
      (slot-value controller 'cur-frame-count)))

(defmethod initialize-instance :after
    ((instance throughput-controller)
     &key duration-seconds
     &allow-other-keys)
  (setf (slot-value instance 'sample-generator)
	(make-sample-generator (get-sample-rate instance)))
  (setf (slot-value instance 'max-frame-count)
	(* (get-sample-rate instance) duration-seconds)))
  
(defmethod notify-frames-requested ((instance throughput-controller))
  (if (done-p instance)
      (close-connection instance)
      (frames instance)))

(defmethod render-frames ((instance throughput-controller) frame-count sample-buffer)
  (let ((rendered-frame-count 0)
	(channel-count (get-channel-count instance))
	(sample-generator (slot-value instance 'sample-generator)))
    (dotimes (frame-number frame-count)
      (if (done-p instance)
	  (return)
	  (progn
	    (incf rendered-frame-count)
	    (incf (slot-value instance 'cur-frame-count))
	    (let ((sample (funcall sample-generator)))
	      (dotimes (i channel-count)
		(setf (aref sample-buffer (+ i (* frame-number channel-count)))
		      sample))))))
  rendered-frame-count))

(defun main ()
  (cl-java-sound-client-logger:set-log-level :info)
  (time
   (let ((my-controller
	   (make-instance
	    'throughput-controller
	    :duration-seconds 5
	    :sample-rate 44100
	    :sample-width 2
	    :channel-count 2)))
     (connect my-controller
	      :port 9000
	      :host "localhost"
	      :buffer-size-frames 10000
	      :omit-audio-output nil)
     (run my-controller))))

;;(main)
