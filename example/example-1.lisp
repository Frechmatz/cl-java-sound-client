;;
;; Example
;; Play a 2-Channel 16Bit 44100Hz Sine wave
;;

(in-package :cl-java-sound-client-example-1)

(defclass example-controller (controller)
  ((cur-frame-count :initform 0)
   (duration-seconds :initarg :duration-seconds)
   (phase-generator :initform nil)))

(defmethod run :before ((instance example-controller))
  (flet ((make-phase-generator ()
	   (let ((phi 0.0) (sample-rate (get-sample-rate instance)))
	     (lambda (frequency)
	       (declare (type single-float frequency))
	       (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
	       phi))))
    (setf (slot-value instance 'phase-generator) (make-phase-generator))))

(defmethod notify-frames-requested ((instance example-controller) frame-count frames-builder)
  (if (<= (* (slot-value instance 'duration-seconds)
	     (get-sample-rate instance))
	  (slot-value instance 'cur-frame-count))
      (close-connection instance)
      (progn
	(setf (slot-value instance 'cur-frame-count)
	      (+ (slot-value instance 'cur-frame-count) frame-count))
	(let ((pg (slot-value instance 'phase-generator))
	      (channel-count (get-channel-count instance)))
	  (dotimes (i frame-count)
	    (let ((sample (sin (funcall pg 440.0))))
	      (dotimes (i channel-count)
		(write-sample frames-builder sample))))
	  (frames instance frames-builder)))))

(defun main ()
  (let ((my-controller
	  (make-instance
	   'example-controller
	   :duration-seconds 5
	   :sample-rate 44100
	   :sample-width 2
	   :channel-count 2)))
    (make-instance 'connection :controller my-controller :port 9000 :host "localhost")
    (run my-controller)))

;;(main)
