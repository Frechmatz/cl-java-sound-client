;;
;; Example
;; Play a 2-Channel 16Bit 44100Hz Sine wave
;;

(in-package :cl-java-sound-client-example-1)

(defclass example-controller (controller)
  ((cur-frame-count :initform nil)
   (channel-count :initform 2)
   (sample-width :initform 2)
   (duration-seconds :initarg :duration-seconds)
   (buffer-size :initform 0)
   (sample-rate :initform 44100)
   (phase-generator :initform nil)))

(defmethod run ((instance example-controller))
  (flet ((make-phase-generator ()
	   (let ((phi 0.0) (sample-rate (slot-value instance 'sample-rate)))
	     (lambda (frequency)
	       (declare (type single-float frequency))
	       (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
	       phi))))
    (setf (slot-value instance 'phase-generator) (make-phase-generator))
    (setf (slot-value instance 'cur-frame-count) 0)
    (start-event-loop
     (get-controller-connection instance)
     :channel-count (slot-value instance 'channel-count)
     :sample-rate (slot-value instance 'sample-rate)
     :sample-width (slot-value instance 'sample-width)
     :buffer-size (slot-value instance 'buffer-size))))

(defmethod notify-frames-requested ((instance example-controller) frame-count frames-builder)
  (if (<= (* (slot-value instance 'duration-seconds)
	     (slot-value instance 'sample-rate))
	  (slot-value instance 'cur-frame-count))
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
    (make-instance 'connection :controller my-controller :port 9000 :host "localhost")
    (run my-controller)))

;;(main)
