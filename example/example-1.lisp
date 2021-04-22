;;
;; Example
;; Play a 2-Channel 16Bit 44100Hz Sine wave
;;

(in-package :cl-java-sound-client-example-1)

(defclass example-controller (controller)
  ((cur-frame-count :initform 0)
   (duration-seconds :initarg :duration-seconds)
   (phase-generator :initform nil)))

(defmethod initialize-instance :after ((instance example-controller) &rest rest)
  (declare (ignore rest))
  (flet ((make-phase-generator ()
	   (let ((phi 0.0) (sample-rate (get-sample-rate instance)))
	     (lambda (frequency)
	       (declare (type single-float frequency))
	       (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
	       phi))))
    (setf (slot-value instance 'phase-generator) (make-phase-generator))))
  
(defmethod notify-frames-requested ((instance example-controller))
  (if (<= (* (slot-value instance 'duration-seconds)
	     (get-sample-rate instance))
	  (slot-value instance 'cur-frame-count))
      (close-connection instance)
      (frames instance)))

(defmethod render-frame ((instance controller) sample-buffer)
  (incf (slot-value instance 'cur-frame-count))
  (let ((sample (sin (funcall (slot-value instance 'phase-generator) 440.0))))
    (dotimes (i (get-channel-count instance))
      (setf (aref sample-buffer i) sample)))
  t)
  
(defun main ()
  (let ((my-controller
	  (make-instance
	   'example-controller
	   :duration-seconds 5
	   :sample-rate 44100
	   :sample-width 2
	   :channel-count 2)))
    (connect my-controller :port 9000 :host "localhost")
    (run my-controller)))

;;(main)
