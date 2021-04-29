(in-package :cl-java-sound-client-profiling)

(defconstant 2PI (coerce (* 2 PI) 'single-float))

(defun make-phase-generator (sample-rate)
    (let ((phi 0.0))
      (lambda (frequency)
	(declare (type single-float frequency))
	(setf phi (rem (+ phi (/ (* 2PI frequency) sample-rate)) (* 2PI)))
	phi)))

(defun make-sample-generator (sample-rate)
  (let ((phase-generator (make-phase-generator sample-rate)))
    (lambda ()
      (sin (funcall phase-generator 440.0)))))

