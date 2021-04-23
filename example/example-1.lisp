;;
;; Example: Play Sine Wave
;;

(in-package :cl-java-sound-client-example-1)

(defun make-phase-generator (sample-rate)
  (let ((phi 0.0))
    (lambda (frequency)
      (declare (type single-float frequency))
      (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
      phi)))

(defclass example-controller (controller)
  ((cur-frame-count :initform 0)
   (max-frame-count :initform 0)
   (phase-generator :initform nil)))

(defun done-p (controller)
  (<= (slot-value controller 'max-frame-count)
      (slot-value controller 'cur-frame-count)))

(defmethod initialize-instance :after
    ((instance example-controller)
     &key duration-seconds
     &allow-other-keys)
  (setf (slot-value instance 'phase-generator)
	(make-phase-generator (get-sample-rate instance)))
  (setf (slot-value instance 'max-frame-count)
	(* (get-sample-rate instance) duration-seconds)))
  
(defmethod notify-frames-requested ((instance example-controller))
  (if (done-p instance)
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
