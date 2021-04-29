(in-package :cl-java-sound-client-logger)

(defparameter *log-level* :trace)

(defun write-log-entry(level format-control format-arguments)
  (if *log-level*
      (apply #'format t (concatenate 'string "~% ~a " format-control) level format-arguments)))

(defun log-info (format-control &rest format-arguments)
  (write-log-entry :info format-control format-arguments))

(defun log-trace (format-control &rest format-arguments)
  (write-log-entry :trace format-control format-arguments))

(defun log-error (format-control &rest format-arguments)
  (write-log-entry :error format-control format-arguments))

