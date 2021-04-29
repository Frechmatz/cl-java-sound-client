(in-package :cl-java-sound-client-logger)

(defparameter *log-level* :trace)

(defparameter *log-error-p* (lambda() t))
(defparameter *log-trace-p* (lambda() nil))
(defparameter *log-info-p* (lambda() t))
(defparameter *log-debug-p* (lambda() nil))

(defun set-log-level (level)
  (cond
    ((eq level :trace)
     (setf *log-trace-p* (lambda() t))
     (setf *log-debug-p* (lambda() t))
     (setf *log-info-p* (lambda() t))
     (setf *log-error-p* (lambda() t)))
    ((eq level :debug)
     (setf *log-trace-p* (lambda() nil))
     (setf *log-debug-p* (lambda() t))
     (setf *log-info-p* (lambda() t))
     (setf *log-error-p* (lambda() t)))
    ((eq level :info)
     (setf *log-trace-p* (lambda() nil))
     (setf *log-debug-p* (lambda() nil))
     (setf *log-info-p* (lambda() t))
     (setf *log-error-p* (lambda() t)))
    ((eq level :error)
     (setf *log-trace-p* (lambda() nil))
     (setf *log-debug-p* (lambda() nil))
     (setf *log-info-p* (lambda() nil))
     (setf *log-error-p* (lambda() t)))
    (t
     (error 'simple-error
	    :format-control "Unsupported log level: ~a"
	    :format-arguments (list level)))))

(defun write-log-entry(level format-control format-arguments)
  (apply #'format t (concatenate 'string "~% ~a " format-control) level format-arguments))

(defun log-info (format-control &rest format-arguments)
  (if (funcall *log-info-p*)
      (write-log-entry :info format-control format-arguments)))

(defun log-debug (format-control &rest format-arguments)
  (if (funcall *log-debug-p*)
      (write-log-entry :debug format-control format-arguments)))

(defun log-trace (format-control &rest format-arguments)
  (if (funcall *log-trace-p*)
      (write-log-entry :trace format-control format-arguments)))

(defun log-error (format-control &rest format-arguments)
  (if (funcall *log-error-p*)
      (write-log-entry :error format-control format-arguments)))

