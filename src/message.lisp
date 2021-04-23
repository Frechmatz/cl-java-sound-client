(in-package :cl-java-sound-client-message)

(defconstant +MESSAGE-TYPE-ACK+ 1)
(defconstant +MESSAGE-TYPE-NAK+ 2)
(defconstant +MESSAGE-TYPE-FRAMES+ 3)
(defconstant +MESSAGE-TYPE-GET-FRAMES+ 4)
(defconstant +MESSAGE-TYPE-INIT+ 5)
(defconstant +MESSAGE-TYPE-STOP+ 6)
(defconstant +MESSAGE-TYPE-START+ 7)
(defconstant +MESSAGE-TYPE-CLOSE+ 8)

;;
;;
;;

(defun ack-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-ACK+))

(defun nak-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-NAK+))

(defun frames-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-FRAMES+))

(defun get-frames-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-GET-FRAMES+))

(defun init-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-INIT+))

(defun stop-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-STOP+))

(defun start-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-START+))

(defun close-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-CLOSE+))

;;
;;
;;

(defun ack-message-p (message-type)
  (ack-message-type-p (getf message-type :message-type)))

(defun nak-message-p (message-type)
  (nak-message-type-p (getf message-type :message-type)))

(defun frames-message-p (message-type)
  (frames-message-type-p (getf message-type :message-type)))

(defun get-frames-message-p (message-type)
  (get-frames-message-type-p (getf message-type :message-type)))

(defun init-message-p (message-type)
  (init-message-type-p (getf message-type :message-type)))

(defun stop-message-p (message-type)
  (stop-message-type-p (getf message-type :message-type)))

(defun start-message-p (message-type)
  (start-message-type-p (getf message-type :message-type)))

(defun close-message-p (message-type)
  (close-message-type-p (getf message-type :message-type)))

;;
;;
;;

(defmacro clip-value (value)
  `(cond
    ((> ,value 1.0)
     1.0)
    ((< ,value -1.0)
     -1.0)
    (t ,value)))

(defun value-to-16bit-signed (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 32768 value)))
  (cond
    ((< 32767 value)
     32767)
    ((< value -32768)
     -32768)
    (t
     value)))

;; 16bit signed
(defun write-sample (stream sample)
  "Sample: -1.0...1.0"
  ;;  (declare (type single-float sample))
  (let ((s16 (value-to-16bit-signed sample)))
    (lisp-binary:write-integer s16 2 stream :byte-order :big-endian)))

(defun write-channel-count (stream channel-count)
  ;; signed short 2 bytes DataInputStream readShort
  (lisp-binary:write-integer channel-count 2 stream :byte-order :big-endian :signed t))

(defun read-frame-count (stream)
  ;; signed int 4 bytes DataInputStream readInt
  (lisp-binary:read-integer 4 stream :byte-order :big-endian :signed t))

(defun write-sample-data-length (stream sample-data-length)
  ;; signed int 4 bytes DataInputStream readInt
  (lisp-binary:write-integer sample-data-length 4 stream :byte-order :big-endian :signed t))

(defun write-sample-rate (stream sample-rate)
  ;; signed int 4 bytes DataInputStream readInt
  (lisp-binary:write-integer sample-rate 4 stream :byte-order :big-endian :signed t))

(defun write-message-type (stream command-id)
  ;; signed short 2 bytes DataInputStream readShort
  (lisp-binary:write-integer command-id 2 stream :byte-order :big-endian :signed t))

(defun read-message-type (stream)
  ;; signed short 2 bytes DataInputStream readShort
  (lisp-binary:read-integer 2 stream :byte-order :big-endian :signed t))

(defun write-buffer-size (stream buffer-size)
  ;; signed int 4 bytes DataInputStream readInt
  (lisp-binary:write-integer buffer-size 4 stream :byte-order :big-endian :signed t))

;;
;; Start/End-Of-Message Marker
;;

(defparameter *START-OF-MESSAGE-MARKER* (make-array
				      4
				      :element-type '(unsigned-byte 8)
				      :initial-contents '(1 2 3 4)))

(defparameter *END-OF-MESSAGE-MARKER* (make-array
				      4
				      :element-type '(unsigned-byte 8)
				      :initial-contents '(4 3 2 1)))


(defun write-marker (stream arr)
  (dotimes (i (length arr))
    (write-byte (elt arr i) stream)))

(defun read-marker (stream arr)
  (dotimes (i (length arr))
    (let ((b (read-byte stream)))
      (if (not (eql b (elt arr i)))
	  (error "Invalid Start/End-Of-Message Marker")))))

;;
;;
;;

(defun read-message (stream)
  (read-marker stream *START-OF-MESSAGE-MARKER*)
  (let ((message-type (read-message-type stream)))
    (let ((message
	    (cond
	      ((ack-message-type-p message-type)
	       (list :message-type message-type))
	      ((nak-message-type-p message-type)
	       (list :message-type message-type))
	      ((get-frames-message-type-p message-type)
	       (list
		:message-type message-type
		:frame-count (read-frame-count stream)))
	      ((frames-message-type-p message-type)
	       (error 'simple-error 
		      :format-control "Frames message not supported ~a"
		      :format-arguments (list message-type)))
	      ((init-message-type-p message-type)
	       (error 'simple-error
		      :format-control "Frames message not supported ~a"
		      :format-arguments (list message-type)))
	      ((stop-message-type-p message-type)
	       (error 'simple-error
		      :format-control "Stop message not supported ~a"
		      :format-arguments (list message-type)))
	      ((start-message-type-p message-type)
	       (error 'simple-error
		      :format-control "Start message not supported ~a"
		      :format-arguments (list message-type)))
	      ((close-message-type-p message-type)
	       (error 'simple-error
		      :format-control "Close message not supported ~a"
		      :format-arguments (list message-type)))
	      (t
	       (error 'simple-error
		      :format-control "Unsupported message type ~a"
		      :format-arguments (list message-type))))))
      (read-marker stream *END-OF-MESSAGE-MARKER*)
      (format t "~%Inbound: ~a" message)
      message)))

;;
;;
;;

(defun write-init-message (stream &key sample-rate channel-count buffer-size)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-INIT+)
  (write-sample-rate stream sample-rate)
  (write-channel-count stream channel-count)
  (write-buffer-size stream buffer-size)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (format t "~%Outbound: InitMessage{sample-rate=~a, channel-count=~a, buffer-size=~a}"
	  sample-rate channel-count buffer-size))

(defun write-start-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-START+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (format t "~%Outbound: StartMessage{}"))
  
(defun write-stop-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-STOP+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (format t "~%Outbound: StopMessage{}"))

(defun write-close-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-CLOSE+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (format t "~%Outbound: CloseMessage{}"))

;;
;; Frames-Builder
;;

(defun make-frames-builder ()
  (let ((os (flexi-streams:make-in-memory-output-stream
	     :element-type '(unsigned-byte 8))))
    (list
     (lambda (sample)
       (write-sample os sample))
     (lambda()
       (flexi-streams:get-output-stream-sequence os)))))

(defun builder-get-samples (frames-builder)
  (funcall (second frames-builder)))

(defun builder-write-sample (frames-builder sample)
  (funcall (first frames-builder) sample))

;; TODO Current implementation is hard coded to request one
;; frame per render-frames request.
(defun write-frames-message (stream &key sample-width channel-count frame-count frames-renderer)
  (declare (ignore sample-width))
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-FRAMES+)
  (let ((frames-builder (make-frames-builder))
	 (sample-buffer (make-array channel-count)))
    (dotimes (i frame-count)
      (let ((rendered-frame-count
	      (funcall frames-renderer 1 sample-buffer)))
	(if (= 0 rendered-frame-count)
	    (return)
	    (dotimes (i channel-count)
	      (builder-write-sample frames-builder (elt sample-buffer i))))))
    (let ((sample-data (builder-get-samples frames-builder)))
      (write-sample-data-length stream (length sample-data))
      (write-sequence sample-data stream)
      (write-marker stream *END-OF-MESSAGE-MARKER*)
      (force-output stream)
      (format t "~%Outbound: FramesMessageV2{sample-data-length=~a}" (length sample-data)))))

