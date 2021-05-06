(in-package :cl-java-sound-client-message)

(defconstant +MESSAGE-TYPE-ACK+ 1)
(defconstant +MESSAGE-TYPE-NAK+ 2)
(defconstant +MESSAGE-TYPE-FRAMES+ 3)
(defconstant +MESSAGE-TYPE-GET-FRAMES+ 4)
(defconstant +MESSAGE-TYPE-INIT+ 5)
(defconstant +MESSAGE-TYPE-STOP+ 6)
(defconstant +MESSAGE-TYPE-START+ 7)
(defconstant +MESSAGE-TYPE-CLOSE+ 8)
(defconstant +MESSAGE-TYPE-ACKINIT+ 9)

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

(defun ackinit-message-type-p (message-type)
  (= message-type +MESSAGE-TYPE-ACKINIT+))

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

(defun ackinit-message-p (message-type)
  (ackinit-message-type-p (getf message-type :message-type)))

;;
;;
;;

(defun value-to-16bit-signed (value)
  "value: -1.0 ... 1.0"
  (setf value (round (* 32768 value)))
  (cond
    ((< 32767 value)
     32767)
    ((< value -32768)
     -32768)
    (t
     value)))

;;
;; Write a sample in 16bit signed big-endian format
;;
;; TODO Optimize / Re-think implementation of value-to-16bit-signed
;; because it consumes a lot of CPU
;;
(defun write-sample-16bit-signed-big-endian (stream sample)
  "Sample: -1.0...1.0"
  (let ((s16 (value-to-16bit-signed sample)))
    (when (< s16 0) (incf s16 (expt 2 (* 2 8))))
    (write-byte (ldb (byte 8 8) s16) stream)
    (write-byte (ldb (byte 8 0) s16) stream)))

(defun write-channel-count (stream channel-count)
  ;; signed short 2 bytes DataInputStream readShort
  (lisp-binary:write-integer channel-count 2 stream :byte-order :big-endian :signed t))

(defun read-buffer-size-frames (stream)
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

(defun write-sample-width (stream sample-width)
  ;; signed short 2 bytes DataInputStream readShort
  (lisp-binary:write-integer sample-width 2 stream :byte-order :big-endian :signed t))

(defun read-message-type (stream)
  ;; signed short 2 bytes DataInputStream readShort
  (lisp-binary:read-integer 2 stream :byte-order :big-endian :signed t))

(defun write-buffer-size-frames (stream buffer-size)
  ;; signed int 4 bytes DataInputStream readInt
  (lisp-binary:write-integer buffer-size 4 stream :byte-order :big-endian :signed t))

(defun write-omit-audio-output (stream omit-audio-output)
  ;; signed short 2 bytes DataInputStream readShort
  (let ((v (if omit-audio-output 1 0)))
    (lisp-binary:write-integer v 2 stream :byte-order :big-endian :signed t)))

;;
;; Start/End-Of-Message Marker
;;

(defparameter *START-OF-MESSAGE-MARKER*
  (make-array
   4
   :element-type '(unsigned-byte 8)
   :initial-contents '(1 2 3 4)))

(defparameter *END-OF-MESSAGE-MARKER*
  (make-array
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
		:message-type message-type))
	      ((frames-message-type-p message-type)
	       (error 'simple-error 
		      :format-control "Frames message not supported ~a"
		      :format-arguments (list message-type)))
	      ((ackinit-message-type-p message-type)
	       (list
		:message-type message-type
		:buffer-size-frames (read-buffer-size-frames stream)))
	      (t
	       (error 'simple-error
		      :format-control "Unsupported message type ~a"
		      :format-arguments (list message-type))))))
      (read-marker stream *END-OF-MESSAGE-MARKER*)
      (log-trace "Inbound: ~a" message)
      message)))

;;
;;
;;

(defun write-init-message (stream &key sample-rate sample-width channel-count buffer-size-frames
				    omit-audio-output)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-INIT+)
  (write-sample-rate stream sample-rate)
  (write-sample-width stream sample-width)
  (write-channel-count stream channel-count)
  (write-buffer-size-frames stream buffer-size-frames)
  (write-omit-audio-output stream omit-audio-output)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-trace
   "Outbound: InitMessage{sample-rate=~a, sample-width=~a channel-count=~a, buffer-size-frames=~a, omit-audio-output=~a}"
   sample-rate sample-width channel-count buffer-size-frames omit-audio-output))

(defun write-start-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-START+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-trace "Outbound: StartMessage{}"))
  
(defun write-stop-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-STOP+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-trace "Outbound: StopMessage{}"))

(defun write-close-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-CLOSE+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-trace "Outbound: CloseMessage{}"))

(defun write-frames-message
    (stream
     &key
       sample-width
       sample-count
       samples)
  "samples: Float-Array holding samples to be written.
   sample-count: Number of samples to be read from samples array.
   sample-width: Sample width in bytes, e.g. 2"
  (let ((byte-count (* sample-width sample-count)))
    (write-marker stream *START-OF-MESSAGE-MARKER*)
    (write-message-type stream +MESSAGE-TYPE-FRAMES+)
    (write-sample-data-length stream byte-count)
    (dotimes (i sample-count)
      (write-sample-16bit-signed-big-endian stream (elt samples i)))
    (write-marker stream *END-OF-MESSAGE-MARKER*)
    (force-output stream)
    (log-trace "Outbound: FramesMessage{sample-data-length=~a}" byte-count)))

