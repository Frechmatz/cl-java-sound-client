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

(defun ack-message-p (message)
  (= +MESSAGE-TYPE-ACK+ (getf message :message-type)))

(defun nak-message-p (message)
  (= +MESSAGE-TYPE-NAK+ (getf message :message-type)))

(defun frames-message-p (message)
  (= +MESSAGE-TYPE-FRAMES+ (getf message :message-type)))

(defun get-frames-message-p (message)
  (= +MESSAGE-TYPE-GET-FRAMES+ (getf message :message-type)))

(defun init-message-p (message)
  (= +MESSAGE-TYPE-INIT+ (getf message :message-type)))

(defun stop-message-p (message)
  (= +MESSAGE-TYPE-STOP+ (getf message :message-type)))

(defun start-message-p (message)
  (= +MESSAGE-TYPE-START+ (getf message :message-type)))

(defun close-message-p (message)
  (= +MESSAGE-TYPE-CLOSE+ (getf message :message-type)))

(defun ackinit-message-p (message)
  (= +MESSAGE-TYPE-ACKINIT+ (getf message :message-type)))

;;
;;
;;

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

(defun write-marker (stream marker)
  (dotimes (i (length marker))
    (write-byte (elt marker i) stream)))

(defun read-marker (stream marker)
  (dotimes (i (length marker))
    (let ((b (read-byte stream)))
      (if (not (eql b (elt marker i)))
	  (error "Invalid Start/End-Of-Message Marker")))))

;;
;;
;;

(defun read-message (stream)
  (read-marker stream *START-OF-MESSAGE-MARKER*)
  (let ((message (list :message-type (read-message-type stream))))
    (log-trace "Read message type: ~a" (getf message :message-type))
    (if (ackinit-message-p message)
	(setf (getf message :buffer-size-frames) (read-buffer-size-frames stream)))
    ;; Let unsupported message payloads fail here
    (read-marker stream *END-OF-MESSAGE-MARKER*)
    (log-debug "Inbound: ~a" message)
    message))

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
  (log-debug
   "Outbound: InitMessage{sample-rate=~a, sample-width=~a channel-count=~a, buffer-size-frames=~a, omit-audio-output=~a}"
   sample-rate sample-width channel-count buffer-size-frames omit-audio-output))

(defun write-start-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-START+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-debug "Outbound: StartMessage{}"))
  
(defun write-stop-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-STOP+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-debug "Outbound: StopMessage{}"))

(defun write-close-message (stream)
  (write-marker stream *START-OF-MESSAGE-MARKER*)
  (write-message-type stream +MESSAGE-TYPE-CLOSE+)
  (write-marker stream *END-OF-MESSAGE-MARKER*)
  (force-output stream)
  (log-debug "Outbound: CloseMessage{}"))

(defun write-frames-message-16bit-signed-big-endian
    (stream
     &key
       sample-width
       sample-count
       samples)
  "samples: Float-Array holding samples to be written.
   sample-count: Number of samples to be read from samples array.
   sample-width: Sample width in bytes, e.g. 2"
  (let ((byte-count (* sample-width sample-count)) (sample 0))
    (write-marker stream *START-OF-MESSAGE-MARKER*)
    (write-message-type stream +MESSAGE-TYPE-FRAMES+)
    (write-sample-data-length stream byte-count)
    (dotimes (i sample-count)
      (setf sample (round (* 32768 (elt samples i))))
      (if (< 32767 sample)
	  (setf sample 32767)
	  (if (< sample -32768)
	      (setf sample -32768)))
      (when (< sample 0) (incf sample 65536))
      (write-byte (ldb (byte 8 8) sample) stream)
      (write-byte (ldb (byte 8 0) sample) stream))
    (write-marker stream *END-OF-MESSAGE-MARKER*)
    (force-output stream)
    (log-debug "Outbound: FramesMessage{sample-data-length=~a}" byte-count)))

