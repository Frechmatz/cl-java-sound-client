(defpackage :cl-java-sound-client
  (:use :cl)
  (:export
   :connection
   :controller
   :get-sample-rate
   :get-channel-count
   :get-sample-width
   :run
   :stop
   :start
   :render-frame
   :close-connection
   :frames
   :notify-frames-requested
   :notify-connection-closed
   :notify-connection-established))

(defpackage :cl-java-sound-client-message
  (:use :cl)
  (:export
   :read-message
   :write-init-message
   :write-start-message
   :write-stop-message
   :write-close-message
   :write-frames-message
   :write-sample
   :ack-message-p
   :nak-message-p
   :frames-message-p
   :get-frames-message-p
   :init-message-p
   :stop-message-p
   :start-message-p
   :close-message-p)
  (:documentation "Internal package. Can change any time."))

