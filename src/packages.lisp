(defpackage :cl-java-sound-client
  (:use :cl))

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
   :close-message-p
   ))

