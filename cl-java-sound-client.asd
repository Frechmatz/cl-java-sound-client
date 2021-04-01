(defsystem :cl-java-sound-client
  :serial t
  :version "0.0.1"
  :depends-on (:usocket :usocket-server :lisp-binary)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "message")
			     (:file "client")))))
