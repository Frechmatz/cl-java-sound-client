(defsystem :cl-java-sound-client
  :serial t
  :version "0.0.1"
  :depends-on (:usocket :lisp-binary :flexi-streams :bordeaux-threads)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "message")
			     (:file "client")))))

(defsystem :cl-java-sound-client/examples
  :serial t
  :version "0.0.1"
  :depends-on (:cl-java-sound-client)
  :components ((:module "example"
		:serial t
		:components ((:file "packages")
			     (:file "example-1")))))
