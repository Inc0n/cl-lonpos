
(asdf:defsystem lonpos
	:version "0.0.1"
	:license "nil"
	:author "Inc0n <o28c14 AT GMAIL>"
	:description "Lonpos board game"
	:depends-on (#:array-operations #:cl-clojure)
	:components ((:file "lonpos2d")
				 ;; (:file "package")
				 ))
