(defsystem :myemacs
  :version "0.1.0"
  :author "José A. Navarro Ramón"
  :license "BSD 3-Clause"
  :depends-on ()
  :components ((:module "src"
                :components
		((:file "packages")
		 (:file "main"
		  :depends-on ("packages")))))
  :description ""
  :in-order-to ((test-op (test-op "myemacs/tests"))))

(defsystem ":myemacs/tests"
  :author "José A. Navarro Ramón"
  :license "BSD 3-Clause"
  :depends-on ("myemacs"
               "rove")
  :components ((:module "tests"
                :components
			((:file "main"
				:depends-on ("packages")))))
  :description "Test system for myemacs"
  :perform (test-op (op c) (symbol-call :rove :run c)))



