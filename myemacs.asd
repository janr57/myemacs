;;;; main.lisp
;;;; Allows the user to run 'myemacs' in three different ways:
;;;;
;;;; The 'myemacs' program keeps track of different configurations of 'emacs'.
;;;; The user can change between them.
;;;;
;;;; Copyright (c) 2020 - José A. Navarro Ramón <josea.navarro1@gmail.com>
;;;; License: BSD 3-Clause


(defsystem :myemacs
  :version "0.1.0"
  :author "José A. Navarro Ramón"
  :license "BSD 3-Clause"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
			((:file "packages")
			 (:file "globals"
			  :depends-on ("packages"))
			 (:file "lang-en"
			  :depends-on ("packages"))
			 (:file "lang-es"
			  :depends-on ("packages"))
			 (:file "lang"
			  :depends-on ("packages" "globals"))
			 (:file "main"
			  :depends-on ("packages")))))
  :description ""
  :in-order-to ((test-op (test-op "myemacs/tests"))))

(defsystem :myemacs/tests
  :author "José A. Navarro Ramón"
  :license "BSD 3-Clause"
  :depends-on ("myemacs"
	       "alexandria"
               "rove")
  :components ((:module "tests"
                :components
			((:file "packages")
			 (:file "globals"
			  :depends-on ("packages"))
			 (:file "lang-en"
			  :depends-on ("packages"))
			 (:file "lang-es"
			  :depends-on ("packages"))
			 (:file "lang"
			  :depends-on ("packages" "globals"))
			 (:file "main"
			  :depends-on ("packages" "lang")))))
  :description "Test system for myemacs"
  :perform (test-op (op c) (symbol-call :rove :run c)))


(defsystem :myemacs/executable
  :build-operation program-op
  :build-pathname "myemacs"
  :entry-point "myemacs:myemacs-standalone"
  :depends-on ("alexandria")
  :components ((:module "src"
		:components
			((:file "packages")
			 (:file "globals"
				:depends-on ("packages"))	
			 (:file "lang-en"
			  :depends-on ("packages"))
			 (:file "lang-es"
			  :depends-on ("packages"))
			 (:file "lang"
			  :depends-on ("packages"
				       "globals"))
			 (:file "main"
			  :depends-on ("packages"))))))



  
