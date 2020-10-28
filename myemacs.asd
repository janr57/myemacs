;;;; myemacs.asd
;;;; ASDF system definitions for 'myemacs'.
;;;;
;;;; The 'myemacs' program keeps track of different configurations of 'emacs'.
;;;; The user can change between them.
;;;;
;;;; Copyright (c) 2020 - José A. Navarro Ramón <janr.devel@gmail.com>
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
			 (:file "funcs"
			  :depends-on ("packages"))
			 (:file "lang-en"
			  :depends-on ("packages"))
			 (:file "lang-es"
			  :depends-on ("packages"))
			 (:file "lang"
			  :depends-on ("packages" "globals" "lang-en" "lang-es"))
			 (:file "exec-mode"
			  :depends-on ("lang"))
			 (:file "os"
			  :depends-on ("lang"))
			 (:file "cl"
			  :depends-on ("lang"))
			 (:file "lex-args"
			  :depends-on ("lang" "funcs"))
			 (:file "syn-args"
			  :depends-on ("lang"))
			 (:file "main"
			  :depends-on ("exec-mode" "os" "cl" "lex-args" "syn-args")))))
  :description ""
  :in-order-to ((test-op (test-op "myemacs/tests"))))

(defsystem :myemacs/tests
  :author "José A. Navarro Ramón"
  :license "BSD 3-Clause"
  :depends-on ("myemacs" "rove")
  :components ((:module "tests"
                :components
			 ((:file "main"))))
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
			 (:file "funcs"
			  :depends-on ("packages"))
			 (:file "lang-en"
			  :depends-on ("packages"))
			 (:file "lang-es"
			  :depends-on ("packages"))
			 (:file "lang"
			  :depends-on ("packages" "globals" "lang-en" "lang-es"))
			 (:file "exec-mode"
			  :depends-on ("lang"))
			 (:file "os"
			  :depends-on ("lang"))
			 (:file "cl"
			  :depends-on ("lang"))
			 (:file "lex-args"
			  :depends-on ("lang" "funcs"))
			 (:file "syn-args"
			  :depends-on ("lang"))
			 (:file "main"
			  :depends-on ("exec-mode" "os" "cl" "lex-args" "syn-args"))))))







  
