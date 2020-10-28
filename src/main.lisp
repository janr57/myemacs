;;;; main.lisp
;;;; Allows the user to run 'myemacs' in three different ways:
;;;; 1) Run in 'SBCL' or 'Clozure Common Lisp' REPLs.
;;;;    (ql:quickload :myemacs)
;;;;    (in-package :myemacs)
;;;;    (myemacs)
;;;; 2) Built as an 'SBCL' or 'Clozure Common Lisp' standalone executable file:
;;;;    $ cd ~/quicklisp/local-project/myemacs
;;;;    $ make myemacs_sbcl     or     $ make myemacs_ccl
;;;;    $ ./myemacs_sbcl        or     $ ./myemacs_ccl 
;;;; 3) Run as a script using 'SBCL' or 'Clozure Common Lisp' (not working yet...)
;;;;    $ sbcl --load "<script-file>"
;;;;
;;;; The 'myemacs' program keeps track of different configurations of 'emacs'.
;;;; The user can change between them.
;;;;
;;;; Copyright (c) 2020 - José A. Navarro Ramón <janr.devel@gmail.com>
;;;; License: BSD 3-Clause

;;; Declaration specifier at compile time (unmark only one of them):
;;; a) In the development phase
(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; b) In the release phase
;;;(declaim (optimize (speed 3) (safety 3) (debug 0)))

(in-package :myemacs)

;; ********************* AUXILIARY FUNCTIONS
;;; All different forms of running the program (:repl, :standalone or :script) converge here.
;;; Arguments:
;;; 'largs': Argument list coming from :standalone, :script or :repl.
;;;          When coming from :standalone or :script it is a list of strings, otherwise it
;;;          is based on symbols (keywords and plain symbols).
;;; 'exec-mode': The execution mode, which may be ':standalone', ':script', ':repl' or 'NIL',
;;;              depending on the function which called 'main'
;;;              ('NIL' in case it was 'main' itself).
;;; Returns:
;;; 'T' or 'NIL' if run without o with errors.
(defun main (&optional (args nil) (exec-mode nil))
  ;;(format t "(main) args -> ~a~%" args)
  ;;(format t "(main) exec-mode -> ~a~%" exec-mode)
  (multiple-value-bind (supported-exec-mode exec-mode-error-closure)
      (register-and-approve-exec-mode exec-mode)
    (multiple-value-bind (supported-os-type os-type-error-closure)
	(register-and-approve-os)
      (multiple-value-bind (supported-lisp lisp-error-closure)
	  (register-and-approve-lisp)
	(multiple-value-bind (process-ok args-error-closure standard-args)
	    (standarize-and-register-args args)
	  (multiple-value-bind (lcmd-ok lcmd-closure)
	      (approve-standard-args standard-args)
	    (multiple-value-bind (no-repeated-command repeated-command-closure)
		(find-repeated-command standard-args)
	    ;;(format t "(main) no-repeated-commands -> ~a~%" no-repeated-commands)
	    ;;(format t "(main) repeated-commands-closure -> ~a~%" repeated-commands-closure)
	    ;; Print the appropriate message
	    (cond
	      ((null supported-exec-mode)
	       (format t "~a~%" (funcall exec-mode-error-closure)))
	      ((null supported-os-type)
	       (format t "~a~%" (funcall os-type-error-closure)))
	      ((null supported-lisp)
	       (format t "~a~%" (funcall lisp-error-closure)))
	      ((null process-ok)
	       (format t "~a~%" (funcall args-error-closure)))
	      ((null lcmd-ok)
	       (format t "~a~%" (funcall lcmd-closure)))
	      ((null no-repeated-command)
	       (format t "~a~%" (funcall repeated-command-closure)))
	      (t (format t "OK!~%")))
	    ;;
	    (if (or (null supported-exec-mode)
		    (null supported-os-type)
		    (null supported-lisp)
		    (null process-ok)
		    (null lcmd-ok)
		    (null no-repeated-command))
		nil t))))))))
    
;;  (msg (info-argument-list largs))
;;  (terpri)
;;  (msg (info-execution-mode exec-mode))

;;; ********
  
;;; ********************* SERVICEABLE FUNCTIONS
;;; Macro which is the main entry point from the REPL
;;; It's purpose is to allow a syntax like e.g.:
;;; (myemacs :use config) instead of (myemacs '(:use config))
;;; Parameters:
;;; 'lrepl-args': argument list, which consistes of a list of symbols, which may be:
;;;                - keywords, representing commands
;;;                - plain symbols, representing options for the commands
;;; It passes to 'main', this argument list and the execution mode :repl
(defmacro myemacs (&rest repl-args)
  ;;(format t "(myemacs) repl-args -> ~a~%" repl-args)
  `(main (quote ,repl-args) :repl))

;;; EXECUTABLE PROGRAM
;;; Entry point of 'myemacs' as a standalone executable program.
;;; The 'myemacs_sbcl' or 'myemacs_ccl' executables can be obtained:
;;; $ cd ~/quicklisp/local-projects/myemacs
;;; $ make myemacs_sbcl or $ make myemacs_ccl
;;; $ ./myemacs_sbcl or $ ./myemacs_ccl
;;; Returns:
;;; 0 or 1 if run without or with errors.
;;; (EXPORTED FUNCTION)
(defun myemacs-standalone ()
  (let ((terminal-args (cdr (get-lisp-arglist))))
    (if (funcall #'main terminal-args :standalone) 0 1)))

;;; SCRIPT FILE (not working yet...)
;;; Entry point of 'myemacs' as a standalone executable program.
;;; Returns:
;;; 0 or 1 if run without or with errors.
;;; (EXPORTED FUNCTION)
  (defun myemacs-script ()
    (let ((terminal-args (cdr (get-lisp-arglist))))
    (if (funcall #'main terminal-args :script) 0 1)))

;;; ********

