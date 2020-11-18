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

;;; doc-main-main-script
(defun main-documentation ()
  (format t "(main-documentation) LANGUAGE -> ~a~%" *language*)
  (setf (documentation 'myemacs-script 'function)
	(msg (doc-main-myemacs-script))))

;;; ********************* AUXILIARY FUNCTIONS
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
	      (multiple-value-bind (debug-flag verbose-flag)
		  (find-global-commands standard-args)
		(let ((cleaned-standard-args (remove-global-commands standard-args)))
		  (multiple-value-bind (commands-number-ok too-many-commands-closure)
		      (approve-num-commands cleaned-standard-args)
		    (setf *debug-flag* debug-flag)
		    (setf *verbose-flag*  verbose-flag)
		    
		    (when (eql exec-mode :repl)
		      (main-documentation))
		    
		    ;; Print the appropriate message if any error was detected
		    (cond
		      ((not supported-exec-mode)
		       (format t "~a~%" (funcall exec-mode-error-closure)))
		      ((not supported-os-type)
		       (format t "~a~%" (funcall os-type-error-closure)))
		      ((not supported-lisp)
		       (format t "~a~%" (funcall lisp-error-closure)))
		      ((not process-ok)
		       (format t "~a~%" (funcall args-error-closure)))
		      ((not lcmd-ok)
		       (format t "~a~%" (funcall lcmd-closure)))
		      ((not no-repeated-command)
		       (format t "~a~%" (funcall repeated-command-closure)))
		      ((not commands-number-ok)
		       (format t "~a~%" (funcall too-many-commands-closure)))
		      ;; Ready for delivery commands to the appropriate functions
		      (t (action-delivery-center (car cleaned-standard-args))))
		    ;; Exit if errors
		    (if (or (not supported-exec-mode)
			    (not supported-os-type)
			    (not supported-lisp)
			    (not process-ok)
			    (not lcmd-ok)
			    (not no-repeated-command)
			    (not commands-number-ok))
			nil t)))))))))))
  

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


