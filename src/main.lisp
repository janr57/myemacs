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

;;; ******************** ERROR MESSAGES
;;; ********

;;; ******************** WARNING MESSAGES
;;; ********

;;; ******************** INFO MESSAGES
;;; ********

;;; ********************* AUXILIARY FUNCTIONS **************************
;;; ********


;;; ******************** MAIN FUNCTION *********************************
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

(defun main (&optional (largs nil) (exec-mode nil))
  (msg (info-argument-list largs))
  (terpri)
  (msg (info-execution-mode exec-mode)))

  ;;(msginfo-argument-list largs)
  ;;(msginfo-execution-mode exec-mode))

;;; ********
  
;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************

;;; Macro which is the main entry point from the REPL
;;; It's purpose is to allow a syntax like e.g.:
;;; (myemacs :use config) instead of (myemacs '(:use config))
;;; Parameters:
;;; 'lrepl-args': argument list, which consistes of a list of symbols, which may be:
;;;                - keywords, representing commands
;;;                - plain symbols, representing options for the commands
;;; It passes to 'main', this argument list and the execution mode :repl
(defmacro myemacs (&rest lrepl-args)
  `(main (quote ,lrepl-args) :repl))

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
  (let ((lterminal-args nil))
    (if (funcall #'main lterminal-args :standalone) 0 1)))

;;; SCRIPT FILE (not working yet...)
;;; Entry point of 'myemacs' as a standalone executable program.
;;; Returns:
;;; 0 or 1 if run without or with errors.
;;; (EXPORTED FUNCTION)
  (defun myemacs-script ()
    (let ((lterminal-args nil))
    (if (funcall #'main lterminal-args :script) 0 1)))

;;; ********

