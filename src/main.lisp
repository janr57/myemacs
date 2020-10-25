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

;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************

;;; ********************
;;; Entry point when run through the REPL.
;;; All different forms of running the program (:repl, :standalone or :script) converge here.
;;; Arguments:
;;; 'largs': Argument list coming from :standalone, :script or :repl.
;;;          When coming from :standalone or :script it is based on strings, otherwise it
;;;          is based on symbols.
;;; (EXPORTED FUNCTION)
(defun main (&optional (largs nil) (exec-type nil))
  (format t "(main) largs -> ~a~%" largs)
  (format t "(main) exec-type -> ~a~%" exec-type))

;;; ********************
;;; Macro which is the main entry point from the REPL
(defmacro myemacs (&rest largs-repl)
  `(main (quote ,largs-repl) :repl))

;;; ********************
;;; EXECUTABLE PROGRAM
;;; Entry point of 'myemacs' as a standalone executable program.
;;; The 'myemacs_sbcl' or 'myemacs_ccl' executables can be obtained:
;;; $ cd ~/quicklisp/local-projects/myemacs
;;; $ make myemacs_sbcl or $ make myemacs_ccl
;;; $ ./myemacs_sbcl or $ ./myemacs_ccl
;;; (EXPORTED FUNCTION)
(defun myemacs-standalone ()
  (let ((lterminal-args nil))
    (funcall #'main lterminal-args :standalone)))

;;; ********************
;;; SCRIPT FILE (not working yet...)
;;; Entry point of 'myemacs' as a standalone executable program.
;;; (EXPORTED FUNCTION)
  (defun myemacs-script ()
    (let ((lterminal-args nil))
    (funcall #'main lterminal-args :script)))
;;; ********

