;;;; main.lisp
;;;; Allows the user to run 'myemacs' in three different ways:
;;;; - run in 'SBCL' or 'Clozure Common Lisp' REPLs.
;;;; - built as an 'SBCL' or 'Clozure Common Lisp' standalone executable file.
;;;; - run as a script using 'SBCL'.
;;;;
;;;; The 'myemacs' program keeps track of different configurations of 'emacs'.
;;;; The user can change between them.
;;;;
;;;; Copyright (c) 2020 - José A. Navarro Ramón <josea.navarro1@gmail.com>
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
  (format t "(main) exec-type -> ~a~%" exec-type))

;;; ********************
;;; Macro which is the main entry point from the REPL
(defmacro myemacs (&rest largs-repl)
  `(main (quote ,largs-repl) :repl))

;;; ********************
;;; Entry point of 'myemacs' as a standalone executable program.
;;; (EXPORTED FUNCTION)
(defun myemacs-standalone ()
  (let ((lterminal-args nil))
    (funcall #'main lterminal-args :standalone)))

;;; ********************
;;; Entry point of 'myemacs' as a standalone executable program.
;;; (EXPORTED FUNCTION)
  (defun myemacs-script ()
    (let ((lterminal-args nil))
    (funcall #'main lterminal-args :script)))
;;; ********

