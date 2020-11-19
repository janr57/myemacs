;;;; doc-en.lisp
;;;; Documentation in English
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

;;; ******************** MAIN

;;                 0000000001111111111222222222233333333333444444444555555555566666666667777777777
;;                 1234567890123456789012345678901234567809123456789012345678901234567890123456789
(defun doc-main-myemacs-en ()
  (with-output-to-string (stream)
    (format stream "Entry point of 'myemacs' in the REPL.~%")
    (format stream " So, allowed expressions must be s-expressions.")
    (format stream " Parameters:~%")
    (format stream " 'repl-args': Argument list, which consists of a list of symbols:~%")
    (format stream "               - Commands, represented as keywords.~%")
    (format stream "               - Symbols, represented as plain symbols.~%")
    (format stream " (EXPORTED MACRO)~%")
    (format stream " Examples:~%")
    (format stream "   (myemacs :help)~%")
    (format stream "   (myemacs :show)~%")
    (format stream "   (myemacs :save-native-as myconfig)")))

(defun doc-main-myemacs-standalone-en ()
  (with-output-to-string (stream)
    (format stream "Entry point of 'myemacs' as a standalone executable program.~%")
    (format stream " Parameters:~%")
    (format stream " ---------~%")
    (format stream " This function relies in the LISP implementation to get the arguments.~%")
    (format stream " Returns:~%")
    (format stream "   0: No errors / 1: Errors~%")
    (format stream " (EXPORTED FUNCTION)~%")
    (format stream " Examples:~%")
    (format stream "    myemacs :help~%")
    (format stream "    myemacs :show~%")
    (format stream "    myemacs :save-native-as myconfig")))

(defun doc-main-myemacs-script-en ()
  (with-output-to-string (stream)
    (format stream "Entry point of 'myemacs' as a script executable program.~%")
    (format stream " Parameters:~%")
    (format stream " ---------~%")
    (format stream " This function relies in the LISP implementation to get the arguments.~%")
    (format stream " Returns:~%")
    (format stream "   0: No errors / 1: Errors~%")
    (format stream " (EXPORTED FUNCTION)")))
