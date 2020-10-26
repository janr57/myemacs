;;;; lang-en.lisp
;;;; Messages in English
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

;;; ******************** WARNING MESSAGES

;;; ******************** INFO MESSAGES
(defun info-argument-list-en (largs &optional (stream t))
  (format stream "The argument list is: ~a" largs))

(defun info-execution-mode-en (exec-mode &optional (stream t))
  (format stream "The execution mode is :~a" exec-mode))


;;; ******************** TEXT STRINGS


