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
(defun err-do-not-use-main-en (&optional (stream t))
  (format stream "Execution mode error: type (myemacs ...) to run the program, please."))

(defun err-unsupported-exec-mode-en (exec-mode &optional (stream t))
  (format stream "Error, unsupported exec-mode :~a" exec-mode))

(defun err-unrecognized-os-type-en (&optional (stream t))
  (format stream "Error, unrecognized OS"))

(defun err-unsupported-os-type-en (os-type-name &optional (stream t))
  (format stream "Error, unsupported ~a OS" (string-upcase os-type-name)))

(defun err-unsupported-lisp-en (lisp-name &optional (stream t))
  (format stream "Error, unsupported ~a common-lisp" (string-upcase lisp-name)))

(defun err-unsupported-lisp-version-en (lisp-name lisp-version &optional (stream t))
  (format stream "Error, unsupported common-lisp version ~a (~a)"
	  (string-upcase lisp-name) lisp-version))

(defun err-first-arg-not-a-command-en (first-arg &optional (stream t))
  (format stream "Error, the first argument '~a' is not a command (it must begin with a colon)."
	  first-arg))

(defun err-invalid-command-en (cmd &optional (stream t))
  (format stream "Error, invalid command :~a." cmd))

(defun err-num-options-en (cmd &optional (stream t))
  (format stream "Error, incorrect options number for command :~a." cmd))

(defun err-incorrect-option-en (cmd &optional (stream t))
  (format stream "Error, incorrect option found for command :~a." cmd))

;;; ******************** WARNING MESSAGES

;;; ******************** INFO MESSAGES
(defun info-argument-list-en (largs &optional (stream t))
  (format stream "The argument list is: ~a" largs))

(defun info-execution-mode-en (exec-mode &optional (stream t))
  (format stream "The execution mode is :~a" exec-mode))


;;; ******************** TEXT STRINGS


