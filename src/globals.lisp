;;;; globals.lisp
;;;; Dynamic variables.
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

;;; Language global variables
(defparameter *default-language* :en)
(defparameter *user-language* nil)
(defparameter *args-language* nil)
(defparameter *language* *default-language*)

;;; Supported characteristics
(defparameter *supported-exec-modes* '(:repl :standalone :script))
(defparameter *supported-os-types* '(:unix))

;;; HASH-TABLE *data*
;;; 'exec-mode
;;; 'os-type
;;; 'os-type-name
;;; 'os
(defparameter *data* (make-hash-table))
