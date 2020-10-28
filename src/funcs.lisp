;;;; funcs.lisp
;;;; General purpose functions.
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

;;; ******************** GENERAL PURPOSE FUNCTIONS
(defun remove-char-from-strlst (char lstr)
  "Elimina el caracter 'char' de cada una de las cadenas que forma la lista."
  (if (null lstr)
      nil
      (cons (remove char (car lstr))
	    (remove-char-from-strlst char (cdr lstr)))))

(defun repeat-string (string ntimes)
  (format nil "~v@{~A~:*~}" ntimes string))
