;;;; doc-es.lisp
;;;; Documentation in Spanish
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

;;             0000000001111111111222222222233333333333444444444555555555566666666667777777777
;;             1234567890123456789012345678901234567809123456789012345678901234567890123456789
(defun doc-main-myemacs-script-es ()
  (with-output-to-string (stream)
    (format stream "Punto de entrada cuando 'myemacs' se ejecuta como un script.~%")
    (format stream " Retorna:~%")
    (format stream "   0: Sin errores / 1: Con errores~%")
    (format stream " (FUNCIÓN EXPORTADA)")))
