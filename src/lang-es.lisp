;;;; lang-es.lisp
;;;; Messages in Spanish
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

;;; ******************** MENSAJES DE ERROR

;;; ******************** MENSAJES DE ADVERTENCIA

;;; ******************** MENSAJES DE INFORMACIÓN
(defun msginfo-argument-list-es (largs &optional (stream t))
  (format stream "La lista de argumentos es: ~a~%" largs))

(defun msginfo-execution-mode-es (exec-mode &optional (stream t))
  (format stream "El modo de ejecución es: ~a~%" exec-mode))

;;; ******************** CADENAS DE TEXTO



