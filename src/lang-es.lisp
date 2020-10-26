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
(defun err-do-not-run-program-with-main-es (&optional (stream t))
  (format stream "Error, se debe teclear (myemacs ...) para ejecutar el programa."))

(defun err-unsupported-exec-mode-es (exec-mode &optional (stream t))
  (format stream "Error, modo de ejecución no reconocido :~a" exec-mode))

(defun err-unsupported-os-type-es (os-type-name &optional (stream t))
  (format stream "Error, sistema operativo ~a no soportado." os-type-name))

;;; ******************** MENSAJES DE ADVERTENCIA

;;; ******************** MENSAJES DE INFORMACIÓN
(defun info-argument-list-es (largs &optional (stream t))
  (format stream "La lista de argumentos es: ~a" largs))

(defun info-execution-mode-es (exec-mode &optional (stream t))
  (format stream "El modo de ejecución es: ~a" exec-mode))


;;; ******************** CADENAS DE TEXTO


