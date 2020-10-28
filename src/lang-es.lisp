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
(defun err-do-not-use-main-es (&optional (stream t))
  (format stream "Error, modo de ejecución incorrecto, debe teclear: (myemacs ...)"))

(defun err-unsupported-exec-mode-es (exec-mode &optional (stream t))
  (format stream "Error, modo de ejecución no reconocido :~a" exec-mode))

(defun err-unrecognized-os-type-es (&optional (stream t))
  (format stream "Error, sistema operativo desconocido"))

(defun err-unsupported-os-type-es (os-type-name &optional (stream t))
  (format stream "Error, sistema operativo ~a no soportado." os-type-name))

(defun err-unsupported-lisp-es (lisp-name &optional (stream t))
  (format stream "Error, common lisp no soportado ~a" (string-upcase lisp-name)))

(defun err-unsupported-lisp-version-es (lisp-name lisp-version &optional (stream t))
  (format stream "Error, versión de common lisp no soportada ~a (~a)"
	  (string-upcase lisp-name) lisp-version))

(defun err-first-arg-not-a-command-es (first-arg &optional (stream t))
  (format stream "Error, el primer argumento '~a' no es un comando (debe comenzar por dos puntos)."
	  first-arg))

(defun err-invalid-command-es (cmd &optional (stream t))
  (format stream "Error, comando no válido ':~a'." cmd))

(defun err-num-options-es (cmd &optional (stream t))
  (format stream "Error, número de opciones incorrecto para el comando ':~a.'" cmd))

(defun err-incorrect-option-es (cmd &optional (stream t))
  (format stream "Error, el comando ':~a' tiene una opción incorrecta." cmd))

(defun err-repeated-command-es (cmd &optional (stream t))
  (format stream "Error, se ha repetido el comando ':~a'." cmd))

(defun err-too-many-commands-es (&optional (stream t))
  (format stream "Error ¡se han tecleado demasiados comandos!"))

;;; ******************** MENSAJES DE ADVERTENCIA

;;; ******************** MENSAJES DE INFORMACIÓN
(defun info-argument-list-es (largs &optional (stream t))
  (format stream "La lista de argumentos es: ~a" largs))

(defun info-execution-mode-es (exec-mode &optional (stream t))
  (format stream "El modo de ejecución es: ~a" exec-mode))


;;; ******************** CADENAS DE TEXTO



