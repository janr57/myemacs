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
(defun info-action-show-active-config-es (active-config available-configs &optional (stream t))
  (let ((available-configs-str (join-strings-from-list available-configs))
	(other-configs-str (join-strings-from-list
			    (remove active-config available-configs :test #'string-equal))))
    (format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
    (format stream "- INFO: Configuraciones almacenadas     -> ~a~%" available-configs-str)
    (format stream "- INFO: Configuración activa            -> (~a)~%" active-config)
    (terpri stream)
    (format stream "Posibles acciones:~%")
    (format stream "1) Activar otra configuración -> myemacs :use <cfg>~%") 
    (format stream "   Configuraciones alternativas: ~a~%" other-configs-str)
    (format stream "2) Borrar una configuración almacenada, esté o no activa -> myemacs :del <cfg>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-configs-str)
    (format stream "3) No cambiar nada y continuar usando esta configuración.~%~%")))

(defun info-action-version-es (&optional (stream t))
  (format stream "~a v~a ~a (~a)~%" *progname* *version* *production* *version-date*))

(defun info-action-help-es (&optional (stream t))
  (format stream "USO: [ :help      || :version    || :show      ||~%")
  (format stream "       :use <cfg> || :save <cfg> || :del <cfg> || :copy <orig> <dest> ]~%")
  (format stream "     [ :debug || :verbose || :lang <en || es> ]~%")
  (format stream "~%")
  (format stream "(blanco)    -> Este mensaje.~%")
  (format stream ":help       -> Este mensaje.~%")
  (format stream ":version    -> Versión del programa.~%")
  (format stream ":show       -> Mostrar configuración activa y posibles acciones a tomar.~%")
  (format stream ":use <cfg>  -> Usar <cfg> como configuración activa.~%")
  (format stream ":save <cfg> -> Guardar la configuración de emacs por defecto con nombre <cfg>.~%")
  (format stream ":del <cfg>  -> Borrar la configuración <cfg>.~%")
  (format stream ":debug      -> Mostrar información de depuración.~%")
  (format stream ":verbose    -> Mostrar más información al ejecutar algún comando (si procede).~%")
  (format stream ":lang < en || es > -> Mostrar los mensajes en el siguiente idioma.~%")
  (terpri stream))

;;(defun info-action-help-es (&optional (stream t))
;;  (format stream "USO: [ :help || :version || :show || :use <cfg> || :save <cfg> || :del <cfg> ]~%")
;;  (format stream "     [ :debug || :verbose || :lang <en || es> ]~%")
;;  (format stream "~%")
;;  (format stream "(blanco)    -> Este mensaje.~%")
;;  (format stream ":help       -> Este mensaje.~%")
;;  (format stream ":version    -> Versión del programa.~%")
;;  (format stream ":show       -> Mostrar configuración activa y posibles acciones a tomar.~%")
;;  (format stream ":use <cfg>  -> Usar <cfg> como configuración activa.~%")
;;  (format stream ":save <cfg> -> Guardar la configuración de emacs por defecto con nombre <cfg>.~%")
;;  (format stream ":del <cfg>  -> Borrar la configuración <cfg>.~%")
;;  (format stream ":debug      -> Mostrar información de depuración.~%")
;;  (format stream ":verbose    -> Mostrar más información al ejecutar algún comando (si procede).~%")
;;  (format stream ":lang < en || es > -> Mostrar los mensajes en el siguiente idioma.~%")
;;  (terpri stream))

;;; ******************** CADENAS DE TEXTO
(defun strinfo-copyright-es (&optional (stream t))
  (format stream "Copyright (C) ~d ~a <~a>" *year* *author* *email*))

(defun strinfo-license-es (&optional (stream t))
  (format stream "Licencia ~a." *license*))


