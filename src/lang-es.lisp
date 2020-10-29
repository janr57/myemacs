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

;;; ******************** CADENAS DE TEXTO
(defun strinfo-version-es (&optional (stream t))
  (format stream "~a v~a ~a (~a)" *progname* *version* *production* *version-date*))

(defun strinfo-copyright-es (&optional (stream t))
  (format stream "Copyright (C) ~d ~a <~a>" *year* *author* *email*))

(defun strinfo-license-es (&optional (stream t))
  (format stream "Licencia ~a." *license*))

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
  (format stream "Error, el primer argumento '~a' no es un comando." first-arg))

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
;;; Message in response to the ':show' command when there is no configuration at all.
(defun info-action-show-no-cfg-es (&optional (stream t))
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil))
  (format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
  (format stream "- INFO: Configuraciones almacenadas     -> NO ENCONTRADAS~%")
  (format stream "- INFO: Configuración activa            -> NO ENCONTRADA~%")
  (terpri stream)
  (format stream "Posibles acciones:~%")
  (format stream "1) Crear una configuración nativa de'emacs' para almacenarla más tarde.~%~%"))

;;; Mensaje de respuesta al comando :show cuando hay una configuración activa
;;; y, al menos, otra alternativa que se pueda activar.
(defun info-action-show-active-alt-es (active-cfg available-cfgs &optional (stream t))
  (let ((available-cfgs-str (join-strings-from-list available-cfgs))
	(other-cfgs-str (join-strings-from-list
			    (remove active-cfg available-cfgs :test #'string-equal))))
    
    (format stream "~a~%" (strinfo-version-es nil))
    (format stream "~a~%" (strinfo-copyright-es nil))
    (format stream "~a~%~%" (strinfo-license-es nil))
    
    (format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
    (format stream "- INFO: Configuraciones almacenadas     -> ~a~%" available-cfgs-str)
    (format stream "- INFO: Configuración activa            -> (~a)~%" active-cfg)
    (terpri stream)
    (format stream "Posibles acciones:~%")
    (format stream "1) Activar otra configuración:~%")
    (format stream "   --> myemacs :use <cfg>~%")
    (format stream "   Configuraciones alternativas: ~a~%" other-cfgs-str)
    (format stream "2) Borrar una configuración almacenada:~%")
    (format stream "   --> myemacs :del <cfg>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "3) Copiar una configuración almacenada:~%")
    (format stream "   --> myemacs :copy <orig> <dest>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "4) Ejecutar 'emacs' y crear una configuración nativa.~%~%")))

;;; Mensaje de respuesta al comando :show cuando hay una configuración activa
;;; y no hay ninguna otra alternativa que se pueda activar.
(defun info-action-show-active-noalt-es (active-cfg available-cfgs &optional (stream t))
  (let ((available-cfgs-str (join-strings-from-list available-cfgs))
	(other-cfgs-str (join-strings-from-list
			    (remove active-cfg available-cfgs :test #'string-equal))))
    
    (format stream "~a~%" (strinfo-version-es nil))
    (format stream "~a~%" (strinfo-copyright-es nil))
    (format stream "~a~%~%" (strinfo-license-es nil))
    
    (format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
    (format stream "- INFO: Configuraciones almacenadas     -> ~a~%" available-cfgs-str)
    (format stream "- INFO: Configuración activa            -> (~a)~%" active-cfg)
    (terpri stream)
    (format stream "Posibles acciones:~%")
    (format stream "1) Borrar una configuración almacenada, esté o no activa:~%")
    (format stream "   --> myemacs :del <cfg>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "2) Copiar una configuración almacenada, esté o no activa:~%")
    (format stream "   --> myemacs :copy <orig> <dest>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "3) No cambiar nada y continuar usando esta configuración.~%~%")))

;;; Mensaje de respuesta al comando ':show' cuando no hay ni configuración nativa, ni activa.
;;; Sólo hay configuraciones almacenadas.
(defun info-action-show-only-saved-cfgs-es (available-cfgs &optional (stream t))
  (let ((available-cfgs-str (join-strings-from-list available-cfgs)))
  
    (format stream "~a~%" (strinfo-version-en nil))
    (format stream "~a~%" (strinfo-copyright-en nil))
    (format stream "~a~%~%" (strinfo-license-en nil))
    (format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
    (format stream "- INFO: Configuraciones almacenadas     -> ~a~%" available-cfgs-str)
    (format stream "- INFO: Configuración activa            -> NO ENCONTRADA~%")
    (terpri stream)
    (format stream "Posibles acciones:~%")
    (format stream "1) Activar una configuración:~%")
    (format stream "   --> myemacs :use <cfg>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "2) Borrar una configuración almacenada:~%")
    (format stream "    --> 'myemacs :del <cfg>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "3) Copiar una configuración almacenada:~%")
    (format stream "    --> 'myemacs :copy <orig> <dest>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "4) Usar 'emacs' para crear una configuración nativa.~%~%")))

;;; Mensaje de respuesta al comando :show cuando hay una configuración nativa de 'emacs'
;;; y alguna configuración almacenada.
(defun info-action-show-native-alt-es (available-cfgs &optional (stream t))
  (let ((available-cfgs-str (join-strings-from-list available-cfgs)))
    
    (format stream "~a~%" (strinfo-version-es nil))
    (format stream "~a~%" (strinfo-copyright-es nil))
    (format stream "~a~%~%" (strinfo-license-es nil))
    
    (format stream "- INFO: Configuración nativa de 'emacs' -> ENCONTRADA~%")
    (format stream "- INFO: Configuraciones almacenadas     -> ~a~%" available-cfgs-str)
    (format stream "- INFO: Configuración activa            -> MO ENCONTRADA~%")
    (terpri stream)
    (format stream "Posibles acciones:~%")
    (format stream "1) Borrar la configuración nativa de 'emacs':~%")
    (format stream "   --> myemacs :del *~%")
    (format stream "2) Copiar la configuración nativa como <dest>:~%")
    (format stream "   --> myemacs :copy * <dest>~%")
    (format stream "   Nombres a evitar: ~a~%" available-cfgs-str)
    (format stream "3) Borrar una configuración almacenada:~%")
    (format stream "   --> myemacs :del <cfg>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "3) Copiar una configuración almacenada:~%")
    (format stream "   --> myemacs :copy <orig> <dest>~%")
    (format stream "   Configuraciones disponibles: ~a~%" available-cfgs-str)
    (format stream "4) Ejecutar 'emacs' usando la configuración nativa.~%~%")))

;;; Mensaje de respuesta al comando :version
(defun info-action-version-es (&optional (stream t))
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil)))

;;; Mensaje de respuesta al comando :help
(defun info-action-help-es (&optional (stream t))
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil))
    
  (format stream "USO: [ :help      || :version    || :show      ||~%")
  (format stream "       :use <cfg> || :save <cfg> || :del <cfg> || :copy <orig> <dest> ]~%")
  (format stream "     [ :debug || :verbose || :lang <en || es> ]~%")
  (format stream "~%")
  (format stream "(blanco)    -> Este mensaje.~%")
  (format stream ":help       -> Este mensaje.~%")
  (format stream ":version    -> Versión del programa.~%")
  (format stream ":show       -> Muestra configuraciones y posibles acciones a tomar.~%")
  (format stream ":use <cfg>  -> Activa la configuración <cfg>.~%")
  (format stream ":save <cfg> -> Guarda la configuración nativa de 'emacs' como <cfg>.~%")
  (format stream ":del <cfg>  -> Borra la configuración <cfg>.~%")
  (format stream ":debug      -> Muestra información de depuración.~%")
  (format stream ":verbose    -> Muestra más información al ejecutar algún comando (si procede).~%")
  (format stream ":lang < en || es > -> Muestra los mensajes en el idioma elegido.~%~%"))


