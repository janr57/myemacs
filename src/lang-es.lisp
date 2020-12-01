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

;;; ******************** QUESTIONS
(defun ask-delete-directory-tree-es (dir)
  (format nil "¿Borrar directorio ~a? " dir))

(defparameter *yes-simple-es* '(("S" . t) ("SI" . t) ("SÍ" . t)))

(defparameter *no-simple-es* '(("N" . nil) ("NO" . nil)))

(defparameter *yes-complex-es* '(("VALE" . t) ("OK" . t) ("CORRECTO" . t)
				 ("VENGA" . t) ("DE ACUERDO" . t)))

(defparameter *no-complex-es* '(("NUNCA" . nil) ("PARA" . nil) ("NO QUIERO" . nil)
				 ("NO LO HAGAS" . nil) ("NONES" . nil)))

(defparameter *yes-no-assoc-es* (append *yes-simple-es* *no-simple-es*
				  *yes-complex-es* *no-complex-es*))

;;; ******************** CADENAS DE TEXTO
(defun strinfo-version-es (&optional (stream t))
  (format stream "~a v~a ~a (~a)" +progname+ +version+ +status+ +version-date+))

(defun strinfo-copyright-es (&optional (stream t))
  (format stream "Copyright (C) ~d ~a <~a>" +year+ +author+ +email+))

(defun strinfo-license-es (&optional (stream t))
  (format stream "Licencia ~a." +license+))

;;; ******************** MENSAJES DE ERROR
(defun err-action-use-native-cfg-es (&optional (stream t))
  (format stream "Error, hay una configuración nativa de emacs. No se puede activar otra.~%"))

(defun err-cfg-not-available-es (cfg &optional (stream t))
  (format stream "Error, configuración no disponible: <~a>~%" cfg))

(defun err-do-not-use-main-es (&optional (stream t))
  (format stream "Error, modo de ejecución incorrecto, debe teclear: (myemacs ...)~%"))

(defun err-unsupported-exec-mode-es (exec-mode &optional (stream t))
  (format stream "Error, modo de ejecución no reconocido :~a~%" exec-mode))

(defun err-unrecognized-os-type-es (&optional (stream t))
  (format stream "Error, sistema operativo desconocido~%"))

(defun err-unsupported-os-type-es (os-type-name &optional (stream t))
  (format stream "Error, sistema operativo ~a no soportado~%." os-type-name))

(defun err-unsupported-lisp-es (lisp-name &optional (stream t))
  (format stream "Error, common lisp no soportado ~a~%" (string-upcase lisp-name)))

(defun err-unsupported-lisp-version-es (lisp-name lisp-version &optional (stream t))
  (format stream "Error, versión de common lisp no soportada ~a (~a)~%"
	  (string-upcase lisp-name) lisp-version))

(defun err-first-arg-not-a-command-es (first-arg &optional (stream t))
  (format stream "Error, el primer argumento '~a' no es un comando.~%" first-arg))

(defun err-invalid-command-es (cmd &optional (stream t))
  (format stream "Error, comando no válido ':~a'~%" cmd))

(defun err-num-options-es (cmd &optional (stream t))
  (format stream "Error, número de opciones incorrecto para el comando ':~a'~%" cmd))

(defun err-incorrect-option-es (cmd &optional (stream t))
  (format stream "Error, el comando ':~a' tiene una opción incorrecta.~%" cmd))

(defun err-repeated-command-es (cmd &optional (stream t))
  (format stream "Error, se ha repetido el comando ':~a'~%" cmd))

(defun err-too-many-commands-es (&optional (stream t))
  (format stream "Error ¡se han tecleado demasiados comandos!~%"))

(defun err-source-dir-does-not-exist-es (srcdir &optional (stream t))
  (format stream "Error, el directorio fuente no existe:~%  --> ~a~%" srcdir))

(defun err-target-dir-exists-es (dstdir &optional (stream t))
  (format stream "Error, el directorio de destino existe:~%  --> ~a~%" dstdir))

(defun err-no-native-cfg-es (&optional (stream t))
  (format stream "Error, no hay ninguna configuración nativa de 'emacs'.~%"))

(defun err-native-cfg-es (&optional (stream t))
  (format stream "Error, ya hay una configuración nativa.~%~%"))

;;; ******************** MENSAJES DE ADVERTENCIA
(defun warn-action-use-cfg-already-active-es (cfg &optional (stream t))
  (format stream "Aviso: La configuración ya está activada -> <~a>~%~%" cfg))

(defun warn-action-del-cfg-not-found-es (cfg &optional (stream t))
  (format stream "Aviso: no existe la configuración -> <~a>~%~%" cfg))

(defun warn-command-cancelled-es (cmd &optional (stream t))
  (format stream "Aviso: cancelado el comando -> :~a~%" cmd))

;;; ******************** MENSAJES DE INFORMACIÓN
;;; Message in response to the ':show' command when there is no configuration at all.
(defun info-action-show-no-cfg-es (&optional (stream t))
  
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil))
  
  ;;(format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
  (format stream "- INFO: Configuración activa            -> -----~%")
  (format stream "- INFO: Configuraciones almacenadas     -> ------~%")
  (terpri stream)
  
  (format stream "Posibles acciones:~%")
  (format stream "1) Crear una configuración nativa de'emacs'.~%~%"))

;;; Mensaje de respuesta al comando :show cuando hay una configuración activa
;;; y, al menos, otra alternativa que se pueda activar.
(defun info-action-show-active-alt-es (active-cfg available-cfgs &optional (stream t))
  (let ((other-cfgs (remove active-cfg available-cfgs :test #'string-equal)))
    
    (format stream "~a~%" (strinfo-version-es nil))
    (format stream "~a~%" (strinfo-copyright-es nil))
    (format stream "~a~%~%" (strinfo-license-es nil))
    
    ;;(format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
    (format stream "- INFO: Configuración activa        -> <~a>~%" active-cfg)
    (format stream "- INFO: Configuraciones almacenadas -> ~a~%" available-cfgs)
    (terpri stream)
    
    (format stream "Posibles acciones:~%")
    (format stream "1) Ejecutar 'emacs' con la configuración activa -> <~a>~%" active-cfg)
    (format stream "2) Activar otra configuración ")
    (format stream "--> myemacs :use <cfg>~%")
    (format stream "   Disponibles: ~a~%" other-cfgs)
    (format stream "3) Borrar una configuración almacenada ")
    (format stream "--> myemacs :del <cfg>~%")
    (format stream "   Disponibles: ~a~%" available-cfgs)
    (format stream "4) Copiar una configuración almacenada ")
    (format stream "--> myemacs :copy <org> <dst>~%")
    (format stream "   Disponibles para <org>: ~a~%" available-cfgs)
    (format stream "   A evitar para <dst>: ~a~%" available-cfgs)
    (format stream "5) Restaurar la configuración nativa ")
    (format stream "--> myemacs :restore-native <cfg>~%")
    (format stream "   Disponibles: ~a~%~%" available-cfgs)))

;;; Mensaje de respuesta al comando :show cuando hay una configuración activa
;;; y no hay ninguna otra alternativa que se pueda activar.
(defun info-action-show-active-noalt-es (active-cfg available-cfgs &optional (stream t))
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil))
  
  ;;(format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
  (format stream "- INFO: Configuración activa        -> <~a>~%" active-cfg)
  (format stream "- INFO: Configuraciones almacenadas -> ~a%" available-cfgs)
  (terpri stream)
  
  (format stream "Posibles acciones:~%")
  (format stream "1) Ejecutar 'emacs' con la configuración activa -> <~a>~%" active-cfg)
  (format stream "2) Copiar la configuración almacenada: ")
  (format stream "--> myemacs :copy <org> <dst>~%")
  (format stream "   Disponibles para <org>: ~a~%" available-cfgs)
  (format stream "   A evitar para <dst>: ~a~%" available-cfgs)
  (format stream "3) Borrar la configuración almacenada: ")
  (format stream "--> myemacs :del <cfg>~%")
  (format stream "   Disponibles: ~a~%~%" available-cfgs)
  (format stream "4) Restaurar la configuración nativa: ")
  (format stream "--> myemacs :restore-native <cfg>~%")
  (format stream "   Disponibles: ~a~%~%" available-cfgs))

;;; Mensaje de respuesta al comando ':show' cuando no hay ni configuración nativa, ni activa.
;;; Sólo hay configuraciones almacenadas.
(defun info-action-show-only-saved-cfgs-es (available-cfgs &optional (stream t))
  (format stream "~a~%" (strinfo-version-en nil))
  (format stream "~a~%" (strinfo-copyright-en nil))
  (format stream "~a~%~%" (strinfo-license-en nil))
  
  ;;(format stream "- INFO: Configuración nativa de 'emacs' -> NO ENCONTRADA~%")
  (format stream "- INFO: Configuración activa        -> ------~%")
  (format stream "- INFO: Configuraciones almacenadas -> ~a~%" available-cfgs)
  (terpri stream)
  
  (format stream "Posibles acciones:~%")
  (format stream "1) Activar una configuración: ")
  (format stream "--> myemacs :use <cfg>~%")
  (format stream "   Configuraciones disponibles: ~a~%" available-cfgs)
  (format stream "2) Restaurar la configuración nativa: ")
  (format stream "--> myemacs :restore-native <cfg>~%")
  (format stream "   Disponibles: ~a~%" available-cfgs)
  (format stream "3) Borrar una configuración almacenada: ")
  (format stream "--> 'myemacs :del <cfg>~%")
  (format stream "   Configuraciones disponibles: ~a~%" available-cfgs)
  (format stream "4) Copiar una configuración almacenada: ")
  (format stream "--> 'myemacs :copy <org> <dst>~%")
  (format stream "   Disponibles para <org>: ~a~%" available-cfgs)
  (format stream "   A evitar para <dst>: ~a~%" available-cfgs)
  (format stream "5) Usar 'emacs' para crear una configuración nativa.~%~%"))

;;; Mensaje de respuesta al comando :show cuando hay una configuración nativa de 'emacs'
;;; y alguna configuración almacenada.
(defun info-action-show-native-alt-es (available-cfgs &optional (stream t))
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil))
  
  ;;(format stream "- INFO: Configuración nativa de 'emacs' -> ENCONTRADA~%")
  (format stream "- INFO: Configuración               -> NATIVA~%")
  (format stream "- INFO: Configuraciones almacenadas -> ~a~%" available-cfgs)
  (terpri stream)
  
  (format stream "Posibles acciones:~%")
  (format stream "1) Ejecutar 'emacs' con la configuración nativa.~%")
  (format stream "2) Almacenar la configuración nativa: ")
  (format stream "--> myemacs :save-native-as <cfg>~%")
  (format stream "   Nombres a evitar: ~a~%" available-cfgs)
  (format stream "3) Borrar la configuración nativa: ")
  (format stream "--> myemacs :del-native~%")
  (format stream "4) Borrar una configuración almacenada: ")
  (format stream "--> myemacs :del <cfg>~%")
  (format stream "   Disponibles: ~a~%" available-cfgs)
  (format stream "5) Copiar una configuración almacenada: ")
  (format stream "--> myemacs :copy <org> <dst>~%")
  (format stream "   Disponibles para <org>: ~a~%" available-cfgs)
  (format stream "   A evitar para <dst>: ~a~%~%" available-cfgs))

;;; Mensaje de respuesta al comando :show cuando sólo hay una configuración nativa de 'emacs'.
(defun info-action-show-native-noalt-es (&optional (stream t))
  (format stream "~a~%" (strinfo-version-es nil))
  (format stream "~a~%" (strinfo-copyright-es nil))
  (format stream "~a~%~%" (strinfo-license-es nil))
  
  ;;(format stream "- INFO: Configuración nativa de 'emacs' -> ENCONTRADA~%")
  (format stream "- INFO: Configuración               -> NATIVA~%")
  (format stream "- INFO: Configuraciones almacenadas -> ------~%")
  (terpri stream)
  
  (format stream "Posibles acciones:~%")
  (format stream "1) Ejecutar 'emacs' usando la configuración nativa.~%")
  (format stream "2) Almacenar la configuración nativa: ")
  (format stream "--> myemacs :save-native-as <cfg>~%")
  ;;(format stream "   Nombres a evitar: ------~%")
  (format stream "3) Borrar la configuración nativa: ")
  (format stream "--> myemacs :del-native~%"))

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
    
  (format stream "USO: [ :help       || :version              || :show                  ||~%")
  (format stream "       :use <cfg>  || :del <cfg>            || :copy <org> <dst>      ||~%")
  (format stream "       :del-native || :save-native-as <cfg> || :restore-native <cfg>     ]~%")
  (format stream "     [ :lang <en || es> || :debug || :verbose ]~%")
  (format stream "~%")
  (format stream "(blanco)    -> Este mensaje.~%")
  (format stream ":help       -> Este mensaje.~%")
  (format stream ":version    -> Versión del programa.~%")
  (format stream ":show       -> Muestra configuraciones y posibles acciones a tomar.~%")
  (format stream ":use <cfg>  -> Activa la configuración <cfg>.~%")
  (format stream ":del <cfg>  -> Borra la configuración <cfg>.~%")
  (format stream ":copy <org> <dst> -> Copia la configuración almacenada de <org> a <dst>.~%")
  (format stream ":del-native  -> Borra la configuración nativa.~%")
  (format stream ":save-native-as <cfg> -> Guarda la configuración nativa de 'emacs' como <cfg>.~%")
  (format stream ":restore-native <cfg> -> Recupera la configuración <cfg> como nativa de 'emacs'.~%")
  (format stream ":lang < en || es > -> Muestra los mensajes en el idioma elegido.~%")
  (format stream ":debug      -> Muestra información de depuración.~%")
  (format stream ":verbose    -> Muestra más información al ejecutar algún comando (si procede).~%~%"))


