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

;;; ******************** QUESTIONS
(defun ask-delete-directory-tree-en (dir)
  (format nil "Delete directory-tree ~a? " dir))

(defun list-yes-en ()
  (list "YES" "Y"))

(defun list-no-en ()
  (list "NO" "N"))

;;; ******************** TEXT STRINGS
(defun strinfo-version-en (&optional (stream t))
  (format stream "~a v~a ~a (~a)" *progname* *version* *production* *version-date*))

(defun strinfo-copyright-en (&optional (stream t))
  (format stream "Copyright (C) ~d ~a <~a>" *year* *author* *email*))

(defun strinfo-license-en (&optional (stream t))
  (format stream "License ~a." *license*))

;;; ******************** ERROR MESSAGES
(defun err-action-use-native-cfg-en (&optional (stream t))
  (format stream "Error, cannot activate a saved configuration when a native one is present.~%~%"))

(defun err-action-use-cfg-not-available-en (cfg &optional (stream t))
  (format stream "Error, not an available configuration: <~a>~%~%" cfg))

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
  (format stream "Error, the first argument '~a' is not a command." first-arg))

(defun err-invalid-command-en (cmd &optional (stream t))
  (format stream "Error, invalid command :~a." cmd))

(defun err-num-options-en (cmd &optional (stream t))
  (format stream "Error, incorrect options number for command ':~a'" cmd))

(defun err-incorrect-option-en (cmd &optional (stream t))
  (format stream "Error, incorrect option found for command ':~a'" cmd))

(defun err-repeated-command-en (cmd &optional (stream t))
  (format stream "Error, command ':~a' has been repeated." cmd))

(defun err-too-many-commands-en (&optional (stream t))
  (format stream "Error, too many commands have been typed!"))

(defun err-source-dir-does-not-exist-en (srcdir &optional (stream t))
  (format stream "Error, source directory doesn't exist:~% --> ~a~%" srcdir))

(defun err-target-dir-exists-en (dstdir &optional (stream t))
  (format stream "Error, target directory exists:~% --> ~a~%" dstdir))

;;; ******************** WARNING MESSAGES
(defun warn-action-use-cfg-already-active-en (cfg &optional (stream t))
  (format stream "Warning, configuration already active -> ~a~%~%" cfg))

(defun warn-action-del-cfg-not-found-en (cfg &optional (stream t))
  (format stream "Warning, configuration not found -> ~a~%~%" cfg))

(defun warn-command-cancelled-en (cmd &optional (stream t))
  (format stream "Warning, command cancelled -> :~a~%" cmd))

;;; ******************** INFO MESSAGES
;;; Message in response to the ':show' command when there is no configuration at all.
(defun info-action-show-no-cfg-en (&optional (stream t))
  (format stream "~a~%" (strinfo-version-en nil))
  (format stream "~a~%" (strinfo-copyright-en nil))
  (format stream "~a~%~%" (strinfo-license-en nil))
  
  (format stream "- INFO: 'emacs' native configuration -> NOT FOUND~%")
  (format stream "- INFO: Active configuration         -> NOT FOUND~%")
  (format stream "- INFO: Saved configurations         -> NOT FOUND~%")
  (terpri stream)

  (format stream "Posible actions:~%")
  (format stream "1) Create a native 'emacs' configuration to save it later on.~%~%"))

;;; Message in response to the ':show' command when there is an active configuration
;;; and other alternative saved configurations.
(defun info-action-show-active-alt-en (active-cfg available-cfgs &optional (stream t))
  (let ((other-cfgs (remove active-cfg available-cfgs :test #'string-equal)))
    
    (format stream "~a~%" (strinfo-version-en nil))
    (format stream "~a~%" (strinfo-copyright-en nil))
    (format stream "~a~%~%" (strinfo-license-en nil))
 
    (format stream "- INFO: 'emacs' native configuration -> NOT FOUND~%")
    (format stream "- INFO: Active configuration         -><~a>~%" active-cfg)
    (format stream "- INFO: Saved configurations         -> ~a~%" available-cfgs)
    (terpri stream)
    
    (format stream "Possible actions:~%")
    (format stream "1) Run 'emacs' with the active configuration -> ~a~%" active-cfg)
    (format stream "2) Activate another configuration:~%")
    (format stream "   --> myemacs :use <cfg>~%")
    (format stream "   Available: ~a~%" other-cfgs)
    (format stream "3) Delete any saved configuration:~%")
    (format stream "   --> myemacs :del <cfg>~%")
    (format stream "   Available: ~a~%" available-cfgs)
    (format stream "4) Copy a saved configuration:~%")
    (format stream "   --> myemacs :copy <src> <dst>~%")
    (format stream "   Available: ~a~%~%" available-cfgs)))

;;; Message in response to the ':show' command when there is an active configuration
;;; and no other alternative saved configurations.
(defun info-action-show-active-noalt-en (active-cfg available-cfgs &optional (stream t))
  
    (format stream "~a~%" (strinfo-version-en nil))
    (format stream "~a~%" (strinfo-copyright-en nil))
    (format stream "~a~%~%" (strinfo-license-en nil))
    
    (format stream "- INFO: 'emacs' native configuration -> NO ENCONTRADA~%")
    (format stream "- INFO: Active configuration         -> NO ENCONTRADA~%")
    (format stream "- INFO: Saved configurations         -> ~a~%" available-cfgs)
    (terpri stream)
    
    (format stream "Posible actions:~%")
    (format stream "1) Run 'emacs' with the active configuration -> ~a~%" active-cfg)
    (format stream "2) Delete any saved configuration:")
    (format stream "    --> 'myemacs :del <cfg>~%")
    (format stream "   Available: ~a~%" available-cfgs)
    (format stream "3) Copy a saved configuration:")
    (format stream "    --> 'myemacs :copy <src> <dst>~%")
    (format stream "   Available: ~a~%~%" available-cfgs))


;;; Message in response to the ':show' command when there is no configuration,
;;; only alternative saved configurations.
(defun info-action-show-only-saved-cfgs-en (available-cfgs &optional (stream t))
  
    (format stream "~a~%" (strinfo-version-en nil))
    (format stream "~a~%" (strinfo-copyright-en nil))
    (format stream "~a~%~%" (strinfo-license-en nil))
    
    (format stream "- INFO: 'emacs' native configuration -> NOT FOUND~%")
    (format stream "- INFO: Active configuration         -> NOT FOUND~%")
    (format stream "- INFO: Saved configurations         -> ~a~%" available-cfgs)
    (terpri stream)
    
    (format stream "Posible actions:~%")
    (format stream "1) Activate a configuration:~%")
    (format stream "   --> myemacs :use <cfg>~%")
    (format stream "   Available configurations: ~a~%" available-cfgs)
    (format stream "2) Delete any saved configuration:~%")
    (format stream "    --> 'myemacs :del <cfg>~%")
    (format stream "   Available: ~a~%" available-cfgs)
    (format stream "3) Copy a saved configuration~%")
    (format stream "    --> 'myemacs :copy <src> <dst>~%")
    (format stream "   Available: ~a~%" available-cfgs)
    (format stream "4) Use 'emacs' and create a native configuration.~%~%"))

;;; Message in response to the ':show' command when there is a native configuration
;;; and other alternative saved configurations.
(defun info-action-show-native-alt-en (available-cfgs &optional (stream t))
    
    (format stream "~a~%" (strinfo-version-en nil))
    (format stream "~a~%" (strinfo-copyright-en nil))
    (format stream "~a~%~%" (strinfo-license-en nil))
    
    (format stream "- INFO: 'emacs' native configuration -> FOUND~%")
    (format stream "- INFO: Active configuration         -> NOT FOUND~%")
    (format stream "- INFO: Saved configurations         -> ~a~%" available-cfgs)
    (terpri stream)

    (format stream "Possible actions:~%")
    (format stream "1) Use 'emacs' with the native configuration.~%")
    (format stream "2) Delete the native configuration:~%")
    (format stream "   --> myemacs :del *~%")
    (format stream "3) Copy native configuration as <dest>:~%")
    (format stream "   --> myemacs :copy * <dst>~%")
    (format stream "   Names to avoid: ~a~%" available-cfgs)
    (format stream "4) Delete any saved configuration:~%")
    (format stream "   --> myemacs :del <cfg>~%")
    (format stream "   Available: ~a~%" available-cfgs)
    (format stream "5) Copy a saved configuration:~%")
    (format stream "   --> myemacs :copy <src> <dst>~%")
    (format stream "   Available: ~a~%~%" available-cfgs))

;;; Message in response to the ':show' command when there is only a native configuration.
(defun info-action-show-native-noalt-en (&optional (stream t))
  
  (format stream "~a~%" (strinfo-version-en nil))
  (format stream "~a~%" (strinfo-copyright-en nil))
  (format stream "~a~%~%" (strinfo-license-en nil))
  
  (format stream "- INFO: 'emacs' native configuration -> FOUND~%")
  (format stream "- INFO: Active configuration         -> NOT FOUND~%")
  (format stream "- INFO: Saved configurations         -> NOT FOUND~%")
  (terpri stream)
  
  (format stream "Possible actions:~%")
  (format stream "1) Use 'emacs' with the native configuration.~%")
  (format stream "2) Delete the native configuration:~%")
  (format stream "   --> myemacs :del *~%")
  (format stream "3) Copy native configuration as <dst>:~%")
  (format stream "   --> myemacs :copy * <dst>~%~%"))

;;; Message in response to :version command
(defun info-action-version-en (&optional (stream t))
  (format stream "~a~%" (strinfo-version-en nil))
  (format stream "~a~%" (strinfo-copyright-en nil))
  (format stream "~a~%~%" (strinfo-license-en nil)))

;;; Message in response to :help command
(defun info-action-help-en (&optional (stream t))
  (format stream "~a~%" (strinfo-version-en nil))
  (format stream "~a~%" (strinfo-copyright-en nil))
  (format stream "~a~%~%" (strinfo-license-en nil))
  (format stream "USAGE: [ :help       || :version              || :show                  ||~%")
  (format stream "         :use <cfg>  || :del <cfg>            || :copy <src> <dst>      || ~%")
  (format stream "         :del-native || :save-native-as <cfg> || :restore-native <cfg>     ]~%")
  (format stream "       [ :lang <en || es> || :debug || :verbose ]~%")
  (format stream "~%")
  (format stream "(blank)     -> This message.~%")
  (format stream ":help       -> This message.~%")
  (format stream ":version    -> Program version.~%")
  (format stream ":show       -> Show  configurations and possible actions to take.~%")
  (format stream ":use <cfg>  -> Change active configuration to <cfg>.~%")
  (format stream ":del <cfg>  -> Delete <cfg> configuration.~%")
  (format stream ":copy <src> <dst> -> Copy saved configuration from <src> to <dst>~%")
  (format stream ":del-native -> Delete native configuration.~%")
  (format stream ":save-native-as <cfg> -> Save native emacs config as <cfg>.~%")
  (format stream ":restore-native <cfg> -> Retrieve native emacs from config <cfg>.~%")
  (format stream ":lang < en || es > -> Show messages in the chosen language.~%")
  (format stream ":debug      -> Show debug info.~%")
  (format stream ":verbose    -> Show more information when running a command (if aplicable).~%~%"))

;;;;; Message in response to :help command
;;(defun info-action-help-en (&optional (stream t))
;;  (format stream "~a~%" (strinfo-version-en nil))
;;  (format stream "~a~%" (strinfo-copyright-en nil))
;;  (format stream "~a~%~%" (strinfo-license-en nil))
;;  (format stream "USAGE: [ :help      || :version    || :show      ||~%")
;;  (format stream "         :use <cfg> || :save-native-as <cfg> || :del <cfg> || :copy <src> <dst> ]~%")
;;  (format stream "       [ :debug || :verbose || :lang <en || es> ]~%")
;;  (format stream "~%")
;;  (format stream "(blank)     -> This message.~%")
;;  (format stream ":help       -> This message.~%")
;;  (format stream ":version    -> Program version.~%")
;;  (format stream ":show       -> Show  configurations and possible actions to take.~%")
;;  (format stream ":use <cfg>  -> Change active configuration to <cfg>.~%")
;;  (format stream ":save <cfg> -> Save native emacs config as <cfg>.~%")
;;  (format stream ":del <cfg>  -> Delete <cfg> configuration.~%")
;;  (format stream ":debug      -> Show debug info.~%")
;;  (format stream ":verbose    -> Show more information when running a command (if aplicable).~%") 
;;  (format stream ":lang < en || es > -> Show messages in the chosen language.~%~%"))

