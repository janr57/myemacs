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

;;; ******************** TEXT STRINGS
(defun strinfo-version-en (&optional (stream t))
  (format stream "~a v~a ~a (~a)" *progname* *version* *production* *version-date*))

(defun strinfo-copyright-en (&optional (stream t))
  (format stream "Copyright (C) ~d ~a <~a>" *year* *author* *email*))

(defun strinfo-license-en (&optional (stream t))
  (format stream "License ~a." *license*))


;;; ******************** ERROR MESSAGES
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
  (format stream "Error, incorrect options number for command ':~a'." cmd))

(defun err-incorrect-option-en (cmd &optional (stream t))
  (format stream "Error, incorrect option found for command ':~a'." cmd))

(defun err-repeated-command-en (cmd &optional (stream t))
  (format stream "Error, command ':~a' has been repeated." cmd))

(defun err-too-many-commands-en (&optional (stream t))
  (format stream "Error, too many commands have been typed!"))

;;; ******************** WARNING MESSAGES

;;; ******************** INFO MESSAGES
;;; Message in response to :show command when there is an active configuration
;;; and, at least, other alternative saved configuration.
(defun info-action-show-active-alt-en (active-config available-configs &optional (stream t))
  (let ((available-configs-str (join-strings-from-list available-configs))
	(other-configs-str (join-strings-from-list
			    (remove active-config available-configs :test #'string-equal))))
    (format stream "~a~%" (strinfo-version-en nil))
    (format stream "~a~%" (strinfo-copyright-en nil))
    (format stream "~a~%~%" (strinfo-license-en nil))
    (format stream "- INFO: 'emacs' native configuration -> NO ENCONTRADA~%")
    (format stream "- INFO: Saved configurations         -> ~a~%" available-configs-str)
    (format stream "- INFO: Active configuration         ->(~a)~%" active-config)
    (terpri stream)
    (format stream "Posible actions:~%")
    (format stream "1) Delete any saved configuration, whether active or not:")
    (format stream "    --> 'myemacs :del <cfg>~%")
    (format stream "   Available configurations: ~a~%" available-configs-str)
    (format stream "2) Copy a saved configuration, whether active or not:")
    (format stream "    --> 'myemacs :copy <orig> <dest>~%")
    (format stream "   Available configurations: ~a~%" available-configs-str)
    (format stream "4) Do not change anything and continue using this configuration.~%~%")))

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
  (format stream "USAGE: [ :help      || :version    || :show      ||~%")
  (format stream "         :use <cfg> || :save <cfg> || :del <cfg> || :copy <orig> <dest> ]~%")
  (format stream "       [ :debug || :verbose || :lang <en || es> ]~%")
  (format stream "~%")
  (format stream "(blank)     -> This message.~%")
  (format stream ":help       -> This message.~%")
  (format stream ":version    -> Program version.~%")
  (format stream ":show       -> Show  configurations and possible actions to take.~%")
  (format stream ":use <cfg>  -> Change active configuration to <cfg>.~%")
  (format stream ":save <cfg> -> Save default emacs config with the name <cfg>.~%")
  (format stream ":del <cfg>  -> Delete configuration with name <cfg>.~%")
  (format stream ":debug      -> Show debug info.~%")
  (format stream ":verbose    -> Show more information when running a command (if aplicable).~%") 
  (format stream ":lang < en || es > -> Show messages in the chosen language.~%~%"))

