;;;; globals.lisp
;;;; Dynamic variables.
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

;;; 
(defparameter *progname* "myemacs")
(defparameter *version* "0.1.0")
(defparameter *production* "alpha-1")
(defparameter *version-date* "29-10-2020")

;;;
(defparameter *author* "José A. Navarro Ramón")
(defparameter *email*  "janr.devel@gmail.com")
(defparameter *year* 2020)
(defparameter *license* "BSD Clause-3")

;;;
;;; Default init emacs file
(defparameter *dotemacs-name-str* ".emacs")
;;; Init emacs file inside directory
(defparameter *init.el-name-str* "init.el")
;;; Emacs directory
(defparameter *emacsdir-name-str* ".emacs.d")
;;; Myemacs basic directory
(defparameter *myemacs-base-dir-name-str* ".myemacs.d")
;;; Regular expression to find saved configurations
(defparameter *emacsdir-star-str* (concatenate 'string *emacsdir-name-str* "-*"))

;;(defparameter *myemacs-base-dir-str* nil)

;;; Language global variables
(defparameter *default-language* :en)
(defparameter *user-language* nil)
(defparameter *supported-user-language* nil)
(defparameter *args-language* nil)
(defparameter *supported-args-language* nil)
(defparameter *language* *default-language*)

;;; Supported characteristics
(defparameter *supported-exec-modes* '(:repl :standalone :script))
(defparameter *supported-os-types* '(:unix))
;list of pairs (lisp-name minimum-version)
(defparameter *supported-lisps* '((:sbcl nil) (:ccl nil)))
(defparameter *supported-languages* `(,*default-language* :es))
(defparameter *valid-commands*
  `((:help = 0 nil) (:version = 0 nil) (:show = 0 nil)
    (:use = 1 nil) (:add = 1 nil) (:del = 1 nil)
    (:verbose = 0 nil) (:debug = 0 nil) (:lang = 1 ,*supported-languages*)))

;;;
;;; HASH-TABLE *data*
;;; 'homedir-str
;;; ******* EXEC-MODE
;;; 'exec-mode
;;; *** OS
;;; 'os-type
;;; 'os-type-name
;;; 'os
;;; ******* COMMON-LISP
;;; 'lisp-name
;;; 'lisp-version
;;; 'lisp-name-ok-p
;;; 'lisp-version-ok
;;; 'supported-lisp-p
;;; ******* NATIVE CFG
;;; 'native-emacsdir-str
;;; 'native-dotemacs-str
;;; 'native-init-str
;;; 'native-emacsdir
;;; 'native-emacsdir-p
;;; 'native-dotemacs
;;; 'native-init
;;; 'emacsdir-symlink
;;; 'native-cfg
;;; ******* SAVED CFGS
;;; 'myemacs-base-dir-str
;;; 'myemacs-base-dir
;;; 'possible-saved-cfg-dirs
;;; 'possible-init-files
;;; 'init-files
;;; 'saved-dirs
;;; 'saved-cfgs
;;; 'active-cfg
(defparameter *data* (make-hash-table))

