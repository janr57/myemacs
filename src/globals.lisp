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
(alexandria:define-constant +progname+ "myemacs" :test #'string=)
(alexandria:define-constant +version+ "0.1.2" :test #'string=)
(alexandria:define-constant +status+ "beta-1" :test #'string=)
(alexandria:define-constant +version-date+ "29-11-2020" :test #'string=)

;;;
(alexandria:define-constant +author+ "José A. Navarro Ramón" :test #'string=)
(alexandria:define-constant +email+  "janr.devel@gmail.com" :test #'string=)
(alexandria:define-constant +year+ 2020)
(alexandria:define-constant +license+ "BSD Clause-3" :test #'string=)

;;;
;;; Emacs directory
(alexandria:define-constant +emacs-dirname+ ".emacs.d" :test #'string=)
;;; Default init emacs file
(alexandria:define-constant +dotemacs-filename+ ".emacs" :test #'string=)
;;; Init emacs file inside directory
(alexandria:define-constant +init-filename+ "init.el" :test #'string=)
;;; Myemacs basic directory
(alexandria:define-constant +myemacs-dirname+ ".myemacs.d" :test #'string=)

(alexandria:define-constant +cfg-dirname+ (string-left-trim "." +emacs-dirname+) :test #'string=)
;;; Regular expression to find saved configurations
(alexandria:define-constant +myemacsdir-regexp+ (concatenate 'string +cfg-dirname+ "-*") :test #'string=)


;;; Language global variables
(defparameter *default-language* :en)
(defparameter *user-language* nil)
(defparameter *supported-user-language* nil)
(defparameter *args-language* nil)
(defparameter *supported-args-language* nil)
(defparameter *language* *default-language*)

(defparameter *debug-flag* nil)
(defparameter *verbose-flag* nil)

;;; Supported characteristics
(defparameter *supported-exec-modes* '(:repl :standalone :script))
(defparameter *supported-os-types* '(:unix))
;list of pairs (lisp-name minimum-version)
(defparameter *supported-lisps-repl* '((:sbcl nil) (:ccl nil) (:ecl nil)))
(defparameter *supported-lisps-standalone* '((:sbcl nil) (:ccl nil)))
(defparameter *supported-lisps-script* '((:sbcl nil) (:ccl nil)))
(defparameter *supported-languages* `(,*default-language* :es))
(defparameter *valid-commands*
  `((:help = 0 nil) (:version = 0 nil) (:show = 0 nil)
    (:use = 1 nil) (:del = 1 nil) (:copy = 2 nil)
    (:del-native = 0 nil) (:save-native-as = 1) (:restore-native = 1)
    (:lang = 1 ,*supported-languages*) (:debug = 0 nil) (:verbose = 0 nil)))


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
;;; 'myemacsdir-str
;;; 'myemacsdir
;;; 'saved-cfgs
;;; 'active-cfg
(defparameter *data* (make-hash-table :size 61))

