;;;; packages.lisp
;;;; Defines the package 'myemacs'.

;;;; The 'myemacs' program keeps track of different configurations of 'emacs'.
;;;; The user can change between them.
;;;;
;;;; Copyright (c) 2020 - José A. Navarro Ramón <josea.navarro1@gmail.com>
;;;; License: BSD 3-Clause

(defpackage myemacs
  (:use :cl)
  (:export :myemacs :myemacs-standalone :myemacs-script))


