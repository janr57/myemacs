;;;; lang.lisp
;;;; General purpose functions.
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

;;; ******************** ERROR MESSAGES
;;; ********

;;; ******************** WARNING MESSAGES
;;; ********

;;; ******************** INFO MESSAGES
;;; ********

;;; ********************* AUXILIARY FUNCTIONS **************************
;;; ********


;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************

;;; ********************
;;; MAIN FUNCTION TO GET MESSAGES IN VARIOUS LANGUAGES
;;; Gets the symbol or string of a prefix function name and appends to it a termination
;;; corresponding to the actual language used. This function must exist in the appropriate
;;; 'lang-<language>.lisp' file.
;;; Parameters:
;;; 'func-prefix': A virtual name of a message function (either a string or a symbol).
;;; Returns:
;;; The complete language-aware function of a message function.
;;; E.g.: func-prefix -> some-message-function -> (function some-message-function-<language>)
(defun lang-aware-function (func-prefix)
  (symbol-function 
   (alexandria:ensure-symbol
    (alexandria:symbolicate func-prefix "-" *language*)
    :myemacs)))

;;; MACRO which provides introspection to the
(defmacro defun-this (fname param-list &body body)
  `(setf (fdefinition (alexandria:ensure-symbol ',fname :myemacs))
         (lambda ,param-list
           (funcall (lambda (this-fn) ,@body) ',fname))))

(defmacro msg-lang (fname param-list &body body)
  `(funcall (lang-aware-function ,fname) ,param-list))




;;; ********
