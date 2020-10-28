;;;; actions.lisp
;;;; Functions involving the syntactical analysis of arguments passed to the  program
;;;; and the actions associated with these arguments.
;;;;
;;;; The 'myemacs' program keeps track of different configurations of emacs.
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

;;; ********************* AUXILIARY FUNCTIONS **************************



;;(defun action-show ()
;;  (show-config))

(defun action-show ()
  (format t "(action-show)~%"))

;;(defun action-help ()
;;  (msg (info-action-help))

;;(defun action-help ()
;;  (show-help))

;;(defun action-help ()
;;  (format t "(action-help)~%"))

(defun action-help ()
  (msg (info-action-help)))
  
(defun action-version ()
  (msg (info-action-version)))

(defun action-use (option)
  (format t "(action-use) option -> ~a~%" option))

(defun action-del (option)
  (format t "(action-del) option -> ~a~%" option))

(defun action-add (option)
  (format t "(action-add) option -> ~a~%" option))


;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************

(defun action-delivery-center (lcmd)
  (let ((action (car lcmd))
	(option (second lcmd)))
  (cond
    ((null lcmd) (action-help))
    ((eql action :help) (action-help))
    ((eql action :show) (action-show))
    ((eql action :version) (action-version))
    ((eql action :use) (action-use option))
    ((eql action :del)  (action-del option))
    ((eql action :add) (action-add option)))))
