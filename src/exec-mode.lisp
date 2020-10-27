;;;; exec-mode.lisp
;;;; Functions involving the execution mode of the program.
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
(defun dont-use-main-closure ()
  (let ((closure-func (lambda () (msg (err-do-not-use-main nil)))))
    closure-func))

(defun unsupported-exec-mode-closure (exec-mode)
  (let ((closure-func (lambda () (msg (err-unsupported-exec-mode exec-mode nil)))))
    closure-func))

;;; ********************* SERVICEABLE FUNCTIONS **************************
(defun register-and-approve-exec-mode (exec-mode)
  ;; Register EXEC-MODE in the *data* hash-table
  (setf (gethash 'exec-mode *data*) exec-mode)
  ;;
  (let ((supported-exec-mode (member exec-mode *supported-exec-modes*)))
    (cond
      ((null exec-mode)
       (values nil (dont-use-main-closure)))
      ((null supported-exec-mode)
       (values nil (exec-mode-unsupported-exec-mode-closure exec-mode)))
      (t (values t nil)))))

;;; ********

