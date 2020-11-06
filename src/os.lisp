;;;; os.lisp
;;;; Functions involved in detecting and approving the operating system.
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

;;; ********************* AUXILIARY FUNCTIONS **************************
(defun unrecognized-os-type-closure ()
  (let ((closure-func (lambda () (msg (err-unrecognized-os-type nil)))))
    closure-func))

(defun unsupported-os-type-closure (os-type-name)
  (let ((closure-func (lambda () (msg (err-unsupported-os-type os-type-name nil)))))
    closure-func))

;;; ********************* SERVICEABLE FUNCTIONS **************************
;;; Gets the operating system.
;;; Returns the next values:
;;;   1) The OS type as a keyword.
;;;   2) The OS type name as a string.
;;;   3) The OS as a keyword.
(defun get-os ()
  (cond
    ((uiop:os-unix-p)
     (values :unix "Unix" (uiop:operating-system)))
    ((uiop:os-windows-p)
     (values :windows "Windows" (uiop:operating-system)))
    ((uiop:os-macosx-p)
     (values :macosx "MacOSX" (uiop:operating-system)))
    (t
     (values nil nil nil))))

(defun register-and-approve-os ()
  (multiple-value-bind (os-type os-type-name os) (get-os)
    ;; Register OS in the *data* hash-table
    (setf (gethash 'os-type *data*) os-type)
    (setf (gethash 'os-type-name *data*) os-type-name)
    (setf (gethash 'os *data*) os)
    ;;
    (let ((supported-os-type (member os-type *supported-os-types*)))
      (cond
	((null os-type)
	 (values nil (unrecognized-os-type-closure)))
	((not supported-os-type)
	 (values nil (unsupported-os-type-closure os-type-name)))
	(t (values t nil))))))



