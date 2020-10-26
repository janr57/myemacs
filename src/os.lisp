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

(defun approve-os ()
  (multiple-value-bind (os-type os-type-name os) (get-os)
    (setf (gethash 'os-type *data*) os-type)
    (setf (gethash 'os-type-name *data*) os-type-name)
    (setf (gethash 'os *data*) os)
    (let ((supported-os-type (member os-type *supported-os-types*)))
      (cond
	((null supported-os-type)
	 (values nil (msg (err-unsupported-os-type os-type-name nil))))
	(t (values t nil))))))

	 
