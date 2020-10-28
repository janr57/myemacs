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
;;; Error closure when found an invalid command.
;;; Parameters:
;;; 'cmd': Invalid command.
;;; Returns:
;;; Closure error message.
(defun invalid-command-closure (cmd)
  (let ((closure-func (lambda () (msg (err-invalid-command cmd nil)))))
    closure-func))

;;; Error closure when a command had an invalid number of options.
;;; Parameters:
;;; 'cmd': Command with an invalid number of options.
;;; Returns:
;;; Closure error message.
(defun incorrect-num-options-closure (cmd)
  (let ((closure-func (lambda () (msg (err-num-options cmd nil)))))
    closure-func))

;;; Error closure when a command had an invalid option.
;;; Parameters:
;;; 'cmd': Command with an invalid option.
;;; Returns:
;;; Closure error message.
(defun incorrect-option-closure (cmd)
  (let ((closure-func (lambda () (msg (err-incorrect-option cmd nil)))))
    closure-func))

;;; Detects whether a given command is correct or there is a problem with it.
;;; These problems may be:
;;;   - invalid command.
;;;   - incorrect number of options.
;;;   - an incorrect option.
;;; Parameters:
;;; 'lcmd': A list which consists of the command in the first place, followed
;;;         by any number of options for this command including zero options,
;;;         i.e.: (command option1 option2 ...).
;;;         Note: The standard argument list contains 'lcmd's elements.
;;; Returns two values:
;;;   1) 'T' if the command is syntactically correct, or 'NIL' if there is a problem.
;;;   2) 'NIL' if the command is correct, or a message error closure to be displayed later on.
(defun approve-command-list (lcmd)
  (let* ((cmd (car lcmd))
	 (options (cdr lcmd))
	 (num-options (length options))
	 (command-restriction-list (car (member cmd *valid-commands* :key #'car)))
	 (func (second command-restriction-list))
	 (num (third command-restriction-list))
	 (valid-options (fourth command-restriction-list)))
    (cond
      ((null command-restriction-list)
       (values nil (invalid-command-closure cmd)))
      ((not (funcall func num-options num))
       (values nil (incorrect-num-options-closure cmd)))
      ((and (not (null valid-options))
	    (not (loop for opt in options always (member opt valid-options))))
       (values nil (incorrect-option-closure cmd)))
      (t (values t nil)))))

(defun count-commands (largs-repl)
  (let ((h (make-hash-table)))
    (dolist (lcmd largs-repl)
      (cond
	((null (gethash (car lcmd) h))
	 (setf (gethash (car lcmd) h) 1))
	(t (incf (gethash (car lcmd) h)))))
    h))

(defun list-repeated-commands (largs-repl)
  (let ((h (count-commands largs-repl)))
    (loop for k being the hash-keys in h using (hash-value v) collect (list k v))))

;;; ********

;;; ********************* SERVICEABLE FUNCTIONS **************************
;;; Detects whether the standard argument list has syntactically correct commands or not
;;; Specifically, it can detect the following problems with commands:
;;;   - invalid command.
;;;   - incorrect number of options.
;;;   - an incorrect option.
;;; Parameters:
;;; 'standard-args': Argument list in standard form.
;;; Returns two values:
;;;   1) 'T' if the standard argument list is syntactically correct, 'NIL' if a command is
;;;      not syntactically correct.
;;;   2) 'NIL' if the standard argument list is correct, or a message error closure to be
;;;      displayed later on.
(defun approve-standard-args (standard-args)
  (dolist (lcmd standard-args)
    (multiple-value-bind (command-list-ok err-closure)
	(approve-command-list lcmd)
      (when (not command-list-ok)
	(return-from approve-standard-args
	  (values command-list-ok err-closure)))))
  (values t nil))




;;; ********


