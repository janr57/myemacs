;;;; syn-args.lisp
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
;;; *** CLOSURES ***

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

;;; Error closure when a command has been repeated in the argument list.
;;; Parameters:
;;; 'cmd': Repeated command.
;;; Returns:
;;; Closure error message.
(defun repeated-command-closure (cmd)
  (let ((closure-func (lambda () (msg (err-repeated-command cmd nil)))))
    closure-func))

;;; Error closure when the cleaned argument list (the :lang :debug and :verbose flags removed)
;;; has many commands.
;;; Returns:
;;; Closure error message.
(defun too-many-commands-closure ()
  (let ((closure-func (lambda () (msg (err-too-many-commands nil)))))
    closure-func))


;;; *** OTHER AUXILIARY FUNCTIONS

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

;;; Produce a hash-table whith the name of the commands as keys and
;;; their values are the number of repetitions in the argument list.
;;; Parameters:
;;; 'standard-args': Argument list in standard-form.
;;; Returns:
;;; Hash-table of the commands in the argument list as keys,
;;; and the number of their repetitions as values.
(defun count-commands-hash-table (standard-args)
  (let ((h (make-hash-table)))
    (dolist (lcmd standard-args)
      (cond
	((null (gethash (car lcmd) h))
	 (setf (gethash (car lcmd) h) 1))
	(t (incf (gethash (car lcmd) h)))))
    h))

;;; Produce a list of repeated commands along with the
;;; number of their invocations in the argument list.
;;; Parameters:
;;; 'standard-args': Argument list in standard-form.
;;; Returns:
;;; A list of pairs (command . number-of-invocations), where the number of invocations.
;;; is greater than one.
(defun repeated-commands (standard-args)
  (let ((h (count-commands-hash-table standard-args)))
    (loop for k being the hash-keys in h using (hash-value v) when (> v 1) collect (list k v))))

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

;;; Detects if there are repeated commands in the argument list and returns a message closure
;;; to be run later on
;;; 'standard-args': Argument list in standard form.
;;; Returns two values:
;;;   1) 'T' if the standard argument list has no repeated commands; otherwise 'NIL'.
;;;   2) 'NIL' if there are no repeated commands. Otherwise, a message error closure to be
;;;      displayed later on. 
(defun find-repeated-command (standard-args)
  (let ((repeated-command (caar (repeated-commands standard-args))))
    (cond
      ((not (null repeated-command))
       (values nil (repeated-command-closure repeated-command)))
      (t (values t nil)))))

;;; Finds the language and the presence of :debug and/or :verbose flags in the standard arg list.
;;; Parameters:
;;; 'standard-args': Argument list in standard form.
;;; Returns three values:
;;;  1) The name of the language, e.g.: :es.
;;;  2) The presence of the :debug flag.
;;;  3) The presence of the :verbose flag.
(defun find-global-commands (standard-args)
  (let ((language (find-and-register-language standard-args))
	(debug-flag (if (find-command :debug standard-args) t nil))
	(verbose-flag (if (find-command :verbose standard-args) t nil)))
    (values debug-flag verbose-flag language)))

;;; Removes the global-commands (:lang :debug :verbose) from the standard argument list.
;;; Parameters:
;;; 'standard-args': Argument list in standard form.
;;; Returns:
;;; The standard argument list without these global commands.
(defun remove-global-commands (standard-args)
  (remove-if #'(lambda (x) (or (eql x :lang)
			       (eql x :debug)
			       (eql x :verbose)))
	     standard-args :key #'car))

;;; Detects the presence of many commands
;;; Parameters:
;;; 'cleaned-standard-args': Standard argument list deprived of the global commands,
;;; :lang, :debug and :verbose.
;;; Returns two values:
;;;  1) 'T' if the number of commands is correct. 'NIL' otherwise.
;;;  2) 'NIL' if the number of commands is corect or an appropriate message error closure
;;;     to be displayed later on.
(defun approve-num-commands (cleaned-standard-args)
  (let ((num-commands (length cleaned-standard-args)))
    (if (not (<= num-commands 1))
	(values nil (too-many-commands-closure))
	(values t nil))))

;;; ********


