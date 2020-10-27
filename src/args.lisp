;;;; args.lisp
;;;; Functions involving the lexical and syntactical analysis of arguments passed to the  program
;;;;
;;;; It is done by:
;;;; 1) Transformation of the argument list passed to the program to a standard form
;;;;    involving list of keyword lists:
;;;;    a) When run as a standalone or script program, the arguments consist of a list
;;;;       of strings, whith commands beginning with a colon symbol and the options of
;;;;       these commands, if any, without it.
;;;;    b) When run as a REPL expression, then the arguments are a list of symbols,
;;;;       where the commands are keywords and their options are, if any, plain symbols.
;;;; 2) A lexical analysys of the standard form of this argument list is performed.
;;;; 3) Even if errors were found, the analysis returns in order to let a function
;;;;    from 'lang.lisp' to search for a valid ':lang ??' command
;;;;    to switch from the user language found in the OS to this new one.
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
;;; Finds next keyword-command in the list.
;;; Parameters:
;;; 'args': A list of symbols (keyword and/or plain symbols).
;;; Returns:
;;; If a command is found, it returns the list from the already found commnad to the end.
;;; Otherwise it returns 'NIL'.
(defun find-keyw-command (args)
  (member-if #'keywordp args))

;;; Finds the next keyword-command in the list beyond the first element which must be
;;; another command.
;;; Parameters:
;;; 'args': A list of symbols (keywords and/or plain symbols) where the 1st element is a command.
;;; Returns:
;;; If a command is found, it returns the list from the already found command until the end.
;;; Otherwise, it returns 'NIL'.
(defun find-next-keyw-command (args)
  (find-keyw-command (cdr args)))

;;; Finds the list of nonkeyword-symbol-argument-values associated to the first element which
;;; must be a keyword-command.
;;; Parameters:
;;; 'args': List of symbols (keyword and/or plainsymbols) where the first element is
;;;         a keyword-command.
;;; Returns:
;;; A list consisting of all contiguous values associated to the keyword-command. Otherwise 'NIL'
(defun find-sym-argvalue-list-from-command (args)
  (loop for sym in (cdr args) until (keywordp sym) collect sym))

;;; Turns a list of symbols into a list of keywords.
;;; Parameters:
;;; 'args': A list of symbols.
;;; Returns:
;;; A list of keywords
(defun symbol-list-to-repl-list (args)
      (mapcar (lambda (arg) (intern (symbol-name arg) "KEYWORD")) args))

;;; Turns the simplified REPL argument (list of simbols) into a fake standard REPL list,
;;; consisting in a standard-like list of lists structure but with keyword-commands and
;;; plain-symbol-values in the list.
(defun args-from-repl-to-fake-standard (args)
  (cond
    ;; 1) End of list (nil)
    ((null args) nil)
    ;; 2)
    ;;   - 1st elt, command (keyword)
    ;;   - 2nd elt, end of list (nil) or another command (keyword)
    ((and (keywordp (car args))
	  (or (null (cdr args))
	      (keywordp (second args))))
     (cons (cons (car args) nil)
	   (args-from-repl-to-fake-standard (cdr args))))
    ;; 3)
    ;;   - 1st elt, command (keyword)
    ;;   - 2nd elt, neither command (keyword) nor end of list (nil), i.e.: a plain symbol (symbol)
    ((not (keywordp (second args)))
     (cons (cons (car args) (find-sym-argvalue-list-from-command args))
	   (args-from-repl-to-fake-standard (find-next-keyw-command args))))))

;;; Converts the fake standard list (keywords and/or plain symbols) into the standard
;;; list (all elements are keywords, not just symbols).
(defun args-from-fake-standard-to-standard (args)
  (cond
    ((null args) nil)
    ((null (car args))
     (args-from-fake-standard-to-standard (cdr args)))
    (t
     (cons (symbol-list-to-repl-list (car args))
           (args-from-fake-standard-to-standard (cdr args))))))

;;; Checks if the argument is a list of objects with a given property.
;;; Parameters:
;;; 'func-type-p': A predicate function to test whether an element has a certain property or not.
;;; 'x': Any S-expression.
;;; Returns 'T' if the elements of the list are of type 'func-type-p', 'NIL' otherwise.
;;; Use examples:
;;; (list-of-type-p #'keywordp lst), (list-of-type-p #'stringp lst), ...
(defun list-of-type-p (func-type-p x)
  (cond
    ((null x) t)
    ((atom x) nil)
    ((not (funcall func-type-p (car x))) nil)
    (t (list-of-type-p func-type-p (cdr x)))))

;;; Checks if the argument is a list of lists of elements with a given property.
;;; Parameters:
;;; 'func-type-p': A predicate function to test whether an element has a certain property or not.
;;; 'x': Any S-expression.
;;; Returns 'T' if the elements of the list of lists are of type 'func-type-p', 'NIL' otherwise.
;;; Use examples:
;;; (list-of-lists-of-type-p #'keywordp x), (list-of-lists-of-type-p #'numberp x), ...
(defun list-of-lists-of-type-p (func-type-p x)
  (cond
    ((null x) t)
    ((atom x) nil)
    ((not (list-of-type-p func-type-p (car x))) nil)
    (t (list-lists-of-type-p func-type-p (cdr x)))))

(defun first-arg-not-a-command-closure (first-arg)
  (let ((closure-func (lambda () (msg (err-first-arg-not-a-command first-arg nil)))))
    closure-func))

(defun args-in-standard-form (args)
  (values t nil args))

(defun args-in-repl-form (args)
  (cond
    ;; Empty list: it would be classified as a standard argument list.
    ((null args) args)
    ;; Detects a first argument which is not a keyword-command
    ((and (symbolp (car args))
	  (not (keywordp (car args))))
     (values nil (first-arg-not-a-command-closure (car args)) args))
    (t (args-from-fake-standard-to-standard
	(args-from-repl-to-fake-standard args)))))

;;; ********************* SERVICEABLE FUNCTIONS **************************
;;; Search for the command in the argument list in its standard form (list of lists of keywords.)
;;; Returns:
;;; A list with the command and its option(s) or 'NIL' if was not found.
(defun find-command (command standard-args)
  (cond
    ((null standard-args) nil)
    ((eql (car (car standard-args)) command)
     (car standard-args))
    (t (find-command command (cdr standard-args)))))

(defun standarize-and-register-args (args)
    (cond
      ((list-of-lists-of-type-p #'keywordp args)
       (args-in-standard-form args))
      ((list-of-type-p #'symbolp args)
       (args-in-repl-form args))))
      ;;((list-of-type-p #'stringp args)
      ;; (arglist-in-terminal-form args)))

;;; ********


