;;;; lex-args.lisp
;;;; Functions involving the lexical analysis of arguments passed to the  program.
;;;;
;;;; When the lexical analysis is performed, the different argument lists depending
;;;; on the execution method are transformed into a so called, standard-argument-list.
;;;; This standard-argument-list is then passed to the syntactical analysis which
;;;; resides in other module.
;;;;
;;;; A little more detail:
;;;; 1) Transformation of the argument list passed to the program to a standard form
;;;;    involving list of keyword lists:
;;;;    a) When run as a standalone or script program, the arguments consist of a list
;;;;       of strings, whith commands beginning with a colon symbol and the options of
;;;;       these commands, if any, without it.
;;;;    b) When run as a REPL expression, then the arguments are a list of symbols,
;;;;       where the commands are keywords and their options are, if any, plain symbols.
;;;; 2) At the same time, a lexical analysis is performed.
;;;; 3) If errors were found, they are saved in order to be presented at the end of the
;;;;    process. So the analysis goes on in order to let a function
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

;;; ------------------------------------------------ 
;;; *** ARGS IN STRING FORM ***
;;; Auxiliary functions needed by 'myemacs-standalone' and 'myemacs-script'.

;;; Detects if the argument is a string-command.
;;; String-command arguments are strings beginning with a colon symbol ':',
;;; e.g.: ":save", ":help", ...
;;; Parameters:
;;; 'strarg': A string element.
;;; Returns:
;;; 'T' if the string-argument is a string-command. Otherwise 'NIL'.
(defun str-command-p (strarg)
  (cond
    ((or (null strarg)
         (not (stringp strarg))) nil)
    ((char-equal (char strarg 0) #\:) t)))

;;; Finds a string-command within the list.
;;; Parameters:
;;; 'srtargs': list of string-arguments.
;;; Returns:
;;; If found a string-command, returns a list from the newly found command to the end of the list.
;;; Otherwise 'NIL'.
(defun find-str-command (strargs)
  (member-if #'str-command-p strargs))

;;; Finds a string-command within the list beginning with the second element
;;; Parameters:
;;; 'srtargs': list of string-arguments, beginning preferably, with a string-command.
;;; Returns:
;;; If found a string-command, returns a list from the newly found command to the end of the list.
;;; Otherwise 'NIL'.
(defun find-next-str-command (strargs)
  (find-str-command (cdr strargs)))

;;; Finds the list of string-argument-values corresponding to the first element which
;;; must be a string-command.
;;; Parameters:
;;; 'strargs': A list of string arguments beginning, preferably with a string-command.
;;; Returns:
;;; A list consistin on all contiguous string-values associated to the string-command.
;;; 'NIL' otherwise.
(defun find-str-argvalue-list-from-command (strargs)
  (loop for str in (cdr strargs) until (str-command-p str) collect str))

;;; Turns a list of strings into a list of keywords
;;; Parameters:
;;; 'lstr': A list of strings
;;; Returns:
;;; A list of keywords
(defun string-list-to-standard-list (strargs)
  (mapcar (lambda (x) (intern x "KEYWORD"))
	  (mapcar #'string-upcase (remove-char-from-strlst #\: strargs))))

;;; Turns the simplified TERMINAL arguments (list of strings) into a string based
;;; fake-standard list, consisting in a standard-like list of lists structure but with
;;; string-colon-commands and string-values in the list.
(defun strargs-from-str-to-str-fake-standard (strargs)
  (cond
    ;; ;; 1) End of list (nil)
    ((null strargs) nil)
    ;; 2)
    ;;   - 1st elt, string-command (string beginning with a colon)
    ;;   - 2nd elt, end of list (nil) or another string-command .
    ((and (str-command-p (car strargs))
	  (or (null (cdr strargs))
	      (str-command-p (second strargs))))
     (cons (cons (car strargs) nil)
	   (strargs-from-str-to-str-fake-standard (cdr strargs))))
    ;; 3)
    ;;   - 1st elt, string-command (string beginning with a colon)
    ;;   - 2nd elt, neither string-command nor end of list (nil), i.e.: a plain string.
    ((not (str-command-p (second strargs)))
     (cons (cons (car strargs) (find-str-argvalue-list-from-command strargs))
	   (strargs-from-str-to-str-fake-standard (find-next-str-command strargs))))))

;;; Converts the string-based-fake-standard list (string-commands and/or plain string) into the standard
;;; list (all elements are keywords, not just symbols).
(defun strargs-from-str-fake-standard-to-standard (strargs)
  (cond
    ((null strargs) nil)
    ((null (car strargs))
     (strargs-from-str-fake-standard-to-standard (cdr strargs)))
    (t (cons (string-list-to-standard-list (car strargs))
             (strargs-from-str-fake-standard-to-standard (cdr strargs))))))

;;; ------------------------------------------------ 
;;; *** ARGS IN SYMBOL FORM ***
;;; Auxiliary functions needed by '(myemacs)' in the REPL.

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
    (t (values
	t nil (args-from-fake-standard-to-standard
	       (args-from-repl-to-fake-standard args))))))

(defun args-in-terminal-form (strargs)
  (cond
    ;; Empty list: it would be classified as a standard argument list.
    ((null strargs) strargs)
    ;; Detects a first argument which is not a keyword-command
    ((not (str-command-p (car strargs)))
     (values nil (first-arg-not-a-command-closure (car strargs)) strargs))
    (t (values
	t nil (strargs-from-str-fake-standard-to-standard
	       (strargs-from-str-to-str-fake-standard strargs))))))

;;(defun args-in-terminal-form (strargs)
;;  (cond
;;    ;; Empty list: it would be classified as a standard argument list.
;;    ((null strargs) strargs)
;;    ;; Detects a first argument which is not a keyword-command
;;    ((not (str-command-p (car strargs)))
;;     (values nil (first-arg-not-a-command-closure (car strargs)) strargs))
;;    (t (strargs-from-str-fake-standard-to-standard
;;	(strargs-from-str-to-str-fake-standard strargs)))))


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
       (args-in-repl-form args))
      ((list-of-type-p #'stringp args)
       (args-in-terminal-form args))))



;;; ********


