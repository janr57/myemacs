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
;;;; 3) Even if errors were found, the analysis tries to find a valid ':lang ??' command
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
;;; Search for the command in the argument list in its standard form (list of lists of keywords.)
;;; Returns:
;;; A list with the command and its option(s) or 'NIL' if was not found.
(defun find-command (command lstandard-args)
  (cond
    ((null lstandard-args) nil)
    ((eql (car (car lstandard-args)) command)
     (car lstandard-args))
    (t (find-command command (cdr lstandard-args)))))

;;; ********************* SERVICEABLE FUNCTIONS **************************

;;; ********


