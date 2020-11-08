;;;; lang.lisp
;;;; Functions dealing with language choosing and language-aware messages.
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
;;; 'get-user-language-unix'
;;; Returns the user language in a UNIX OS, e.g.: ':es', ':en', ...
(defun get-user-language-unix ()
  (let* ((lang-env-var (uiop:getenv "LANG"))
	 (user-language-str (subseq lang-env-var 0 (position #\_ lang-env-var)))
	 (user-language (intern (string-upcase user-language-str) "KEYWORD")))
    user-language))

;;; 'get-user-language'
;;; Returns the user language in approved OS, e.g.: ':es', ':en', ...
(defun get-user-language ()
  (let ((user-language nil))
    (cond
      ((uiop:os-unix-p)
       (setf user-language (get-user-language-unix)))
      (t nil))
    (when user-language
      (setf *user-language* user-language))
    user-language))

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

(defun lang-aware-global-value (value-prefix)
  (symbol-value
   (alexandria:ensure-symbol
    (alexandria:symbolicate "*" value-prefix "-" *language* "*")
    :myemacs)))


;;; ********

;;; ********************* SERVICEABLE FUNCTIONS

;;; Macro to choose the right language-aware message function
;;; Parameters:
;;; 'fname': Symbol representing the prefix-part (non-language part) of the message function name
;;; 'param-list': List of parameters that are passed to the language-aware (laf) function.
;;; Returns:
;;; The result of appling the arguments in the 'param-list' to the language-aware (laf) function.
(defmacro msg ((fname &rest param-list))
  `(let ((laf (lang-aware-function ',fname)))
     (setf (fdefinition (alexandria:ensure-symbol ',fname :myemacs)) laf)
     (funcall laf ,@param-list)))

;;; Register possible languages from sources: 'default language' 'user language' and 'arguments language',
;;; and chooses the supported language in the following order (from least to most important ones):
;;;   - Default language (least important)
;;;   - User language from OS 
;;;   - Arguments passed to the program with the option :lang <language> (most important)
(defun find-and-register-language (standard-args)
  (let* ((language nil)
	 (user-language (get-user-language))
	 (user-language-ok (if (member user-language *supported-languages*) t nil))
	 (args-language (intern
			 (string-upcase (string (second (find-command :lang standard-args))))
			 "KEYWORD"))
	 (args-language-ok (if (member args-language *supported-languages*) t nil)))
    (setf *user-language* user-language)
    (setf *supported-user-language* user-language-ok)
    (setf *args-language* args-language)
    (setf *supported-args-language* args-language-ok)
    ;;(format t "(find-language) user-language -> ~a~%" user-language)
    ;;(format t "(find-language) user-language-ok -> ~a~%" user-language-ok)
    ;;(format t "(find-language) args-language -> ~a~%" args-language)
    ;;(format t "(find-language) args-language-ok -> ~a~%" args-language-ok)
    ;;(format t "--------------------------~%")
    ;;(format t "(find-language) *user-language* -> ~a~%" *user-language*)
    ;;(format t "(find-language) *supported-user-language* -> ~a~%" *supported-user-language*)
    ;;(format t "(find-language) *args-language* -> ~a~%" *args-language*)
    ;;(format t "(find-language) *supported-args-language* -> ~a~%" *supported-args-language*)
    (cond
      ((not (not args-language-ok))
       (setf language args-language))
      ((not (not user-language-ok))
       (setf language user-language))
      (t
       (setf language *default-language*)))
    ;;(format t "(find-language) FIN language -> ~a~%" language)
    (setf *language* language)
    ;;(format t "--------------------------~%")
    ;;(format t "(find-language) *language* -> ~a~%" *language*)
    language))

;;; ********
