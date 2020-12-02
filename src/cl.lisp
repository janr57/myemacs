;;;; cl.lisp
;;;; Functions involved in the recognition of the Common-Lisp used.
;;;;
;;;; The 'myemacs' program keeps track of different configurations of 'emacs'.
;;;; The user can change between them.
;;;;
;;;; Copyright(c) 2020 - José A. Navarro Ramón <janr.devel@gmail.com>
;;;; License: BSD 3-Clause

;;; Declaration specifier at compile time (unmark only one of them):
;;; a) In the development phase
(declaim (optimize (speed 0) (safety 3) (debug 3)))
;;; b) In the release phase
;;;(declaim (optimize (speed 3) (safety 3) (debug 0)))

(in-package :myemacs)

;;; ********************* AUXILIARY FUNCTIONS  **************************
;;; Checks if the argument is a string of digits (representing an integer.)
;;; Returns: 'T' if the argument is a string of digits; 'NIL' if it is not.
;;; Parameters:
;;; 'possible-digit-string' is a string we wish to check whether it is only made of digits or not.
(defun string-represents-an-integer-p (str)
  (loop for char across str always (digit-char-p char)))

;;; Produce a list of strings from a version string which may contain non digit characters
;;; This function is tested on the uiop:lisp-version-string result provided by CCL
;;; which in the tested release, it was "1.12-f98". In SBCL it was "2.0.9"
;;; Parameters:
;;; 'string': The string provided by 'uiop:lisp-version-string'.
;;; Returns:
;;; A list of three strings (in SBCL it was ("2" "0" "9"), while in CCL, ("1" "12" "f98")).
;;; ***** (CHECK THIS FUNCTION WHEN A NEW COMMON-LISP VERSION IS RELEASED) ****
(defun strlist-from-possible-dirty-version-string (string)
  (uiop:split-string string :max 3 :separator ".-"))

;;; Checks whether a string list is formed of digits-only strings or not.
;;; Parameters:
;;; 'lstr': A list of strings.
;;; Returns:
;;; 'T' if all the string elements represent integers, otherwise 'NIL'.
(defun string-digits-list-p (lstr)
  (cond
    ((null lstr) t)
    ((not (string-represents-an-integer-p (car lstr))) nil)
    (t (string-digits-list-p (cdr lstr)))))

;;; Produce a clean version list from a possible dirty version string list
;;; by deleting terms from the first one with a non-digit character,
;;; e.g: ("1" "12" "f98") -> (1 12)
;;; Parameters:
;;; A possible dirty version string.
;;; Returns a clean list of integers representing the common lisp version
(defun version-list-from-string-list (lstr)
  (cond
    ((null lstr) nil)
    ((not (string-represents-an-integer-p (car lstr))) nil)
    (t (cons (parse-integer (car lstr)) (version-list-from-string-list (cdr lstr))))))

;;; Gets a version string from the version string provided by uiop:version-string, e.g. "2.0.9"
;;; This is because CCL may append a non-digit string to the version-string, e.g. "1.12-f98"
;;; Returns:
;;; A complete version string, e.g.:  "2.0.9" or "1.12"
(defun extract-version-from-string (string)
  (cond
    ((null (uiop:parse-version string))
     (uiop:unparse-version
      (version-list-from-string-list
       (strlist-from-possible-dirty-version-string string))))
    (t string)))

(defun unsupported-lisp-closure (lisp-name)
  (let ((closure-func (lambda () (msg (err-unsupported-lisp lisp-name nil)))))
    closure-func))

(defun unsupported-lisp-version-closure (lisp-name lisp-version)
  (let ((closure-func (lambda () (msg (err-unsupported-lisp-version lisp-name lisp-version nil)))))
    closure-func))

;;; ********************* SERVICEABLE FUNCTIONS **************************
;;; Gets the argument list when 'myemacs' is run as a ':standalone' or ':script' execution modes.
;;; Returns: the arguments as a list of strings.
;;; Note: The argument list depends on the LISP used. Only :sbcl and :ccl are approved.
;;;       Other possible LISPs (not yet approved) and the command to get the argument list, are:
;;;        - LISPWORKS -> 'system:*line-arguments-list*'
;;;        - CMU -> 'extensions:command-line-words*'
;;;        - GCL -> ¡si::*command-args*'
;;;        - ECL -> ext:command-args
(defun get-lisp-arglist ()
  (or
   #+SBCL sb-ext:*posix-argv*
   #+CCL ccl:*command-line-argument-list*
   #+ECL ext:command-args
   nil))

;;; Gets COMMON LISP used.
;;; Returns:
;;;   1) Mame of the LISP used, as a keyword.
;;;   2) Version of this LISP program, as a string.
(defun get-lisp ()
  (values (uiop:implementation-type)
	  (extract-version-from-string (uiop:lisp-version-string))))

(defun lisp-name-ok-p (lisp-name)
  (let ((exec-mode (gethash 'exec-mode *data*)))
    (case exec-mode
      (:repl (member lisp-name *supported-lisps-repl* :key #'car))
      (:standalone (member lisp-name *supported-lisps-standalone* :key #'car))
       (:script (member lisp-name *supported-lisps-script* :key #'car)))))

;;; Returns the two values of the LISP program only if approved. Two 'NIL's if not.
;;; Arguments:
;;; 'name': Name of the LISP in keyword form, e.g.: :SBCL, :CCL, :ECL, ...
;;; 'version': The version as a string.
(defun approve-lisp (lisp-name lisp-version)
  (let* ((lisp-name-ok (lisp-name-ok-p lisp-name))
	 (lisp-version-ok (uiop:version<= (car (cdr (car lisp-name-ok))) lisp-version)))
    (when (not lisp-name-ok)
      (setf lisp-version-ok nil))
    (values lisp-name-ok lisp-version-ok)))

;;; Register COMMON LISP characteristics:
;;; name, version, approved-name-p, approved-version-p, supported-lisp-p in the *data* hastable.
;;; Returns:
;;; 
(defun register-and-approve-lisp ()
   (multiple-value-bind (lisp-name lisp-version) (get-lisp)
     (multiple-value-bind (lisp-name-ok lisp-version-ok) (approve-lisp lisp-name lisp-version)
       (let ((supported-lisp-p lisp-version-ok))
	 ;; Register LISP in the *data* hash-table
	 (setf (gethash 'lisp-name *data*) lisp-name)
	 (setf (gethash 'lisp-version *data*) lisp-version)
	 (setf (gethash 'lisp-name-ok-p *data*) lisp-name-ok)
	 (setf (gethash 'lisp-version-ok *data*) lisp-version-ok)
	 (setf (gethash 'supported-lisp-p *data*) supported-lisp-p)
	 ;;
	 (cond
	   ((not supported-lisp-p)
	    (values nil (unsupported-lisp-closure lisp-name)))
	   ((not lisp-version-ok)
	    (values nil (unsupported-lisp-version-closure lisp-name lisp-version)))
	   (t (values t nil)))))))

