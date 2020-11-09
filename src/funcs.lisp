;;;; funcs.lisp
;;;; General purpose functions.
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

;;; ******************** GENERAL PURPOSE FUNCTIONS
(defun remove-char-from-strlst (char lstr)
  "Elimina el caracter 'char' de cada una de las cadenas que forma la lista."
  (if (null lstr)
      nil
      (cons (remove char (car lstr))
	    (remove-char-from-strlst char (cdr lstr)))))

(defun subst-char-in-string (str old new)
  (do ((i 0 (1+ i)))
      ((= i (length str)) str)
    (when (eql (elt str i) old)
      (setf (elt str i) new))))

(defun repeat-string (string ntimes)
  (format nil "~v@{~A~:*~}" ntimes string))

;;; Converts a list of strings into a string with the elements separated by spaces.
;;; Parameters:
;;; 'lstr': List of strings.
;;; Returns:
;;; A string of the list elements separated by spaces.
(defun join-strings-from-list (lstr &optional (separation-str " "))
  (cond
    ((null lstr)
     (format nil ""))
    ((null (cdr lstr))
     (concatenate 'string (car lstr) ""))
    (t (concatenate 'string
		  (car lstr)
		  separation-str
		  (join-strings-from-list (cdr lstr) separation-str)))))

(defun rem-last-dirsep (dirstr)
  (let ((dirsep (string (uiop:directory-separator-for-host))))
    (string-right-trim dirsep dirstr)))

(defun add-last-dirsep (dirstr)
  (let ((dirsep (string (uiop:directory-separator-for-host))))
    (concatenate 'string dirstr dirsep)))

;;; Forms a directory, as a string, joining two strings:
;;; a base-path and the string directory name.
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string
;;; 'emacsdir-name-str': The name of the standard emacs directory as a string
;;; Returns:
;;; The complete emacs directory path as a string
(defun directory-str-unix (dir-name base-path-str &key (lastsep t))
  (let* ((dirsep (string (uiop:directory-separator-for-host)))
	 (end-dirsep (if lastsep dirsep "")))
    (concatenate 'string (string-right-trim dirsep base-path-str) dirsep dir-name end-dirsep)))

;;; Complete standard .emacs init emacs file path as a string
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string.
;;; 'dotemacs-name-str': The name of the .emacs init file.
;;; Returns:
;;; The complete standard .emacs init file path as a string.
(defun file-str-unix (file-name base-path-str)
  (let ((dirsep (string (uiop:directory-separator-for-host))))
    (concatenate 'string (string-right-trim dirsep base-path-str) dirsep file-name)))

(defun assoc-list-finder (assoc-list)
  (lambda (key)
    (let ((res (assoc key assoc-list :test #'string-equal)))
      (cond
	((not res)
	 (values nil nil))
	(t (values (car res) (cdr res)))))))

(defun prompt-read-yes-no (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (let ((answer (string-upcase (read-line *query-io*))))
    (multiple-value-bind (found-answer yes-or-no-result)
	(funcall (assoc-list-finder (lang-aware-global-value 'yes-no-assoc)) answer)
      (cond
	(found-answer yes-or-no-result)
	(t (prompt-read-yes-no prompt))))))

;;; ******************** COMMON MYEMACS FUNCTIONS
(defun keyw-to-cfg (cfg-symb)
  (string-downcase cfg-symb))

(defun cfg-to-keyw (cfg-str)
  (intern (string-upcase cfg-str) "KEYWORD"))

(defun cfgdir-str-from (txt &key (lastsep t))
  (directory-str-unix (concatenate 'string *cfgdir-name* "-" txt)
		      (gethash 'myemacsdir-str *data*) :lastsep lastsep))

(defun cfgdir-from (txt)
  (pathname (cfgdir-str-from txt)))

;;; Gets the name of the saved configuration given a saved configuration directory.
;;; Parameters:
;;; 'any-emacsdir': A saved configuration directory as string.
;;; Returns:
;;; The name of the saved configuration corresponding to the directory.
(defun get-emacs-cfg-name-unix (any-emacsdir)
  (when any-emacsdir
    (let* ((any-emacsdir-str (namestring any-emacsdir))
           (pos (position #\- any-emacsdir-str)))
      (when pos
        (string-right-trim
         (string (uiop:directory-separator-for-host))
         (subseq any-emacsdir-str (+ pos 1)))))))

(defun get-possible-saved-dirs (myemacsdir-str)
  (directory
   (directory-str-unix *myemacsdir-regexp* myemacsdir-str)))

(defun get-possible-saved-init (possible-saved-dirs)
  (mapcar #'(lambda (x) (file-str-unix *init-filename* (namestring x))) possible-saved-dirs))

(defun get-saved-cfgs (myemacsdir-str)
  (mapcar #'get-emacs-cfg-name-unix
	  (mapcar #'directory-namestring 
		  (remove-if-not #'probe-file (get-possible-saved-init
					       (get-possible-saved-dirs myemacsdir-str))))))

(defun get-native-init (native-init-str)
  ;;(format t "(get-native-init) native-init-str -> ~a~%" native-init-str)
  (let ((native-init (probe-file native-init-str)))
    (cond
      ((null native-init) nil)
      ((not (string-equal (namestring native-init) native-init-str))
       (setf native-init nil)))
    native-init))

(defun get-native-emacsdir-p (native-emacsdir native-emacsdir-str)
  ;;(format t "(get-native-emacsdir-p) native-emacsdir -> ~a~%" native-emacsdir)
  ;;(format t "(get-native-emacsdir-p) native-emacsdir-str -> ~a~%" native-emacsdir-str)
  (if (or (null native-emacsdir)
	  (not (string-equal (namestring native-emacsdir) native-emacsdir-str)))
      nil t))
    
     
