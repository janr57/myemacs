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

(defun rem-last-sep (dirstr)
  (let ((dirsep (string (uiop:directory-separator-for-host))))
    (string-right-trim dirsep dirstr)))

(defun add-last-sep (dirstr)
  (let ((dirsep (string (uiop:directory-separator-for-host))))
    (concatenate 'string dirstr dirsep)))

;;; Forms a directory, as a string, joining two strings:
;;; a base-path and the string directory name.
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string
;;; 'emacsdir-name-str': The name of the standard emacs directory as a string
;;; Returns:
;;; The complete emacs directory path as a string
(defun get-directory-in-path-str-unix (dir-name-str &key (basepath (uiop:getenv "HOME"))
						      (lastsep t))
  (let* ((dirsep (string (uiop:directory-separator-for-host)))
	 (end-dirsep (if lastsep dirsep "")))
    (concatenate 'string basepath dirsep dir-name-str end-dirsep)))

;;; Complete standard .emacs init emacs file path as a string
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string.
;;; 'dotemacs-name-str': The name of the .emacs init file.
;;; Returns:
;;; The complete standard .emacs init file path as a string.
(defun get-file-in-path-str-unix (file-name-str &optional (path-str (uiop:getenv "HOME")))
  (let ((dirsep (string (uiop:directory-separator-for-host))))
    (concatenate 'string (string-right-trim dirsep path-str) dirsep file-name-str)))

(defun prompt-read-yes-no (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (let ((answer (string-upcase (read-line *query-io*))))
    (cond
      ((find answer (msg (list-yes)) :test #'string-equal)
       t)
      ((find answer (msg (list-no)) :test #'string-equal)
       nil)
      (t (prompt-read-yes-no prompt)))))

;;(defun prompt-read-yes-no (prompt yes no)
;;  (format *query-io* "~a: " prompt)
;;  (force-output *query-io*)
;;  (let ((answer (string-upcase (read-line *query-io*))))
;;    (cond
;;      ((find answer '("YES" "Y" "SI" "SÍ" "S") :test #'string-equal)
;;       t)
;;      ((find answer '("NO" "N") :test #'string-equal)
;;       nil)
;;      (t (prompt-read prompt)))))



;;; ******************** COMMON MYEMACS FUNCTIONS

