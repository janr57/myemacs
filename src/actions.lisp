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

;;; Complete standard emacs directory path as a string
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string
;;; 'emacsdir-name-str': The name of the standard emacs directory as a string
;;; Returns:
;;; The complete emacs directory path as a string
(defun get-emacsdir-str-unix (homedir-str emacsdir-name-str)
  (concatenate 'string homedir-str
	       (string (uiop:directory-separator-for-host))
	       emacsdir-name-str))

;;; Complete standard .emacs init emacs file path as a string
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string.
;;; 'dotemacs-name-str': The name of the .emacs init file.
;;; Returns:
;;; The complete standard .emacs init file path as a string.
(defun get-dotemacs-str-unix (homedir-str dotemacs-name-str)
  (concatenate 'string homedir-str
	       (string (uiop:directory-separator-for-host))
	       dotemacs-name-str))

;;; Complete init emacs file inside the standard emacs directory as a string
;;; Parameters:
;;; 'emacsdir-str': Emacs's standard directory as a string.
;;; 'init.el-name-str': Name of the init file inside emacs standard directory as a string.
;;; Returns:
;;; The complete init emacs file inside the standard emacs directory as a string.
(defun get-init.el-unix (emacsdir-str init.el-name-str)
  (concatenate 'string emacsdir-str
	       (string (uiop:directory-separator-for-host))
	       init.el-name-str))

;;; List of saved emacs's configuration directories found, as pathnames.
;;; Parameters:
;;; 'homedir-str': The user's home directory as a string.
;;; 'emacsdir-star-str': The starred emacsdir saved directory prefix, e.g.: '.emacs.d-*'
;;; Returns a list of saved emacs's configuration directories found, as pathnames.
(defun get-saved-config-dirs-unix (homedir-str emacsdir-star-str)
  (directory
   (get-emacsdir-str-unix homedir-str emacsdir-star-str)))

;;; List of possible init files in saved directories as pathnames.
;;; Parameters:
;;; 'lsaved-config-dirs': A list of saved config directories.
;;; Returns:
;;; The list of possible init.el files in the directories, as pathnames.
;;; They may actually exist or not.
(defun get-possible-init.el-file-list-unix (lsaved-config-dirs)
  (cond
    ((null lsaved-config-dirs) nil)
    (t (cons (get-init.el-file-unix (car lsaved-config-dirs))
	     (get-possible-init.el-file-list-unix (cdr lsaved-config-dirs))))))

;;; Forms the complete 'init.el' file that may exist or not in the directory, as a pathname.
;;; Parameters:
;;; 'any-emacsdir': A directory which has been saved as a configuration file.
;;; Returns:
;;; Complete 'init.el' path of the 'init.el' in the directory, as a pathname.
(defun get-init.el-file-unix (any-emacsdir)
  (when any-emacsdir
    (let* ((initfile (pathname *init.el-name-str*))
           (initfile-name (pathname-name initfile))
           (initfile-type (pathname-type initfile))
           (dir (pathname-directory any-emacsdir)))
      (make-pathname
       :directory dir
       :name initfile-name
       :type initfile-type))))

;;; Gets the name of the saved configuration given a saved configuration directory.
;;; Parameters:
;;; 'any-emacsdir': A saved configuration directory as string.
;;; Returns:
;;; The name of the saved configuration corresponding to the directory.
(defun get-emacs-conf-name-unix (any-emacsdir)
  (when any-emacsdir
    (let* ((any-emacsdir-str (namestring any-emacsdir))
           (pos (position #\- any-emacsdir-str)))
      (when pos
        (string-right-trim
         (string (uiop:directory-separator-for-host))
         (subseq any-emacsdir-str (+ pos 1)))))))

(defun get-and-register-config-unix ()
  (let* ((homedir-str (uiop:getenv "HOME"))
	 (emacsdir-str (get-emacsdir-str-unix homedir-str *emacsdir-name-str*))
	 (dotemacs-str (get-dotemacs-str-unix homedir-str *dotemacs-name-str*))
	 (init.el-str (get-init.el-unix emacsdir-str *init.el-name-str*))
	 (possible-saved-config-dirs (get-saved-config-dirs-unix homedir-str *emacsdir-star-str*))
	 (possible-init-files (get-possible-init.el-file-list-unix possible-saved-config-dirs))
	 (found-init-files (remove-if-not #'probe-file possible-init-files))
	 (saved-dirs (mapcar #'directory-namestring found-init-files))
	 (conf-names (mapcar #'get-emacs-conf-name-unix saved-dirs))
	 (found-default-dotemacs (probe-file dotemacs-str))
	 (found-emacsdir (probe-file emacsdir-str))
	 (found-default-init.el (probe-file init.el-str))
	 	 (active-config (get-emacs-conf-name-unix found-emacsdir)))
    
;;    (format t "(get-and-register-config-unix) homedir-str -> ~a~%" homedir-str)
;;    (format t "(get-and-register-config-unix) emacsdir-str -> ~a~%" emacsdir-str)
;;    (format t "(get-and-register-config-unix) dotemacs-str -> ~a~%" dotemacs-str)
;;    (format t "(get-and-register-config-unix) init.el-str -> ~a~%" init.el-str)
;;    (format t "(get-and-register-config-unix) possible-saved-config-dirs -> ~a~%" possible-saved-config-dirs)
;;    (format t "(get-and-register-config-unix) possible-init-files -> ~a~%" possible-init-files)
;;    (format t "(get-and-register-config-unix) found-init-files -> ~a~%" found-init-files)
;;    (format t "(get-and-register-config-unix) saved-dirs -> ~a~%" saved-dirs)
;;    (format t "(get-and-register-config-unix) conf-names -> ~a~%" conf-names)
;;    (format t "(get-and-register-config-unix) found-default-dotemacs -> ~a~%" found-default-dotemacs)
;;    (format t "(get-and-register-config-unix) found-emacsdir -> ~a~%" found-emacsdir)
;;    (format t "(get-and-register-config-unix) found-init.el -> ~a~%" found-default-init.el)
;;    (format t "(get-and-register-config-unix) active-config -> ~a~%" active-config)
    
    (setf (gethash 'homedir-str *data*) homedir-str)
    (setf (gethash 'emacsdir-str *data*) emacsdir-str)
    (setf (gethash 'dotemacs-str *data*) dotemacs-str)
    (setf (gethash 'init.el-str *data*) init.el-str)
    (setf (gethash 'possible-saved-config-dirs *data*) possible-saved-config-dirs)
    (setf (gethash 'possible-init-files *data*) possible-init-files)
    (setf (gethash 'found-init-files *data*) found-init-files)
    (setf (gethash 'saved-dirs *data*) saved-dirs)
    (setf (gethash 'conf-names *data*) conf-names)
    (setf (gethash 'found-default-dotemacs *data*) found-default-dotemacs)
    (setf (gethash 'found-emacsdir *data*) found-emacsdir)
    (setf (gethash 'found-default-init.el *data*) found-default-init.el)
    (setf (gethash 'active-config *data*) active-config)))

(defun action-show-no-conf ()
  (msg (info-action-show-no-conf)))

(defun action-show-active-alt (active-config available-configs found-default-dotemacs)
  (when found-default-dotemacs
    (delete-dotemacs-unix dotemacs-str))
  (msg (info-action-show-active-alt active-config available-configs)))

(defun action-show-active-noalt (active-config available-configs found-default-dotemacs)
  (when found-default-dotemacs
    (delete-dotemacs-unix dotemacs-str))
  (msg (info-action-show-active-noalt active-config available-configs)))
  
  (defun action-show-only-saved-confs (available-configs)
  (msg (info-action-show-only-saved-confs available-configs)))

(defun show-config-unix ()
  (let ((homedir-str (gethash 'homedir-str *data*))
	(dotemacs-str (gethash 'dotemacs-str *data*))
	(init.el-str (gethash 'init.el-str *data*))
	(emacsdir-str (gethash 'emacsdir-str *data*))
	(found-default-dotemacs (gethash 'found-default-dotemacs *data*))
	(found-default-init.el (gethash 'found-default-init.el *data*))
	(found-emacsdir (gethash 'found-emacsdir *data*))
	(active-config (gethash 'active-config *data*))
	(conf-names (gethash 'conf-names *data*)))
	
	;;(format t "(show-config-unix) found-default-dotemacs -> ~a~%" found-default-dotemacs)
	;;(format t "(show-config-unix) found-default-init.el -> ~a~%" found-default-init.el)
	;;(format t "(show-config-unix) found-emacsdir -> ~a~%" found-emacsdir)
	;;(format t "(show-config-unix) active-config -> ~a~%" active-config)
	;;(format t "(show-config-unix) conf-names -> ~a~%" conf-names)
	
    (cond
      ;; No configuration found
      ((and (not found-default-dotemacs)
	    (not found-default-init.el)
	    (not found-emacsdir)
	    (not active-config)
	    (not conf-names))
       (action-show-no-config))
      ;; An active configuration
      ((and found-emacsdir
	    active-config
	    (> (length conf-names) 1))
       (action-show-active-alt active-config conf-names found-default-dotemacs))
      ((and found-emacsdir
	    active-config
	    (= (length conf-names) 1))
       (action-show-active-noalt active-config conf-names found-default-dotemacs))
      ;; No configuration, but some saved ones
      ((and (not found-default-dotemacs)
	    (not found-emacsdir)
	    conf-names)
       (action-show-only-saved-confs conf-names)))
;;      ;; A default configuration and some saved ones
;;      ((and (or found-default-dotemacs
;;		found-emacsdir)
;;	    lconf-names
;;	    (not active-config))
;;       (format t "(show-config-unix) FOUND A DEFAULT CONFIGURATION AND SOME SAVED ONES~%"))
;;      ;; A default configuration and no saved ones
;;      (t
;;       (format t "(show-config-unix) FOUND A DEFAULT CONFIGURATION, BUT FOUND SOME SAVED ONES~%"))
;;  (format t "(show-config-unix) ...~%"))))
))


(defun show-config ()
  (cond
    ((uiop:os-unix-p)
     (get-and-register-config-unix)
     (show-config-unix)
     )))




;;;

(defun action-help ()
  (msg (info-action-help)))

(defun action-show ()
  (show-config))
  
(defun action-version ()
  (msg (info-action-version)))

(defun action-use (option)
  (format t "(action-use) option -> ~a~%" option))

(defun action-del (option)
  (format t "(action-del) option -> ~a~%" option))

(defun action-add (option)
  (format t "(action-add) option -> ~a~%" option))

;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************



(defun action-delivery-center (lcmd)
  (let ((action (car lcmd))
	(option (second lcmd)))
  (cond
    ((null lcmd) (action-help))
    ((eql action :help) (action-help))
    ((eql action :show) (action-show))
    ((eql action :version) (action-version))
    ((eql action :use) (action-use option))
    ((eql action :del)  (action-del option))
    ((eql action :add) (action-add option)))))
