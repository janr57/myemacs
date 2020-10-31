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
;;; Complete standard emacs directory path as a string
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string
;;; 'emacsdir-name-str': The name of the standard emacs directory as a string
;;; Returns:
;;; The complete emacs directory path as a string
(defun get-directory-in-path-str-unix (dir-name-str &optional (path-str (uiop:getenv "HOME")))
  (let ((directory-separator (string (uiop:directory-separator-for-host))))
    (concatenate 'string (string path-str)
		 directory-separator
		 dir-name-str
		 directory-separator)))

;;; Complete standard .emacs init emacs file path as a string
;;; Parameters:
;;; 'homedir-str': The user's 'HOME' directory as a string.
;;; 'dotemacs-name-str': The name of the .emacs init file.
;;; Returns:
;;; The complete standard .emacs init file path as a string.
(defun get-file-in-path-str-unix (file-name-str &optional (path-str (uiop:getenv "HOME")))
  (let ((directory-separator (string (uiop:directory-separator-for-host))))
    (concatenate 'string (string-right-trim directory-separator path-str)
		 directory-separator
		 file-name-str)))
  
;;; List of saved emacs's configuration directories found, as pathnames.
;;; Parameters:
;;; 'homedir-str': The user's home directory as a string.
;;; 'emacsdir-star-str': The starred emacsdir saved directory prefix, e.g.: '.emacs.d-*'
;;; Returns a list of saved emacs's configuration directories found, as pathnames.
(defun get-saved-cfg-dirs-in-path-unix (emacsdir-star-str &optional (path-str *myemacs-base-dir-str*))
;;  (format t "(get-saved-cfg-dirs-in-path-unix) emacsdir-star-str -> ~a~%" emacsdir-star-str)
;;  (format t "(get-saved-cfg-dirs-in-path-unix) path-str -> ~a~%" path-str)
  (directory
   (get-directory-in-path-str-unix emacsdir-star-str (namestring path-str))))

;;; List of possible init files in saved directories as pathnames.
;;; Parameters:
;;; 'lsaved-cfgig-dirs': A list of saved config directories.
;;; Returns:
;;; The list of possible init.el files in the directories, as pathnames.
;;; They may actually exist or not.
(defun get-possible-init.el-file-list-unix (saved-cfg-dirs)
  (cond
    ((null saved-cfg-dirs) nil)
    (t (cons (get-init.el-file-unix (car saved-cfg-dirs))
	     (get-possible-init.el-file-list-unix (cdr saved-cfg-dirs))))))

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
(defun get-emacs-cfg-name-unix (any-emacsdir)
  (when any-emacsdir
    (let* ((any-emacsdir-str (namestring any-emacsdir))
           (pos (position #\- any-emacsdir-str)))
      (when pos
        (string-right-trim
         (string (uiop:directory-separator-for-host))
         (subseq any-emacsdir-str (+ pos 1)))))))

;;; Finds and registers files and directories relevant to the native 'emacs' configuration.
;;; Returns three values:
;;;  1) 'found-native-emacsdir': If found, path of the native emacs directory, 'NIL' otherwise.
;;;  2) 'found-native-dotemacs': If found, path of this native emacs init file, 'NIL' otherwise.
;;;  3) 'found-native-init.el': If found, path of this native emacs init file, 'NIL' otherwise.
(defun look-for-and-register-native-cfg ()
  ;; Aqcuire values
  (let* ((native-emacsdir-str (get-directory-in-path-str-unix *emacsdir-name-str*))
	 (native-dotemacs-str (get-file-in-path-str-unix *dotemacs-name-str*))
	 (native-init-str (get-file-in-path-str-unix *init.el-name-str* native-emacsdir-str))
	 (found-native-emacsdir (probe-file native-emacsdir-str))
	 (found-native-emacsdir-p (uiop:pathname-equal found-native-emacsdir
						       (uiop:ensure-pathname native-emacsdir-str)))
	 (found-native-dotemacs (probe-file native-dotemacs-str))
	 (found-native-init (uiop:pathname-equal (probe-file native-init-str)
						    (uiop:ensure-pathname native-init-str)))
	 (emacsdir-symlink nil))
    (when (not found-native-emacsdir-p)
      (setf emacsdir-symlink found-native-emacsdir)
      (setf found-native-emacsdir nil))
    ;; Register values
    (setf (gethash 'homedir-str *data*) (uiop:getenv "HOME"))
    (setf (gethash 'native-emacsdir-str *data*) native-emacsdir-str)
    (setf (gethash 'native-dotemacs-str *data*) native-dotemacs-str)
    (setf (gethash 'native-init-str *data*) native-init-str)
    (setf (gethash 'found-native-emacsdir *data*) found-native-emacsdir)
    (setf (gethash 'emacsdir-symlink *data*) emacsdir-symlink)
    (setf (gethash 'found-native-dotemacs *data*) found-native-dotemacs)
    (setf (gethash 'found-native-init *data*) found-native-init)
    ;; Return some values
    (values found-native-emacsdir found-native-dotemacs found-native-init)))

;;; Finds and registers files and directories relevant to the saved 'myemacs' configuration.
;;; Returns three values:
;;;  1) 'found-native-emacsdir': If found, path of the native emacs directory, 'NIL' otherwise.
;;;  2) 'found-native-dotemacs': If found, path of this native emacs init file,'NIL' otherwise.
;;;  3) 'found-native-init': If found, path of this native emacs init file, 'NIL' otherwise.
(defun look-for-and-register-saved-cfgs ()
  ;; Acquire values
  (let* ((myemacs-base-dir-str (get-directory-in-path-str-unix *myemacs-base-dir-name-str*))
	 (myemacs-base-dir (probe-file myemacs-base-dir-str))
	 (emacsdir-symlink (gethash 'emacsdir-symlink *data*))
	 (possible-saved-cfg-dirs (get-saved-cfg-dirs-in-path-unix *emacsdir-star-str*
								   myemacs-base-dir))
	 (possible-init-files (get-possible-init.el-file-list-unix possible-saved-cfg-dirs))
	 (init-files (remove-if-not #'probe-file possible-init-files))
	 (saved-dirs (mapcar #'directory-namestring init-files))
	 (saved-cfgs (mapcar #'get-emacs-cfg-name-unix saved-dirs))
	 (active-cfg (get-emacs-cfg-name-unix emacsdir-symlink)))
    ;; Register values
    (setf (gethash 'myemacs-base-dir-str *data*) myemacs-base-dir-str)
    (setf (gethash 'myemacs-base-dir *data*) myemacs-base-dir)
    ;;(setf (gethash 'emacsdir-symlink *data*) emacsdir-symlink)
    (setf (gethash 'possible-saved-cfg-dirs *data*) possible-saved-cfg-dirs)
    (setf (gethash 'possible-init-files *data*) possible-init-files)
    (setf (gethash 'init-files *data*) init-files)
    (setf (gethash 'saved-dirs *data*) saved-dirs)
    (setf (gethash 'saved-cfgs *data*) saved-cfgs)
    (setf (gethash 'active-cfg *data*) active-cfg)
    ;; Return some values
    (values saved-dirs saved-cfgs active-cfg)))

(defun get-and-register-cfg-unix ()
  (multiple-value-bind (native-emacsdir native-dotemacs native-init)
      (look-for-and-register-native-cfg)
    (multiple-value-bind (saved-dirs saved-cfgs active-cfg)
	(look-for-and-register-saved-cfgs)
    ;;; Cleaning spurious native files/directory
    (cond
      ;; A native .emacs.d dir is found but no native init files.
      ;; Delete it if empty and recheck.
      ((and (not native-dotemacs)
            (not native-init)
            (not active-cfg)
            native-emacsdir)
       (progn
	 (uiop:delete-empty-directory native-emacsdir)
	 (setf native-emacsdir (probe-file emacsdir-str))
	 (setf (gethash 'native-emacsdir *data*) native-emacsdir)))
      ;; An active cfg found (symbolic link .emacs.d dir) together with
      ;; a native init file .emacs. Delete it and recheck.
      ((and active-cfg
	    native-dotemacs)
       (progn
	 (uiop:delete-file-if-exists found-native-dotemacs)
	 (setf found-native-dotemacs (probe-file dotemacs-str))))))))

(defun show-cfg-unix ()
  (let ((homedir-str (gethash 'homedir-str *data*))
	(dotemacs-str (gethash 'dotemacs-str *data*))
	(init.el-str (gethash 'init.el-str *data*))
	(emacsdir-str (gethash 'emacsdir-str *data*))
	(found-native-dotemacs (gethash 'found-native-dotemacs *data*))
	(found-native-init (gethash 'found-native-init *data*))
	(found-emacsdir (gethash 'found-emacsdir *data*))
	(active-cfg (gethash 'active-cfg *data*))
	(saved-cfgs (gethash 'saved-cfgs *data*)))
	
	;;(format t "(show-cfg-unix) found-native-dotemacs -> ~a~%" found-native-dotemacs)
	;;(format t "(show-cfg-unix) found-native-init -> ~a~%" found-native-init)
	;;(format t "(show-cfg-unix) found-emacsdir -> ~a~%" found-emacsdir)
	;;(format t "(show-cfg-unix) active-cfg -> ~a~%" active-cfg)
	;;(format t "(show-cfg-unix) saved-cfgs -> ~a~%" saved-cfgs)
	
    (cond
      ;; No configuration found
      ((and (not found-native-dotemacs)
	    (not found-native-init)
	    (not found-emacsdir)
	    (not active-cfg)
	    (not saved-cfgs))
       (msg (info-action-show-no-cfg)))
      ;; An active configuration
      ((and found-emacsdir
	    active-cfg
	    (> (length saved-cfgs) 1))
       (msg (info-action-show-active-alt active-cfg saved-cfgs)))
      ((and found-emacsdir
	    active-cfg
	    (= (length saved-cfgs) 1))
       (msg (info-action-show-active-noalt active-cfg saved-cfgs)))
      ;; No configuration, but some saved ones
      ((and (not found-native-dotemacs)
	    (not found-emacsdir)
	    saved-cfgs)
       (msg (info-action-show-only-saved-cfgs saved-cfgs)))
      ;; A native configuration and some saved ones
      ((and (or found-native-dotemacs
		found-emacsdir)
	    saved-cfgs
	    (not active-cfg))
       (msg (info-action-show-native-alt saved-cfgs)))
      ;; A native configuration and no saved ones
      (t
       (format t "(show-cfg-unix) FOUND A NATIVE CONFIGURATION, AND NO SAVED ONES~%")))))

(defun show-cfg ()
  (cond
    ((uiop:os-unix-p)
     (get-and-register-cfg-unix)
     (show-cfg-unix))))

(defun use-cfg (option)
  (cond
    ((uiop:os-unix-p)
     (get-and-register-cfg-unix))))
     ;;(use-cfg-unix))))
  
;;;
(defun action-help ()
  (msg (info-action-help)))

(defun action-show ()
  (show-cfg))
  
(defun action-version ()
  (msg (info-action-version)))

(defun action-use (option)
  (use-cfg option))

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
