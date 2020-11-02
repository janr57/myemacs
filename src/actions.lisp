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
(defun cfg-keyw-to-str (cfg-symb)
  (string-downcase cfg-symb))

(defun cfg-str-to-keyw (cfg-str)
  (intern (string-upcase cfg-str) "KEYWORD"))
  
;;; List of saved emacs's configuration directories found, as pathnames.
;;; Parameters:
;;; 'homedir-str': The user's home directory as a string.
;;; 'emacsdir-star-str': The starred emacsdir saved directory prefix, e.g.: '.emacs.d-*'
;;; Returns a list of saved emacs's configuration directories found, as pathnames.
(defun get-saved-cfg-dirs-in-path-unix (emacsdir-star-str &optional (path-str *myemacs-base-dir-str*))
(format t "(get-saved-cfg-dirs-in-path-unix) emacsdir-star-str -> ~a~%" emacsdir-star-str)
(format t "(get-saved-cfg-dirs-in-path-unix) path-str -> ~a~%" path-str)
(format t "(get-saved-cfg-dirs-in-path-unix) -> ~a~%" (get-directory-in-path-str-unix emacsdir-star-str :basepath path-str))
  (directory
   (get-directory-in-path-str-unix emacsdir-star-str :basepath path-str)))

;;(defun get-saved-cfg-dirs-in-path-unix (emacsdir-star-str &optional (path-str *myemacs-base-dir-str*))
;;;;  (format t "(get-saved-cfg-dirs-in-path-unix) emacsdir-star-str -> ~a~%" emacsdir-star-str)
;;;;  (format t "(get-saved-cfg-dirs-in-path-unix) path-str -> ~a~%" path-str)
;;  (directory
;;   (get-directory-in-path-str-unix emacsdir-star-str :basepath (namestring path-str))))

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
;;; Returns four values:
;;;  1) 'native-cfg': 'T' if a native configuration is found, otherwise 'NIL'.
;;;  2) 'native-emacsdir': If found, path of the native emacs directory, 'NIL' otherwise.
;;;  3) 'native-dotemacs': If found, path of this native emacs init file, 'NIL' otherwise.
;;;  4) 'native-init.el': If found, path of this native emacs init file, 'NIL' otherwise.
(defun look-for-and-register-native-cfg ()
  ;; Aqcuire values
  (let* ((native-emacsdir-str (get-directory-in-path-str-unix *emacsdir-name-str*))
	 (native-dotemacs-str (get-file-in-path-str-unix *dotemacs-name-str*))
	 (native-init-str (get-file-in-path-str-unix *init.el-name-str* native-emacsdir-str))
	 (native-emacsdir (probe-file native-emacsdir-str))
	 (native-emacsdir-p (uiop:pathname-equal native-emacsdir
						       (uiop:ensure-pathname native-emacsdir-str)))
	 (native-dotemacs (probe-file native-dotemacs-str))
	 (native-init (uiop:pathname-equal (probe-file native-init-str)
						    (uiop:ensure-pathname native-init-str)))
	 (emacsdir-symlink nil)
	 (native-cfg nil))
    ;; Consider changing value for 'emacsdir-symlink' and 'native-emacsdir':
    (when (not native-emacsdir-p)
      (setf emacsdir-symlink native-emacsdir)
      (setf native-emacsdir nil))
    ;; Detect if there is definitely a native emacs configuration:
    (when (and native-emacsdir
	       (or native-dotemacs
		   native-init))
      (setf native-cfg t))

    ;; Register values
    (setf (gethash 'homedir-str *data*) (uiop:getenv "HOME"))
    (setf (gethash 'native-emacsdir-str *data*) native-emacsdir-str)
    (setf (gethash 'native-dotemacs-str *data*) native-dotemacs-str)
    (setf (gethash 'native-init-str *data*) native-init-str)
    (setf (gethash 'native-emacsdir *data*) native-emacsdir)
    (setf (gethash 'native-emacsdir-p *data*) native-emacsdir-p)
    (setf (gethash 'native-dotemacs *data*) native-dotemacs)
    (setf (gethash 'native-init *data*) native-init)
    (setf (gethash 'emacsdir-symlink *data*) emacsdir-symlink)
    (setf (gethash 'native-cfg *data*) native-cfg)
    ;; Return some values
    (values native-cfg native-emacsdir native-dotemacs native-init)))

;;(defun find-or-create-myemacs-base-dir (myemacs-base-dir-str)
;;  ;;(let ((myemacs-base-dir (probe-file myemacs-base-dir-str)))
;;  (format t "(find-or-create-myemacs-base-dir) myemacs-base-dir-str -> ~a~%" myemacs-base-dir-str)
;;  (ensure-directories-exist (pathname myemacs-base-dir-str)))	 

;;; Finds and registers files and directories relevant to the saved 'myemacs' configuration.
;;; Returns three values:
;;;  1) 'saved-dirs': If found, path of the native emacs directory, 'NIL' otherwise.
;;;  2) 'saved-cfgs': If found, path of this native emacs init file,'NIL' otherwise.
;;;  3) 'active-cfg': If found, path of this native emacs init file, 'NIL' otherwise.
(defun look-for-and-register-saved-cfgs ()
  ;; Acquire values
  (let* ((myemacs-base-dir-str (get-directory-in-path-str-unix *myemacs-base-dir-name-str*))
	 ;;(myemacs-base-dir (probe-file myemacs-base-dir-str))
	 (myemacs-base-dir (ensure-directories-exist (pathname myemacs-base-dir-str)))
	 (emacsdir-symlink (gethash 'emacsdir-symlink *data*))
	 (possible-saved-cfg-dirs (get-saved-cfg-dirs-in-path-unix *emacsdir-star-str*
								   (namestring myemacs-base-dir)))
	 (possible-init-files (get-possible-init.el-file-list-unix possible-saved-cfg-dirs))
	 (init-files (remove-if-not #'probe-file possible-init-files))
	 (saved-dirs (mapcar #'directory-namestring init-files))
	 (saved-cfgs (mapcar #'get-emacs-cfg-name-unix saved-dirs))
	 (active-cfg (get-emacs-cfg-name-unix emacsdir-symlink)))

    (format t "(look-for-and-register-saved-cfgs) myemacs-base-dir-str -> ~a~%" myemacs-base-dir-str)
    (format t "(look-for-and-register-saved-cfgs) myemacs-base-dir -> ~a~%" myemacs-base-dir)
    (format t "(look-for-and-register-saved-cfgs) emacsdir-symlink -> ~a~%" emacsdir-symlink)
    (format t "(look-for-and-register-saved-cfgs) possible-saved-cfg-dirs -> ~a~%" possible-saved-cfg-dirs)
    (format t "(look-for-and-register-saved-cfgs) saved-dirs -> ~a~%" saved-dirs)
    (format t "(look-for-and-register-saved-cfgs) saved-cfgs -> ~a~%" saved-cfgs)
    (format t "(look-for-and-register-saved-cfgs) active-cfg -> ~a~%" active-cfg)
    
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
  (multiple-value-bind (native-cfg native-emacsdir native-dotemacs native-init)
      (look-for-and-register-native-cfg)
    (multiple-value-bind (saved-dirs saved-cfgs active-cfg)
	(look-for-and-register-saved-cfgs)
      (let ((native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 (native-dotemacs-str (gethash 'native-dotemacs-str *data*)))
      ;; Cleaning spurious native files/directory
      (cond
	;; A native .emacs.d dir is found but no native init files.
	;; Delete it if empty and recheck.
	((and (not native-dotemacs)
              (not native-init)
              (not active-cfg)
              native-emacsdir)
	 (progn
	   (uiop:delete-directory-tree native-emacsdir :validate t)
	   (setf native-emacsdir (probe-file native-emacsdir-str))
	   (setf (gethash 'native-emacsdir *data*) native-emacsdir)))
	;; An active cfg found (symbolic link .emacs.d dir) together with
	;; a native init file .emacs. Delete it and recheck.
	(active-cfg
	 (progn
	   (uiop:delete-file-if-exists native-dotemacs)
	   (setf native-dotemacs (probe-file native-dotemacs-str))
	   (setf (gethash 'native-dotemacs *data*) native-dotemacs))))))))

(defun show-cfg-unix ()
  (let ((homedir-str (gethash 'homedir-str *data*))
	(native-dotemacs-str (gethash 'native-dotemacs-str *data*))
	(native-init-str (gethash 'native-init-str *data*))
	(native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	(native-dotemacs (gethash 'native-dotemacs *data*))
	(native-init (gethash 'native-init *data*))
	(native-emacsdir (gethash 'native-emacsdir *data*))
	(native-cfg (gethash 'native-cfg *data*))
	(active-cfg (gethash 'active-cfg *data*))
	(saved-cfgs (gethash 'saved-cfgs *data*)))
    (cond
      ;; No configuration found
      ((and (not native-cfg)
	    (not active-cfg)
	    (not saved-cfgs))
       (msg (info-action-show-no-cfg)))
      ;; An active configuration and alternative saved configurations
      ((and active-cfg
	    (> (length saved-cfgs) 1))
       (msg (info-action-show-active-alt active-cfg saved-cfgs)))
      ;; An active configuration but no alternative saved configurations
      ((and active-cfg
	    (= (length saved-cfgs) 1))
       (msg (info-action-show-active-noalt active-cfg saved-cfgs)))
      ;; No configuration, but some saved ones
      ((and (not native-cfg)
	    (not active-cfg)
	    saved-cfgs)
       (msg (info-action-show-only-saved-cfgs saved-cfgs)))
      ;; A native configuration and some saved ones
      ((and native-cfg
	    saved-cfgs)
       (msg (info-action-show-native-alt saved-cfgs)))
      ;; A native configuration and no saved ones
      ((and native-cfg
	    (not saved-cfgs))
       (msg (info-action-show-native-noalt)))
      (t
       (format t "(show-cfg-unix) *** ERROR THE PROGRAM SHOULDN'T HAVE REACHED HERE!~%")))))

(defun use-cfg-unix (cfg)
  (let* ((native-dotemacs-str (gethash 'native-dotemacs-str *data*))
	 (native-init-str (gethash 'native-init-str *data*))
	 (native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 (native-dotemacs (gethash 'native-dotemacs *data*))
	 (native-init (gethash 'native-init *data*))
	 (native-emacsdir (gethash 'native-emacsdir *data*))
	 (myemacs-base-dir-str (gethash 'myemacs-base-dir-str *data*))
	 (myemacs-base-dir (gethash 'myemacs-base-dir *data*))
	 (native-cfg (gethash 'native-cfg *data*))
	 (active-cfg (gethash 'active-cfg *data*))
	 (saved-cfgs (gethash 'saved-cfgs *data*))
	 (saved-cfgs-keyw (mapcar #'cfg-str-to-keyw saved-cfgs))
	 (cfg-found-in-saved-cfgs (find cfg saved-cfgs-keyw))
	 (cfg-str (string-downcase cfg))
	 (active-cfg-keyw (cfg-str-to-keyw active-cfg))
	 ;;(directory-separator (string (uiop:directory-separator-for-host)))
	 (target-link-name (concatenate 'string *emacsdir-name-str* "-" cfg-str))
	 ;;(target-link-name (string-right-trim directory-separator target-link-name))
	 ;;(myemacs-base-dir-str (string-right-trim directory-separator myemacs-base-dir-str))
	 ;;(native-emacsdir-str (string-right-trim directory-separator native-emacsdir-str))
	 (target-link (get-directory-in-path-str-unix target-link-name
						      :basepath (rem-last-sep myemacs-base-dir-str)
						      :lastsep nil))
	 (changed-p nil))

;;    (format t "(use-cfg-unix) native-dotemacs-str -> ~a~%" native-dotemacs-str)
;;    (format t "(use-cfg-unix) native-init-str -> ~a~%" native-init-str)
;;    (format t "(use-cfg-unix) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(use-cfg-unix) native-dotemacs -> ~a~%" native-dotemacs-str)
;;    (format t "(use-cfg-unix) native-init -> ~a~%" native-init-str)
;;    (format t "(use-cfg-unix) native-emacsdir -> ~a~%" native-emacsdir-str)
;;    (format t "(use-cfg-unix) myemacs-base-dir-str -> ~a~%" myemacs-base-dir-str)
;;    (format t "(use-cfg-unix) myemacs-base-dir -> ~a~%" myemacs-base-dir)
;;    (format t "(use-cfg-unix) native-cfg -> ~a~%" native-cfg)
;;    (format t "(use-cfg-unix) active-cfg -> ~a~%" active-cfg)
;;    (format t "(use-cfg-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(use-cfg-unix) target-link-name -> ~a~%" target-link-name)
;;    (format t "(use-cfg-unix) target-link -> ~a~%" target-link)
    ;; Detect errors
    (cond
      ;; Error: A native emacs configuration exists
      (native-cfg
       (msg (err-action-use-native-cfg)))
      ;; Error: cfg does not exist
      ((null cfg-found-in-saved-cfgs)
       (msg (err-action-use-cfg-not-available  cfg)))
      ;; La configuración ya está activa
      ((eql cfg (cfg-str-to-keyw active-cfg))
       (msg (warn-action-use-cfg-already-active (cfg-keyw-to-str cfg))))
      ;; Si hay una configuración activa, hay que borrar el directorio de emacs
      (active-cfg
       ;;(format t "Tengo que borrar un enlace simbólico y crear otro.")))))
       (progn
	   (delete-file (rem-last-sep native-emacsdir-str))
	   (osicat:make-link (rem-last-sep native-emacsdir-str) :target target-link)
	   (setf changed-p t)))
      ((not active-cfg)
       (not native-cfg)
      (progn
	(osicat:make-link (rem-last-sep native-emacsdir-str) :target target-link)
	(setf changed-p t))))
    (when changed-p
      (get-and-register-cfg-unix)
      (show-cfg-unix)
      (setf changed nil))))


(defun show-cfg ()
  (cond
    ((uiop:os-unix-p)
     (get-and-register-cfg-unix)
     (show-cfg-unix))))

(defun use-cfg (cfg)
  (cond
    ((uiop:os-unix-p)
     (get-and-register-cfg-unix)
     (use-cfg-unix cfg))))

;;;
(defun action-help ()
  (msg (info-action-help)))

(defun action-show ()
  (show-cfg))
  
(defun action-version ()
  (msg (info-action-version)))

(defun action-use (cfg)
  (use-cfg cfg))

(defun action-del (cfg)
  (format t "(action-del) cfg -> ~a~%" cfg))

(defun action-add (cfg)
  (format t "(action-add) cfg -> ~a~%" cfg))

;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************

(defun action-delivery-center (lcmd)
  (let ((action (car lcmd))
	(cfg (second lcmd)))
  (cond
    ((null lcmd) (action-help))
    ((eql action :help) (action-help))
    ((eql action :show) (action-show))
    ((eql action :version) (action-version))
    ((eql action :use) (action-use cfg))
    ((eql action :del)  (action-del cfg))
    ((eql action :add) (action-add cfg)))))
