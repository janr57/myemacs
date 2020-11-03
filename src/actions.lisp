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
;;(defun get-saved-cfg-dirs-in-path-unix (emacsdir-star-str &optional (path-str *myemacs-base-dir-str*))
;;;;  (format t "(get-saved-cfg-dirs-in-path-unix) emacsdir-star-str -> ~a~%" emacsdir-star-str)
;;;;  (format t "(get-saved-cfg-dirs-in-path-unix) path-str -> ~a~%" path-str)
;;  (directory
;;   (get-directory-in-path-str-unix emacsdir-star-str :basepath path-str)))

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
;;(defun get-possible-init.el-file-list-unix (saved-cfg-dirs)
;;  (cond
;;    ((null saved-cfg-dirs) nil)
;;    (t (cons (get-init.el-file-unix (car saved-cfg-dirs))
;;	     (get-possible-init.el-file-list-unix (cdr saved-cfg-dirs))))))

;;; Forms the complete 'init.el' file that may exist or not in the directory, as a pathname.
;;; Parameters:
;;; 'any-emacsdir': A directory which has been saved as a configuration file.
;;; Returns:
;;; Complete 'init.el' path of the 'init.el' in the directory, as a pathname.
;;(defun get-init.el-file-unix (any-cfg-emacsdir)
;;  (when any-cfg-emacsdir
;;    (let* ((initfile (pathname *init-filename*))
;;           (initfile-name (pathname-name initfile))
;;           (initfile-type (pathname-type initfile))
;;           (dir (pathname-directory any-cfg emacsdir)))
;;      (make-pathname
;;       :directory dir
;;       :name initfile-name
;;       :type initfile-type))))

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
   (directory-str-unix *emacsdir-regexp* myemacsdir-str)))

(defun get-possible-saved-init (possible-saved-dirs)
  (mapcar #'(lambda (x) (file-str-unix *init-filename* (namestring x))) possible-saved-dirs))

(defun get-saved-cfgs (myemacsdir-str)
  (mapcar #'get-emacs-cfg-name-unix
	  (mapcar #'directory-namestring 
		  (remove-if-not #'probe-file (get-possible-saved-init
					       (get-possible-saved-dirs myemacsdir-str))))))

;;; Finds and registers files and directories relevant to the native 'emacs' configuration.
;;; Returns four values:
;;;  1) 'native-cfg': 'T' if a native configuration is found, otherwise 'NIL'.
;;;  2) 'native-emacsdir': If found, path of the native emacs directory, 'NIL' otherwise.
;;;  3) 'native-dotemacs': If found, path of this native emacs init file, 'NIL' otherwise.
;;;  4) 'native-init.el': If found, path of this native emacs init file, 'NIL' otherwise.
;;; Note:
;;; 'probe-file' produces a pathname if the file exists, 'NIL' otherwise.
;;; 'uiop:ensure-pathname' produces a pathname even if the file doesn't exist.
(defun native-cfg-unix ()
  ;; Aqcuire values
  (let* ((homedir-str (add-last-sep (uiop:getenv "HOME")))
	 (native-emacsdir-str (directory-str-unix *emacsdir-name* homedir-str))
	 (native-dotemacs-str (file-str-unix *dotemacs-filename* homedir-str))
	 (native-init-str (file-str-unix *init-filename* native-emacsdir-str))
	 (native-emacsdir (probe-file native-emacsdir-str))
	 (native-emacsdir-p (uiop:pathname-equal native-emacsdir
						 (uiop:ensure-pathname native-emacsdir-str)))
	 (native-dotemacs (probe-file native-dotemacs-str))
	 (native-init (uiop:pathname-equal (probe-file native-init-str)
					   (uiop:ensure-pathname native-init-str)))
	 (emacsdir-symlink nil)
	 (native-cfg nil))
  
    ;; Detect whether '.emacs.d' is a symlink or not and register vars accordingly:
    (when (not native-emacsdir-p)
      (setf emacsdir-symlink native-emacsdir)
      (setf native-emacsdir nil))
    
    ;; Detect if there is definitely a native emacs configuration:
    (when (and native-emacsdir-p
	       (or native-dotemacs
		   native-init))
      (setf native-cfg t))
    
;;    (format t "(native-cfg-unix) homedir-str -> ~a~%" homedir-str)
;;    (format t "(native-cfg-unix) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(native-cfg-unix) native-dotemacs-str -> ~a~%" native-dotemacs-str)
;;    (format t "(native-cfg-unix) native-init-str -> ~a~%" native-init-str)
;;    (format t "(native-cfg-unix) native-emacsdir -> ~a~%" native-emacsdir)
;;    (format t "(native-cfg-unix) native-emacsdir-p -> ~a~%" native-emacsdir-p)
;;    (format t "(native-cfg-unix) native-emacsdir-symlink -> ~a~%" emacsdir-symlink)
;;    (format t "(native-cfg-unix) native-emacsdir -> ~a~%" native-emacsdir)
;;    (format t "(native-cfg-unix) native-dotemacs -> ~a~%" native-dotemacs)
;;    (format t "(native-cfg-unix) native-init -> ~a~%" native-init)

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
    (values native-cfg native-emacsdir native-dotemacs
	    native-emacsdir-str native-dotemacs-str)))


;;; Finds and registers files and directories relevant to the saved 'myemacs' configuration.
;;; Returns three values:
;;;  1) 'saved-dirs': If found, path of the native emacs directory, 'NIL' otherwise.
;;;  2) 'saved-cfgs': If found, path of this native emacs init file,'NIL' otherwise.
;;;  3) 'active-cfg': If found, path of this native emacs init file, 'NIL' otherwise.
(defun saved-cfgs-unix ()
  ;; Acquire values
  (let* ((homedir-str (gethash 'homedir-str *data*))
	 (myemacsdir-str (directory-str-unix *myemacsdir-name* homedir-str))
	 (myemacsdir (ensure-directories-exist (pathname myemacsdir-str)))
	 (emacsdir-symlink (gethash 'emacsdir-symlink *data*))
	 (saved-cfgs (get-saved-cfgs myemacsdir-str))
	 (active-cfg (get-emacs-cfg-name-unix emacsdir-symlink)))

;;    (format t "(saved-cfgs-unix) myemacsdir-str -> ~a~%" myemacsdir-str)
;;    (format t "(saved-cfgs-unix) myemacsdir -> ~a~%" myemacsdir)
;;    (format t "(saved-cfgs-unix) emacsdir-symlink -> ~a~%" emacsdir-symlink)
;;    (format t "(saved-cfgs-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(saved-cfgs-unix) active-cfg -> <~a>~%" active-cfg)
    
    ;; Register values
    (setf (gethash 'myemacsdir-str *data*) myemacsdir-str)
    (setf (gethash 'myemacsdir *data*) myemacsdir)
    (setf (gethash 'saved-cfgs *data*) saved-cfgs)
    (setf (gethash 'active-cfg *data*) active-cfg)
    ;; Return some values
    (values active-cfg saved-cfgs)))

;;; Registers relevant values concerning the native 'emacs' and the 'myemacs' configurations.
(defun register-cfg-unix ()
  (multiple-value-bind (native-cfg native-emacsdir native-dotemacs
			native-emacsdir-str native-dotemacs-str)
      (native-cfg-unix)
    (multiple-value-bind (active-cfg) (saved-cfgs-unix)
      ;; Cleaning spurious native files/directory
      (cond
	;; A native .emacs.d dir is found but no native init files.
	;; Delete it if empty and recheck.
	((and (not native-cfg)
              native-emacsdir)
	 (progn
	   (uiop:delete-directory-tree native-emacsdir :validate t)
	   (setf native-emacsdir (probe-file native-emacsdir-str))
	   (setf (gethash 'native-emacsdir *data*) native-emacsdir)))
	;; An active cfg found (symbolic link .emacs.d dir) together with
	;; a native init file .emacs. Delete it and recheck.
	((and active-cfg
	     native-dotemacs)
	 (progn
	   (uiop:delete-file-if-exists native-dotemacs)
	   (setf native-dotemacs (probe-file native-dotemacs-str))
	   (setf (gethash 'native-dotemacs *data*) native-dotemacs)))))))

(defun action-show-unix ()
  (let (;;(homedir-str (gethash 'homedir-str *data*))
	;;(native-dotemacs-str (gethash 'native-dotemacs-str *data*))
	;;(native-init-str (gethash 'native-init-str *data*))
	;;(native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	;;(native-dotemacs (gethash 'native-dotemacs *data*))
	;;(native-init (gethash 'native-init *data*))
	;;(native-emacsdir (gethash 'native-emacsdir *data*))
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
  (let* (;;(native-dotemacs-str (gethash 'native-dotemacs-str *data*))
	 ;;(native-init-str (gethash 'native-init-str *data*))
	 (native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 ;;(native-dotemacs (gethash 'native-dotemacs *data*))
	 ;;(native-init (gethash 'native-init *data*))
	 ;;(native-emacsdir (gethash 'native-emacsdir *data*))
	 (myemacs-base-dir-str (gethash 'myemacs-base-dir-str *data*))
	 ;;(myemacs-base-dir (gethash 'myemacs-base-dir *data*))
	 (native-cfg (gethash 'native-cfg *data*))
	 (active-cfg (gethash 'active-cfg *data*))
	 (saved-cfgs (gethash 'saved-cfgs *data*))
	 ;;(saved-cfgs-keyw (mapcar #'cfg-str-to-keyw saved-cfgs))
	 (cfg-found-in-saved-cfgs (find cfg (mapcar #'cfg-str-to-keyw saved-cfgs)))
	 (cfg-str (string-downcase cfg))
	 ;;(active-cfg-keyw (cfg-str-to-keyw active-cfg))
	 ;;(directory-separator (string (uiop:directory-separator-for-host)))
	 (target-link-name (concatenate 'string *emacsdir-name* "-" cfg-str))
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
       (msg (err-action-use-cfg-not-available cfg)))
      ;; La configuración ya está activa
      ((eql cfg (cfg-str-to-keyw active-cfg))
       (msg (warn-action-use-cfg-already-active (cfg-keyw-to-str cfg))))
      ;; Si hay una configuración activa, hay que borrar el directorio de emacs
      (active-cfg
       ;;(format t "Tengo que borrar un enlace simbólico y crear otro.")))))
       (progn
	 ;; Delete the .emacs.d symlink
	 (delete-file (rem-last-sep native-emacsdir-str))
	 ;; Create a new symlink
	 (osicat:make-link (rem-last-sep native-emacsdir-str) :target target-link)
	 (setf changed-p t)))
      ((not active-cfg)
       (not native-cfg)
       (progn
	 ;; Create a new symlink
	 (osicat:make-link (rem-last-sep native-emacsdir-str) :target target-link)
	 (setf changed-p t))))
    (when changed-p
      (get-and-register-cfg-unix)
      (show-cfg-unix)
      (setf changed-p nil))))

(defun get-cfgdir-from-saved-cfgs (cfg-str)
  (let ((index (position cfg-str (gethash 'saved-cfgs *data*) :test #'string-equal)))
    (if index
	(nth index (gethash 'saved-dirs *data*))
	nil)))	   

(defun del-cfg-unix (cfg)
  (let* (;;(native-dotemacs-str (gethash 'native-dotemacs-str *data*))
	 ;;(native-init-str (gethash 'native-init-str *data*))
	 (native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 ;;(native-dotemacs (gethash 'native-dotemacs *data*))
	 ;;(native-init (gethash 'native-init *data*))
	 ;;(native-emacsdir (gethash 'native-emacsdir *data*))
	 (myemacs-base-dir-str (gethash 'myemacs-base-dir-str *data*))
	 ;;(myemacs-base-dir (gethash 'myemacs-base-dir *data*))
	 (native-cfg (gethash 'native-cfg *data*))
	 (active-cfg (gethash 'active-cfg *data*))
	 (saved-cfgs (gethash 'saved-cfgs *data*))
	 (saved-dirs (gethash 'saved-dirs *data*))
	 ;;(saved-cfgs-keyw (mapcar #'cfg-str-to-keyw saved-cfgs))
	 (cfg-found-in-saved-cfgs (find cfg (mapcar #'cfg-str-to-keyw saved-cfgs)))
	 (cfg-str (string-downcase cfg))
	 ;;(active-cfg-keyw (cfg-str-to-keyw active-cfg))
	 ;;(directory-separator (string (uiop:directory-separator-for-host)))
	 (target-link-name (concatenate 'string *emacsdir-name* "-" cfg-str))
	 ;;(target-link-name (string-right-trim directory-separator target-link-name))
	 ;;(myemacs-base-dir-str (string-right-trim directory-separator myemacs-base-dir-str))
	 ;;(native-emacsdir-str (string-right-trim directory-separator native-emacsdir-str))
	 (target-link (get-directory-in-path-str-unix target-link-name
						      :basepath (rem-last-sep myemacs-base-dir-str)
	       					      :lastsep nil))
	 (delete-dir-p nil)
	 (changed-p nil))

;;    (format t "(del-cfg-unix) native-dotemacs-str -> ~a~%" native-dotemacs-str)
;;    (format t "(del-cfg-unix) native-init-str -> ~a~%" native-init-str)
;;    (format t "(del-cfg-unix) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(del-cfg-unix) native-dotemacs -> ~a~%" native-dotemacs-str)
;;    (format t "(del-cfg-unix) native-init -> ~a~%" native-init-str)
;;    (format t "(del-cfg-unix) native-emacsdir -> ~a~%" native-emacsdir-str)
;;    (format t "(del-cfg-unix) myemacs-base-dir-str -> ~a~%" myemacs-base-dir-str)
;;    (format t "(del-cfg-unix) myemacs-base-dir -> ~a~%" myemacs-base-dir)
;;    (format t "(del-cfg-unix) native-cfg -> ~a~%" native-cfg)
;;    (format t "(del-cfg-unix) active-cfg -> ~a~%" active-cfg)
;;    (format t "(del-cfg-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(del-cfg-unix) target-link-name -> ~a~%" target-link-name)
;;    (format t "(del-cfg-unix) target-link -> ~a~%" target-link)
    ;; Detect errors
    (cond
      ;; Error: There are no saved configs or cfg is not found in them.
      ((or (null saved-cfgs)
	   (not (find cfg-str saved-cfgs :test #'string-equal)))
       (msg (warn-action-del-cfg-not-found cfg-str)))
      ;; cfg is the active configuration
      ((string-equal cfg-str active-cfg)
       (progn
	 (setf delete-dir-p (prompt-read-yes-no (msg
						    (ask-delete-directory-tree
						     (get-cfgdir-from-saved-cfgs cfg-str)))))
	 (when delete-dir-p
	   ;;(format t "(del-cfg-unix) ~a is the active configuration.~%" cfg-str)
	   ;;(format t "(del-cfg-unix) going to delete symlink.~%")
	   ;; Delete the .emacs.d symlink
	   (delete-file (rem-last-sep native-emacsdir-str))
	   ;; Delete the real symlink directory
	   (uiop:delete-directory-tree (pathname (get-cfgdir-from-saved-cfgs active-cfg)) :validate t))
	 (unless delete-dir-p
	   (format t "Anulado comando :del~%"))))
      ;; cfg is not the active configuration
      (t
       (progn
	 (setf delete-dir-p (prompt-read-yes-no (msg
						    (ask-delete-directory-tree
						     (get-cfgdir-from-saved-cfgs cfg-str)))))
	 (when delete-dir-p
	   ;;(format t "(del-cfg-unix) ~a is not the active configuration.~%" (get-cfgdir-from-saved-cfgs cfg-str))
	   ;;(format t "(del-cfg-unix) going to delete de ~a directory.~%" (get-cfgdir-from-saved-cfgs cfg-str)))
	   (uiop:delete-directory-tree (pathname (get-cfgdir-from-saved-cfgs cfg-str)) :validate t))
	 (unless delete-dir-p
	   (format t "Anulado comando :del~%")))))
    
    (when changed-p
      (get-and-register-cfg-unix)
      (show-cfg-unix)
      (setf changed-p nil))))

;;(defun show-cfg ()
;;  (cond
;;    ((uiop:os-unix-p)
;;     (get-and-register-cfg-unix)
;;     (show-cfg-unix))))

;;(defun use-cfg (cfg)
;;  (cond
;;    ((uiop:os-unix-p)
;;     (get-and-register-cfg-unix)
;;     (use-cfg-unix cfg))))
;;
;;(defun del-cfg (cfg)
;;  (cond
;;    ((uiop:os-unix-p)
;;     (get-and-register-cfg-unix)
;;     (del-cfg-unix cfg))))




;;; help
(defun action-help ()
  (msg (info-action-help)))

;;; version
(defun action-version ()
  (msg (info-action-version)))

;;; show
(defun action-show ()
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-show-unix))))

;;(defun action-show ()
;;  (format t "(action-show)~%"))

;;(defun action-show ()
;;  (show-cfg))

;;; use
(defun action-use (cfg)
  (use-cfg cfg))

;;(defun action-use (cfg)
;;  (use-cfg cfg))

;; del
(defun action-del (cfg)
  (format t "(action-del) cfg -> ~a~%" cfg))

;;(defun action-del (cfg)
;;  (del-cfg cfg))

;; copy
(defun action-copy (src dst)
  (format t "(action-copy) src -> ~a~%" src)
  (format t "(action-copy) dst -> ~a~%" dst))

;; del-native
(defun action-del-native ()
  (format t "(action-del-native)~%"))

;; save-native-as
(defun action-save-native-as (cfg)
  (format t "(action-save-native-as) cfg -> ~a~%" cfg))

;;; retrieve-native
(defun action-retrieve-native (cfg)
  (format t "(action-retrieve-native) cfg -> ~a~%" cfg))



;;; ************************************************************************************************
;;; ********************* SERVICEABLE FUNCTIONS
;;; ************************************************************************************************

;;; Calls the appropriate action-function
;;; Parameters:
;;; 'lcmd': Keyword list where the first element is a command and the rest are options.
(defun action-delivery-center (lcmd)
  (let ((action (car lcmd))
	(opt (cdr lcmd)))
  (cond
    ((null lcmd) (action-help))
    ((eql action :help) (action-help))
    ((eql action :version) (action-version))
    ((eql action :show) (action-show))
    ((eql action :use) (action-use (keyw-to-cfg (car opt))))
    ((eql action :del)  (action-del (keyw-to-cfg (car opt))))
    ((eql action :copy) (action-copy (keyw-to-cfg (car opt)) (keyw-to-cfg (second opt))))
    ((eql action :del-native) (action-del-native))
    ((eql action :save-native-as) (action-save-native-as (keyw-to-cfg (car opt))))
    ((eql action :retrieve-native) (action-retrieve-native (keyw-to-cfg (car opt)))))))


;;(defun action-delivery-center (lcmd)
;;  (let ((action (car lcmd))
;;	(cfg (cdr lcmd)))
;;  (cond
;;    ((null lcmd) (action-help))
;;    ((eql action :help) (action-help))
;;    ((eql action :version) (action-version))
;;    ((eql action :show) (action-show))
;;    ((eql action :use) (action-use (car cfg)))
;;    ((eql action :del)  (action-del (car cfg)))
;;    ((eql action :copy) (action-copy (car cfg) (second cfg)))
;;    ((eql action :del-native) (action-del-native))
;;    ((eql action :save-native-as) (action-save-native-as (car cfg)))
;;    ((eql action :retrieve-native) (action-retrieve-native (car cfg))))))
