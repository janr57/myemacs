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
  ;;(format t "(native-cfg-unix) ENTERING~%")
  ;; Aqcuire values
  (let* ((homedir-str (add-last-dirsep (uiop:getenv "HOME")))
	 (native-emacsdir-str (directory-str-unix +emacs-dirname+ homedir-str))
	 (native-dotemacs-str (file-str-unix +dotemacs-filename+ homedir-str))
	 (native-init-str (file-str-unix +init-filename+ native-emacsdir-str))
	 (native-emacsdir (probe-file native-emacsdir-str))
;;	 (native-emacsdir-p (uiop:pathname-equal native-emacsdir
;;						 (uiop:ensure-pathname native-emacsdir-str)))
;;	 (native-emacsdir-p (string-equal (namestring native-emacsdir)
	 ;;						 native-emacsdir-str))
	 (native-emacsdir-p (get-native-emacsdir-p native-emacsdir native-emacsdir-str))
	 (native-dotemacs (probe-file native-dotemacs-str))
;;	 (native-init (uiop:pathname-equal (probe-file native-init-str)
;;					   (uiop:ensure-pathname native-init-str)))
	 (native-init (get-native-init native-init-str))
	 emacsdir-symlink
	 native-cfg)
  
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
  ;;(format t "(saved-cfgs-unix) ENTERING~%")
  ;; Acquire values
  (let* ((homedir-str (gethash 'homedir-str *data*))
	 (myemacsdir-str (directory-str-unix +myemacs-dirname+ homedir-str))
	 (myemacsdir (ensure-directories-exist (pathname myemacsdir-str)))
	 (emacsdir-symlink (gethash 'emacsdir-symlink *data*))
	 (saved-cfgs (get-saved-cfgs myemacsdir-str))
	 (active-cfg (get-emacs-cfg-name-unix emacsdir-symlink)))

    (when (not (find active-cfg saved-cfgs :test #'string-equal))
      (setf active-cfg nil))

;;    (format t "(saved-cfgs-unix) homedir-str -> ~a~%" homedir-str)
;;    (format t "(saved-cfgs-unix) myemacsdir-str -> ~a~%" myemacsdir-str)
;;    (format t "(saved-cfgs-unix) myemacsdir -> ~a~%" myemacsdir)
;;    (format t "(saved-cfgs-unix) emacsdir-symlink -> ~a~%" emacsdir-symlink)
;;    (format t "(saved-cfgs-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(saved-cfgs-unix) active-cfg -> <~a>~%" active-cfg)
;;    (terpri t)
    
    ;; Register values
    (setf (gethash 'myemacsdir-str *data*) myemacsdir-str)
    (setf (gethash 'myemacsdir *data*) myemacsdir)
    (setf (gethash 'saved-cfgs *data*) saved-cfgs)
    (setf (gethash 'active-cfg *data*) active-cfg)
    ;; Return some values
    (values active-cfg saved-cfgs)))

;;; Registers relevant values concerning the native 'emacs' and the 'myemacs' configurations.
(defun register-cfg-unix ()
  ;;(format t "(register-cfg-unix) ENTERING~%")
  (multiple-value-bind
	(native-cfg native-emacsdir native-dotemacs native-emacsdir-str native-dotemacs-str)
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
  ;;(format t "(action-show-unix) ENTERING~%")
  (let ((native-cfg (gethash 'native-cfg *data*))
	(active-cfg (gethash 'active-cfg *data*))
	(saved-cfgs (gethash 'saved-cfgs *data*)))

;;    (format t "(action-show-unix) native-cfg -> ~a~%" native-cfg)
;;    (format t "(action-show-unix) active-cfg -> ~a~%" active-cfg)
;;    (format t "(action-show-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (terpri t)

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
       (format t "(action-show-unix) *** ERROR THE PROGRAM SHOULDN'T HAVE REACHED HERE!~%")))))

(defun action-use-unix (cfg)
  ;;(format t "(action-use-unix) ENTERING~%")
  (let* ((native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 (native-cfg (gethash 'native-cfg *data*))
	 (active-cfg (gethash 'active-cfg *data*))
	 (saved-cfgs (gethash 'saved-cfgs *data*))
	 (cfg-in-saved-cfgs (find cfg saved-cfgs :test #'string-equal))
	 (cfgdir (cfgdir-from cfg))
	 changed-p)

;;    (format t "(action-use-unix) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(action-use-unix) native-cfg -> ~a~%" native-cfg)
;;    (format t "(action-use-unix) active-cfg -> ~a~%" active-cfg)
;;    (format t "(action-use-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(action-use-unix) cfg-in-saved-cfgs -> ~a~%" cfg-in-saved-cfgs)
;;    (format t "(action-use-unix) cfgdir -> ~a~%" cfgdir)
;;    (terpri t)

    ;; Detect errors
    (cond
      ;; Error: A native emacs configuration exists
      (native-cfg
       (msg (err-action-use-native-cfg)))
      ;; Error: cfg does not exist
      ((not cfg-in-saved-cfgs)
       (msg (err-cfg-not-available cfg)))
      ;; La configuración ya está activa
      ((string-equal cfg active-cfg)
       (msg (warn-action-use-cfg-already-active cfg)))
      ;; Si hay una configuración activa, hay que borrar el directorio de emacs
      (active-cfg
       ;;(format t "Tengo que borrar un enlace simbólico y crear otro.")))))
       (progn
	 ;; Delete the .emacs.d symlink
	 (delete-file (rem-last-dirsep native-emacsdir-str))
	 ;; Create a new symlink
	 (osicat:make-link (rem-last-dirsep native-emacsdir-str) :target cfgdir)
	 (setf changed-p t)))
      ((and (not active-cfg)
	   (not native-cfg))
       (progn
	 ;; Create a new symlink
	 (osicat:make-link (rem-last-dirsep native-emacsdir-str) :target cfgdir)
	 (setf changed-p t))))
    
    (when changed-p
      (register-cfg-unix)
      (action-show-unix)
      (setf changed-p nil))))

(defun action-del-unix (cfg)
  ;;(format t "(action-del-unix) ENTERING~%")
  (let* ((native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 (active-cfg (gethash 'active-cfg *data*))
	 (saved-cfgs (gethash 'saved-cfgs *data*))
	 (cfgdir (cfgdir-from cfg))
	 delete-dir-p
	 changed-p)

;;    (format t "(action-del-unix) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(action-del-unix) active-cfg -> ~a~%" active-cfg)
;;    (format t "(action-del-unix) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(action-del-unix) cfgdir -> ~a~%" cfgdir)
;;    (terpri t)

    ;; Detect errors
    (cond
      ;; Error: There are no saved configurations or cfg is not found in the list of saved ones.
      ((or (null saved-cfgs)
	   (not (find cfg saved-cfgs :test #'string-equal)))
       (msg (warn-action-del-cfg-not-found cfg)))
      ;; cfg is the active configuration
      ((string-equal cfg active-cfg)
       (progn
	 (setf delete-dir-p (prompt-read-yes-no
			     (msg (ask-delete-directory-tree cfgdir))))
	 (when delete-dir-p
	   ;; Delete the .emacs.d symlink
	   (delete-file (rem-last-dirsep native-emacsdir-str))
	   ;; Delete the real symlink directory
	   (uiop:delete-directory-tree cfgdir :validate t)
	   (setf changed-p t))
	 (unless delete-dir-p
	   (format t "Anulado comando :del~%"))))
      ;; cfg is not the active configuration
      (t (progn
	   (setf delete-dir-p (prompt-read-yes-no
			       (msg (ask-delete-directory-tree cfgdir))))
	   (when delete-dir-p
	     (uiop:delete-directory-tree cfgdir :validate t)
	     (setf changed-p t))
	   (unless delete-dir-p
	     (format t "Anulado comando :del~%")))))
    
    (when changed-p
      (register-cfg-unix)
      (action-show-unix)
      (setf changed-p nil))))

(defun action-copy-unix (src dst)
  ;;(format t "(action-copy-unix) ENTERING~%")
  (let ((srcdir (cfgdir-from src))
	(dstdir (cfgdir-from dst))
	(changed-p nil))

;;    (format t "(action-copy-unix) src -> ~a~%" src)
;;    (format t "(action-copy-unix) dst -> ~a~%" dst)
;;    (format t "(action-copy-unix) srcdir -> ~a~%" srcdir)
;;    (format t "(action-copy-unix) dstdir -> ~a~%" dstdir)
;;    (terpri t)

    (cond
      ((not (uiop:directory-exists-p srcdir))
       (msg (err-source-dir-does-not-exist srcdir)))
      ((uiop:directory-exists-p dstdir)
       (msg (err-target-dir-exists dstdir)))
      (t (progn
	   (copy-directory:copy srcdir dstdir)
	   (setf changed-p t))))

    (when changed-p
      (register-cfg-unix)
      (action-show-unix)
      (setf changed-p nil))))

;;; del-native
(defun action-del-native-unix ()
  ;;(format t "(action-del-native-unix) ENTERING~%")
  (let ((native-cfg (gethash 'native-cfg *data*))
	(native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	(native-dotemacs-str (gethash 'native-dotemacs-str *data*))
	(native-emacsdir (gethash 'native-emacsdir *data*))
	(native-dotemacs (gethash 'native-dotemacs *data*))
	(changed-p nil))
    
;;    (format t "(action-del-native) native-cfg -> ~a~%" native-cfg)
;;    (format t "(action-del-native) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(action-del-native) native-dotemacs-str -> ~a~%" native-dotemacs-str)
;;    (format t "(action-del-native) native-emacsdir -> ~a~%" native-emacsdir)
;;    (format t "(action-del-native) native-dotemacs -> ~a~%" native-dotemacs)
;;    (terpri t)

    (cond
       ((not native-cfg)
	(msg (err-no-native-cfg)))
       (t (progn
	    (when native-dotemacs
	      ;;(format t "(action-del-native-unix) DELETE -> ~a~%" native-dotemacs-str)
	      (uiop:delete-file-if-exists native-dotemacs-str)
	      (setf changed-p t))
	    ;;(uiop:delete-file-if-exists native-emacsdir-str)
	    (when (uiop:directory-exists-p native-emacsdir-str)
	      ;;(format t "(action-del-native-unix) DELETE -> ~a~%" native-emacsdir-str)
	      (uiop:delete-directory-tree native-emacsdir :validate t)
	      (setf changed-p t)))))

    (when changed-p
      (register-cfg-unix)
      (action-show-unix)
      (setf changed-p nil))))

;;; save-native-as
(defun action-save-native-as-unix (cfg)
  ;;(format t "(action-save-native-as-unix) ENTERING~%")
  (let* ((native-cfg (gethash 'native-cfg *data*))
	 (native-emacsdir (gethash 'native-emacsdir *data*))
	 (native-dotemacs (gethash 'native-dotemacs *data*))
	 (saved-cfgs (gethash 'saved-cfgs *data*))
	 (cfg-in-saved-cfgs (find cfg saved-cfgs :test #'string-equal))
	 (cfgdir-str (cfgdir-str-from cfg))
	 (init-file-str (file-str-unix +init-filename+ cfgdir-str))
	 changed-p)
    
;;    (format t "(action-save-native-as) cfg -> ~a~%" cfg)
;;    (format t "(action-save-native-as) native-cfg -> ~a~%" native-cfg)
;;    (format t "(action-save-native-as) native-emacsdir -> ~a~%" native-emacsdir)
;;    (format t "(action-save-native-as) native-dotemacs -> ~a~%" native-dotemacs)
;;    (format t "(action-save-native-as) saved-cfgs -> ~a~%" saved-cfgs)
;;    (format t "(action-save-native-as) cfg-in-saved-cfgs -> ~a~%" cfg-in-saved-cfgs)
;;    (format t "(action-save-native-as) cfgdir-str -> ~a~%" cfgdir-str)
;;    (format t "(action-save-native-as) init-file-str -> ~a~%" init-file-str)
;;    (terpri t)

    (cond
      ((not native-cfg)
       (msg (err-no-native-cfg)))
      (cfg-in-saved-cfgs
       (msg (err-cfg-not-available cfg)))
      (t (progn
	   ;; Copiar directorio nativo ".emacs.d" a "~/.myemacs/emacs.d-<cfg>"
	   (copy-directory:copy native-emacsdir (cfgdir-from cfg))
	   ;; Copiar fichero ".emacs" a "~/.myemacs/emacs.d-<cfg>/init.el"
	   (when native-dotemacs
	     (uiop:copy-file native-dotemacs init-file-str))
	   (setf changed-p t))))

    (when changed-p
      (register-cfg-unix)
      (action-show-unix)
      (setf changed-p nil))))

;;; restore-native
(defun action-restore-native-unix (cfg)
  ;;(format t "(action-restore-native-unix) ENTERING~%")
  (let* ((native-cfg (gethash 'native-cfg *data*))
	 (native-emacsdir-str (gethash 'native-emacsdir-str *data*))
	 (active-cfg (gethash 'active-cfg *data*))
	 changed-p)
    
;;    (format t "(action-restore-native-unix) cfg -> ~a~%" cfg)
;;    (format t "(action-restore-native-unix) native-cfg -> ~a~%" native-cfg)
;;    (format t "(action-restore-native-unix) native-emacsdir-str -> ~a~%" native-emacsdir-str)
;;    (format t "(action-restore-native-unix) active-cfg -> ~a~%" active-cfg)
;;    (terpri t)

    (cond
      (native-cfg
       (msg (err-native-cfg)))
      (t (progn
	   (when active-cfg
	     ;; Delete the .emacs.d symlink
	     (delete-file (rem-last-dirsep native-emacsdir-str))
	     (setf changed-p t))
	   ;; Copy cfg directory to .emacs.d
	   (copy-directory:copy (cfgdir-from cfg) native-emacsdir-str)
	   (setf changed-p t))))

    (when changed-p
      (register-cfg-unix)
      (action-show-unix)
      (setf changed-p nil))))

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


;;; use
(defun action-use (cfg)
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-use-unix cfg))))

;; del
(defun action-del (cfg)
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-del-unix cfg))))  

;; copy
(defun action-copy (src dst)
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-copy-unix src dst))))

;; del-native
(defun action-del-native ()
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-del-native-unix))))

;; save-native-as
(defun action-save-native-as (cfg)
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-save-native-as-unix cfg))))
  
;;; retrieve-native
(defun action-restore-native (cfg)
  (cond
    ((uiop:os-unix-p)
     (register-cfg-unix)
     (action-restore-native-unix cfg))))

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
    ((eql action :restore-native) (action-restore-native (keyw-to-cfg (car opt)))))))

