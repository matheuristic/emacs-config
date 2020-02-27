;;; init.el --- Emacs config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; Emacs configuration file, symlink or copy to ~/.emacs or ~/.emacs.d/init.el

;; Startup times can be measured in Linux using
;; $ emacs -q --eval='(message "%s" (emacs-init-time))'
;; or in Mac OS X using
;; $ open -n /Applications/Emacs.app --args -q --eval='(message "%s" (emacs-init-time))'

;; Optimizing startup times
;; 1. profile using the `esup' package ("M-x esup")
;; 2. defer loading of packages when possible, e.g. use-package's `:defer N'
;;    with N=1 (sec) for important packages and N=2 for less important ones
;; 3. avoid helper functions that can cause eager loads

;;; Code:

;; optimizations for reducing startup time (reverted at the end of file)
;; * set file-name-handler-alist to nil as it is scanned when files are loaded
(defvar tmp-file-name-handler-alist file-name-handler-alist) ;; save to tmp var
(setq file-name-handler-alist nil)

;; performance optimizations
;; * increase garbage collection threshold
;; * increase max bytes read from a sub-process in a single op (Emacs 27+)
(setq gc-cons-threshold 100000000 ;; in bytes, default is 800k
      read-process-output-max 1048576) ;; in bytes, default is 4096 bytes

;; load local pre-initialization file ~/.emacs.d/init-pre.el
(let ((local-f (expand-file-name "init-pre.el" user-emacs-directory)))
  (if (file-exists-p local-f) (load-file local-f)))

;; always store Customize settings in a separate file
;; default to ~/.emacs/custom.el if none is specified
(when (or (not (boundp 'custom-file))
          (not custom-file))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; user packages in ~/.emacs.d/lisp
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir) (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)
(dolist (project (directory-files lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

;; third-party packages in ~/.emacs.d/site-lisp and its subdirectories
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

(require 'init-backup) ;; configure automatic file backups
(require 'init-package) ;; use-package macro for configuring packages

;; copy environment variables from shell, OS X GUI mode-only
(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :init (if (memq window-system '(mac ns))
                (exec-path-from-shell-initialize))))

;; visit large files without loading it entirely
(use-package vlf
  :config (require 'vlf-setup))

(require 'init-sensitive)
(require 'init-ui)
(require 'init-dired)
(require 'init-term)
(require 'init-vc)
(require 'init-proj)
(require 'init-org)
(require 'init-lang)
(require 'init-http)
(require 'init-web)

;; load local post-initialization file ~/.emacs.d/init-post.el
(let ((local-f (expand-file-name "init-post.el" user-emacs-directory)))
  (if (file-exists-p local-f) (load-file local-f)))

;; load Customize settings
(load custom-file 'noerror)

;; revert earlier optimizations for reducing startup time
(setq file-name-handler-alist tmp-file-name-handler-alist)
(makunbound 'tmp-file-name-handler-alist) ;; unbind tmp var

(provide 'init)
;;; init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
