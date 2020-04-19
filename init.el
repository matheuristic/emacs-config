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

;; backwards-compatibility code for Emacs versions <27
(when (version<= emacs-version "26.3")
  ;; load early-initialization file ~/.emacs.d/early-init.el
  ;; Emacs 27+ automatically loads this file before rendering UI elements
  (let ((local-f (expand-file-name "early-init.el" user-emacs-directory)))
    (if (file-exists-p local-f) (load-file local-f))))

;; visit large files without loading it entirely
(use-package vlf
  :config (require 'vlf-setup))

;; load custom Emacs configuration components
(require 'init-backup)
(require 'init-sensitive)
(require 'init-ui)
(require 'init-dired)
(require 'init-term)
(require 'init-proj)
(require 'init-org)
(require 'init-lang)
(require 'init-http)
(require 'init-web)

;; load local post initialization file ~/.emacs.d/init-local.el
(let ((local-f (expand-file-name "init-local.el" user-emacs-directory)))
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
