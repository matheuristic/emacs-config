;;; init.el --- Emacs config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; Emacs configuration file, symlink/copy to ~/.emacs or ~/.emacs.d/init.el

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

;; Whether to regenerate outdated bytecode
(defvar load-prefer-newer nil)

;; ELPA-compatible package.el repositories
(defvar package-archives '(("GNU"   . "https://elpa.gnu.org/packages/")
                           ("MELPA" . "https://melpa.org/packages/")))

;; Priority levels for the ELPA-compatible repositories
(defvar package-archive-priorities '(("GNU"   . 2)
                                     ("MELPA" . 6)))

;; initialize package.el
(setq package-enable-at-startup nil)
(require 'package)
(package-initialize)

;; bootstrap use-package, provides configuration macros
;; for info, see https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key packages
;; configure imenu support for the `require' and `use-package' keywords
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

;; copy environment variables from shell, OS X GUI mode-only
(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :init (if (memq window-system '(mac ns))
                (exec-path-from-shell-initialize))))

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

;; local machine-specific settings
;; automatically break lines that get too long
;; (add-hook 'text-mode-hook (lambda ()
;;                             (turn-on-auto-fill)
;;                             (setq adaptive-fill-mode t)))
;; use multimarkdown for processing markdown files in markdown-mode
(if (executable-find "multimarkdown")
  (setq markdown-command "multimarkdown"))
;; HTTP requests privacy settings
(setq url-cookie-untrusted-urls '(".*")) ;; no cookies
(setq url-privacy-level 'paranoid) ;; more private HTTP requests
(url-setup-privacy-info) ;; apply `url-privacy-level'

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
