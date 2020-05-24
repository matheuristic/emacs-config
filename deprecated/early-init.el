;;; early-init.el --- Emacs config file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/dotfiles

;;; Commentary:

;; Early initialization config, symlink or copy to ~/.emacs.d/early-init.el

;; In Emacs 27+, the sequence of initialization is
;; 1. early-init.el
;; 2. package.el
;; 3. init.el

;; early-init.el is run before UI elements are rendered,
;; so it is best to turn off unnecessary UI elements here

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

;; add user packages in ~/.emacs.d/lisp to load path
(defvar lisp-dir (expand-file-name "lisp" user-emacs-directory))
(unless (file-exists-p lisp-dir) (make-directory lisp-dir))
(add-to-list 'load-path lisp-dir)
(dolist (project (directory-files lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

;; add third-party packages in ~/.emacs.d/site-lisp and subdirs to load path
(defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(unless (file-exists-p site-lisp-dir) (make-directory site-lisp-dir))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (if (file-directory-p project) (add-to-list 'load-path project)))

;; remove unused UI elements
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; use local eReader theme from ~/.emacs.d/lisp/ereader-theme.el
(require 'ereader-theme)
(load-theme 'ereader t)

;; set typefaces for graphical Emacs
;; main fonts
(set-face-attribute 'default nil :family "Iosevka SS08"
                    :height 150 :weight 'normal :width 'normal)
(set-face-attribute 'fixed-pitch nil :family "Iosevka SS08"
                    :height 150 :weight 'normal :width 'normal)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile"
                    :height 150 :weight 'normal :width 'normal)
(set-face-attribute 'mode-line nil :family "Iosevka SS08"
                    :height 120 :weight 'normal :width 'normal)
(set-face-attribute 'mode-line-inactive nil :family "Iosevka SS08"
                    :height 120 :weight 'normal :width 'normal)
;; fallback font, see https://idiocy.org/emacs-fonts-and-fontsets.html
(set-fontset-font t nil "Symbola" nil 'append)

;; change how characters are displayed, see
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Active-Display-Table.html#Active-Display-Table
;; the following displays '0' using '\ue007' (private slashed zero in B612 font)
;; (setq standard-display-table (make-display-table))
;; (aset standard-display-table ?0 [?])

;; local machine-specific settings, set:
;; - conda install directory, used in lisp/init-lang-python.el
;; - MS Python Language Server binary path, used by lisp/init-lang-python.el
;; - default directory for Org files to ~/org
;; - top-level Org files in default Org directory as the Org agenda files
(setq conda-anaconda-home "~/miniconda3/"
      lsp-python-ms-executable "~/.local/bin/Microsoft.Python.LanguageServer"
      org-directory (file-name-as-directory (file-truename "~/org/"))
      org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))

;; always store Customize settings in a separate file
;; defaults to ~/.emacs/custom.el if none is specified
(when (or (not (boundp 'custom-file))
          (not custom-file))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(provide 'early-init)
;;; early-init.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
