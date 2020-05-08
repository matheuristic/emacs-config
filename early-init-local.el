;;; early-init-local.el --- Sample Emacs local early init config file -*- lexical-binding: t; -*-

;; Author: matheuristic

;;; Commentary:

;; Local early initialization config code goes here (e.g. package repos)
;; Symlink or copy this file to ~/.emacs.d/early-init-local.el

;;; Code:

;; use GNU, Org, MELPA Stable and MELPA repositories for package.el
;; (setq package-archives '(("GNU"          . "https://elpa.gnu.org/packages/")
;;                          ("MELPA Stable" . "https://stable.melpa.org/packages/")
;;                          ("MELPA"        . "https://melpa.org/packages/")
;;                          ("Org"          . "https://orgmode.org/elpa/"))
;;       package-archive-priorities '(("GNU"          . 2)
;;                                    ("MELPA Stable" . 4)
;;                                    ("MELPA"        . 6)
;;                                    ("Org"          . 8)))

;; only load support for a subset of languages
;; (setq init-lang-enable-list '("csv" "docker" "json" "markdown" "yaml"))

;; use local eReader theme from ~/.emacs.d/lisp/ereader-theme.el
;; (let ((local-f (expand-file-name "lisp/ereader-theme.el" user-emacs-directory)))
;;   (when (file-exists-p local-f)
;;     (load-file local-f)
;;     (load-theme 'ereader t)))

;; set graphical Emacs fonts
;; (when (display-graphic-p)
;;   ;; change how characters are displayed
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Active-Display-Table.html#Active-Display-Table
;;   ;; the following displays '0' as '\ue007' (slashed zero in B612 font)
;;   ;; (setq standard-display-table (make-display-table))
;;   ;; (aset standard-display-table ?0 [?])

;;   ;; main fonts -> default, fixed-pitch, variable-pitch and mode-line
;;   ;; fallback font -> for when the display face does not have a character glyph

;;   ;; 1. bitmap fonts, better for low DPI screens
;;   ;; (set-face-attribute 'default nil :family "DinaRemaster"
;;   ;;                     :height 150 :weight 'normal :width 'normal)
;;   ;; (set-face-attribute 'fixed-pitch nil :family "DinaRemaster"
;;   ;;                     :height 150 :weight 'normal :width 'normal)
;;   ;; (set-face-attribute 'variable-pitch nil :family "Pixel Operator"
;;   ;;                     :height 150 :weight 'normal :width 'normal)
;;   ;; (set-face-attribute 'mode-line nil :family "DinaRemaster"
;;   ;;                     :height 120 :weight 'normal :width 'normal)
;;   ;; (set-face-attribute 'mode-line-inactive nil :family "DinaRemaster"
;;   ;;                     :height 120 :weight 'normal :width 'normal)
;;   ;; (set-fontset-font "fontset-default" 'unicode "Unifont")

;;   ;; 2. outline fonts, better for high DPI screens
;;   (set-face-attribute 'default nil :family "Iosevka"
;;                       :height 140 :weight 'normal :width 'expanded)
;;   (set-face-attribute 'fixed-pitch nil :family "Iosevka"
;;                       :height 140 :weight 'normal :width 'expanded)
;;   (set-face-attribute 'variable-pitch nil :family "Iosevka Aile"
;;                       :height 140 :weight 'normal :width 'normal)
;;   (set-face-attribute 'mode-line nil :family "Iosevka"
;;                       :height 110 :weight 'normal :width 'normal)
;;   (set-face-attribute 'mode-line-inactive nil :family "Iosevka"
;;                       :height 110 :weight 'normal :width 'normal)
;;   (set-fontset-font "fontset-default" 'unicode "Unifont"))


;; set conda install directory, used in lisp/init-lang-python.el
;; (setq conda-anaconda-home "~/miniconda3/")

;; path to MS Python Language Server binary, used by lisp/init-lang-python.el
;; (setq lsp-python-ms-executable "~/.local/bin/Microsoft.Python.LanguageServer")

;; set default directory for Org files to ~/org
;; (setq org-directory (file-name-as-directory (file-truename "~/org/")))

;; use top-level Org files in default Org directory for Org agenda files
;; (setq org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))

(provide 'early-init-local)
;;; early-init-local.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
