;;; init-pre.el --- Sample Emacs local pre-initialization config file -*- lexical-binding: t; -*-

;; Author: matheuristic

;;; Commentary:

;; Pre-initialization code goes here
;; Symlink or copy this file to ~/.emacs.d/init-pre.el

;;; Code:

;; use GNU and MELPA repositories for package.el
;; (setq package-archives '(("GNU"          . "https://elpa.gnu.org/packages/")
;;                          ("MELPA"        . "https://melpa.org/packages/"))
;;       package-archive-priorities '(("GNU"          . 10)
;;                                    ("MELPA"        . 0)))

;; only load support for a subset of languages
;; (setq init-lang-enable-list '("csv" "docker" "json" "markdown" "yaml"))

;; font list by priority, used in lisp/init-ui-font.el
;; (setq init-ui-font-fixed-pitch-list '("IBM Plex Mono"
;;                                       "Consolas"
;;                                       "Menlo"
;;                                       "DejaVu Sans Mono")
;;       init-ui-font-variable-pitch-list '("IBM Plex Serif"
;;                                          "Alegreya"
;;                                          "Constantia"
;;                                          "Charter"
;;                                          "DejaVu Serif"))

;; set directory where conda is installed, used in lisp/init-lang-python.el
;; (setq conda-anaconda-home "~/miniconda3/")

;; path to MS Python Language Server binary, used by lisp/init-lang-python.el
;; (setq lsp-python-ms-executable "~/.local/bin/Microsoft.Python.LanguageServer")

;; set default directory for Org files to ~/org
;; (setq org-directory (file-name-as-directory (file-truename "~/org/")))

;; use top-level Org files in default Org directory for Org agenda files
;; (setq org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))

;; use multimarkdown for processing markdown files in markdown-mode
;; (setq markdown-command "multimarkdown")

(provide 'init-pre)
;;; init-pre.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
