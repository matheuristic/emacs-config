;;; init-ui-color.el --- Emacs config color layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure color theme

;;; Code:

;; use eReader theme
(use-package ereader-theme
  :ensure nil ;; ereader-theme.el is a local file
  :config (load-theme 'ereader t))

(provide 'init-ui-color)

;;; init-ui-color.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
