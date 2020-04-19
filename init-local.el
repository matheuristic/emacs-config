;;; init-local.el --- Sample Emacs local init config file -*- lexical-binding: t; -*-

;; Author: matheuristic

;;; Commentary:

;; Local non-early initialization configuration code goes here
;; This gets loaded after all other code in init.el
;; Symlink or copy this file to ~/.emacs.d/init-local.el

;;; Code:

;; Automatically break lines when they get too long
;; (add-hook 'text-mode-hook (lambda ()
;;                             (turn-on-auto-fill)
;;                             (setq adaptive-fill-mode t)))

;; use multimarkdown for processing markdown files in markdown-mode
;; (if (executable-find "multimarkdown")
;;   (setq markdown-command "multimarkdown"))

;; HTTP requests privacy settings
;; (setq url-cookie-untrusted-urls '(".*")) ;; no cookies
;; (setq url-privacy-level 'paranoid) ;; more private HTTP requests
;; (url-setup-privacy-info) ;; apply `url-privacy-level'

(provide 'init-local)
;;; init-local.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:

