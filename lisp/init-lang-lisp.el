;;; init-lang-lisp.el --- Emacs config LISP language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Common Lisp tooling

;;; Code:

;; default to sbcl for Lisp REPL
(defvar inferior-lisp-program "sbcl --dynamic-space-size 1024")

;; use paredit-mode when editing Lisp buffers
(add-hook 'lisp-mode-hook #'paredit-mode)

;; company-mode completion backend for SLIME
(use-package slime-company
  :defer t
  :config (setq slime-company-major-modes '(lisp-mode slime-repl-mode)))

;; Lisp IDE, https://github.com/slime/slime
(use-package slime
  :commands (slime slime-lisp-mode-hook)
  :config (slime-setup '(slime-fancy slime-company)))

(provide 'init-lang-lisp)

;;; init-lang-lisp.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
