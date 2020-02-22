;;; init-lang-scheme.el --- Emacs config Scheme language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Scheme tooling

;;; Code:

;; uncomment the following line to use Chez Scheme by default in geiser
;; (defvar geiser-active-implementations '(chez))

;; use paredit-mode when editing Scheme buffers
(add-hook 'scheme-mode-hook #'paredit-mode)

;; Scheme IDE, https://www.nongnu.org/geiser/
(use-package geiser
  :defer t
  :pin "MELPA")

(provide 'init-lang-scheme)

;;; init-lang-scheme.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
