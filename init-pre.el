;;; init-pre.el --- Sample Emacs local pre-initialization config file -*- lexical-binding: t; -*-

;; Author: matheuristic

;;; Commentary:

;; Pre-initialization code goes here
;; Symlink or copy this file to ~/.emacs.d/init-pre.el

;;; Code:

;; Example of only using GNU and MELPA repositories for package.el
;; (setq package-archives '(("GNU"          . "https://elpa.gnu.org/packages/")
;;                          ("MELPA"        . "https://melpa.org/packages/"))
;;       package-archive-priorities '(("GNU"          . 10)
;;                                    ("MELPA"        . 0)))

;; Example of only loading support for a subset of languages
;; (setq init-lang-enable-list '("csv" "docker" "json" "markdown" "yaml")

(provide 'init-pre)
;;; init-pre.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
