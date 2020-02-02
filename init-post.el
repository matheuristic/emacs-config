;;; init-post.el --- Sample Emacs local post-initialization config file -*- lexical-binding: t; -*-

;; Author: matheuristic

;;; Commentary:

;; Post-initialization code goes here
;; Symlink or copy this file to ~/.emacs.d/init-post.el

;;; Code:

;; Example of configuration to automatically break lines when they get too wide
;; (add-hook 'text-mode-hook (lambda ()
;;                             (turn-on-auto-fill)
;;                             (setq adaptive-fill-mode t)))

(provide 'init-post)
;;; init-post.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:

