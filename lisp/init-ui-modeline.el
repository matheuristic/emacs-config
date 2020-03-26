;;; init-ui-modeline.el --- Emacs config mode line settings -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; GUI mode line

;;; Code:

;; GUI only
(when (display-graphic-p)
  ;; fast and fancy minimalist mode line (requires all-the-icons be installed)
  (use-package doom-modeline
    :after all-the-icons
    :config
    (setq doom-modeline-buffer-file-name-style 'auto
          doom-modeline-env-version nil
          doom-modeline-minor-modes t
          doom-modeline-persp-name nil
          doom-modeline-unicode-fallback t)
    (doom-modeline-mode 1))

  ;; hide minor modes in mode line menu (also accessible with `minions-minor-mode-menu')
  (use-package minions
    :init
    (setq minions-direct '(overwrite-mode view-mode) ;; modes in minions-direct are always shown
          minions-mode-line-lighter "☰") ;; use UTF-8 mode line lighter
    (minions-mode 1)))

(provide 'init-ui-modeline)

;;; init-ui-modeline.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
