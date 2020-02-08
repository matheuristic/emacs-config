;;; init-ui-modeline.el --- Emacs config UI layer mode-line settings -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up mode-line

;;; Code:

(when (display-graphic-p)
  ;; show full path in mode-line buffer name tooltip
  (setq-default mode-line-buffer-identification
                (list (propertize
                       "%12b"
                       'face 'mode-line-buffer-id
                       'help-echo '(format
                                    (mapconcat 'identity
                                               '("%s"
                                                 "mouse-1: Previous buffer"
                                                 "mouse-3: Next buffer")
                                               "\n")
                                    (or (buffer-file-name) (buffer-name)))
                       'mouse-face 'mode-line-highlight
                       'local-map mode-line-buffer-identification-keymap)))

  ;; hide minor-modes in mode-line menu, which is also accessible via `minions-minor-mode-menu'
  (use-package minions
    :init (minions-mode 1)
    :config (setq minions-direct '(overwrite-mode view-mode) ;; modes in minions-direct are always shown
                  minions-mode-line-lighter "☰")))

(provide 'init-ui-modeline)

;;; init-ui-modeline.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
