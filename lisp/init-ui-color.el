;;; init-ui-color.el --- Emacs config color layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure color theme

;;; Code:

;; emulate reading on e-ink devices
(use-package eink-theme
  :config
  (load-theme 'eink t)
  ;; visible fringe bitmaps (invisible in the theme by default)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default))
  ;; have comment delimiters match comments
  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :foreground (face-foreground 'font-lock-comment-face)
                      :background (face-background 'font-lock-comment-face)
                      :inherit 'font-lock-comment-face)
  ;; make inactive mode-line foreground visible
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#909090")
  ;; more distinctive strings
  (set-face-attribute 'font-lock-string-face nil
                      :foreground "#606060")
  ;; less prominent line numbers
  (set-face-attribute 'line-number nil
                      :foreground "#cacaca"
                      :weight 'light)
  ;; enable highlights
  (set-face-attribute 'highlight nil
                      :background "#efecaf")
  ;; underline links
  (set-face-attribute 'link nil
                      :underline t)
  ;; have highlighted lines be visible
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil :background "#fedcba")))

(provide 'init-ui-color)

;;; init-ui-color.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
