;;; init-ui-font.el --- Emacs config font layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up typography

;;; Code:

(defgroup init-ui-font-el nil
  "Font settings."
  :group 'convenience)

(defcustom init-ui-font-fixed-pitch-list '("Consolas"
                                           "Menlo"
                                           "DejaVu Sans Mono")
  "List of fonts, by priority, to use for the fixed pitch face."
  :type '(repeat string)
  :group 'init-ui-font-el)

(defcustom init-ui-font-variable-pitch-list '("Constantia"
                                              "Charter"
                                              "DejaVu Serif")
  "List of fonts, by priority, to use for the variable pitch face."
  :type '(repeat string)
  :group 'init-ui-font-el)

(defcustom init-ui-font-default 'fixed-pitch
  "Whether to use the fixed or variable pitch face to use as the default"
  :type '(choice (const :tag "Fixed pitch" fixed-pitch)
                 (const :tag "Variable pitch" variable-pitch))
  :group 'init-ui-font-el)

;; font settings only have effect in graphical Emacs
(when (display-graphic-p)
  ;; helper functions
  (require 'cl-extra)

  (defun my-font-exists (font-name)
    "Returns FONT-NAME if given font exists on the system and `nil` otherwise"
    (if (x-list-fonts font-name) font-name))

  (defun my-set-font (face family &optional height weight width)
    "Set font for FACE to FAMILY at the given HEIGHT, WEIGHT and WIDTH"
    (set-face-attribute face nil
                        :family family
                        :height (or height 110)
                        :weight (or weight 'normal)
                        :width (or width 'normal)))

  ;; set fixed pitch font
  (let* ((my-font-priority-list init-ui-font-fixed-pitch-list)
         (my-font (cl-some #'my-font-exists my-font-priority-list))
         (is-darwin (eq system-type 'darwin)))
    (when my-font
      (when (eq 'fixed-pitch init-ui-font-default)
        (my-set-font 'default my-font (if is-darwin 150 110) nil nil))
      (my-set-font 'fixed-pitch my-font (if is-darwin 150 110) nil nil)
      (my-set-font 'mode-line my-font (if is-darwin 120 90) nil nil)
      (my-set-font 'mode-line-inactive my-font (if is-darwin 120 90) nil nil)))

  ;; set variable pitch font
  (let* ((my-font-priority-list init-ui-font-variable-pitch-list)
         (my-font (cl-some #'my-font-exists my-font-priority-list))
         (is-darwin (eq system-type 'darwin)))
    (when my-font
      (when (eq 'variable-pitch init-ui-font-default)
        (my-set-font 'default my-font (if is-darwin 150 110) nil nil))
      (my-set-font 'variable-pitch my-font (if is-darwin 150 110) nil nil)))

  ;; enable ligatures, only works on Emacs Mac Port by Mitsuharu
  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode))

  ;; unbind helper functions
  (fmakunbound 'my-font-exists)
  (fmakunbound 'my-set-font))

(provide 'init-ui-font)

;;; init-ui-font.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
