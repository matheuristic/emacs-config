;;; init-lang-syntax.el --- Emacs config syntax checking layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure Flymake as the default syntax checker

;;; Code:

(require 'init-ui-hydra)

;; Flymake, C-h . shows error at point in minibuffer
(use-package flymake 
  :ensure nil ;; built-in
  :bind (:map flymake-mode-map
         ("C-c C-M-e e" . my-hydra/flymake/body))
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5 ;; auto-check buffer change wait time
        flymake-start-on-save-buffer nil) ;; don't do check on save
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake) 
  (use-package flymake-quickdef) ;; macro for quick Flymake backend defns
  (use-package flymake-diagnostic-at-point
    :hook (flymake-mode . flymake-diagnostic-at-point-mode)
    :config (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer ;; display error msg in minibuffer
                  flymake-diagnostic-at-point-error-prefix "» "))
  ;; Compress Flymake mode-line name
  (defun my-flymake-modeline-filter (ret)
    "Filter function for `flymake--mode-line-format`."
    (setf (seq-elt (car ret) 1) " FlyM")
    ret)
  (advice-add #'flymake--mode-line-format
              :filter-return #'my-flymake-modeline-filter)
  (defun my-toggle-flymake-diagnostics ()
    "Toggles flymake diagnostics window for current buffer."
    (interactive)
    (if flymake-mode
        (let* ((buf-name (buffer-name (current-buffer)))
               (flymake-winds (condition-case nil
                                  (get-buffer-window-list
                                   (concat "*Flymake diagnostics for " buf-name "*"))
                                (error nil))))
          (if flymake-winds
              (dolist (wind flymake-winds) (quit-window nil wind))
            (flymake-show-diagnostics-buffer)))))
  (defhydra my-hydra/flymake (:color amaranth)
    "Error"
    ("p" flymake-goto-prev-error "previous")
    ("n" flymake-goto-next-error "next")
    ("l" my-toggle-flymake-diagnostics "list")
    ("s" flymake-start "start-check")
    ("q" nil "quit" :exit t))
  (with-eval-after-load 'minions
    (add-to-list 'minions-direct 'flymake-mode)))

(provide 'init-lang-syntax)

;;; init-lang-syntax.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
