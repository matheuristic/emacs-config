;;; init-linter.el --- Emacs config linter layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Lint code using Flymake

;;; Code:

(require 'init-ui-hydra)

;; Flymake, "C-h ." shows error at point in minibuffer
(use-package flymake
  :ensure nil ;; built-in
  :bind (:map flymake-mode-map
         ("C-c C-M-e e" . my-hydra/flymake/body)
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! l" . my-toggle-flymake-diagnostics))
  :hook (emacs-lisp-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5 ;; auto check buffer change wait time
        flymake-start-on-save-buffer nil) ;; don't run checks when saving
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  (use-package flymake-diagnostic-at-point
    :hook (flymake-mode . flymake-diagnostic-at-point-mode)
    :config (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer ;; show error msg in minibuffer
                  flymake-diagnostic-at-point-error-prefix ""))
  ;; Shorter Flymake mode-line symbol
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
    "Flymake"
    ("p" flymake-goto-prev-error "prev-err")
    ("n" flymake-goto-next-error "next-err")
    ("l" my-toggle-flymake-diagnostics "list")
    ("s" flymake-start "start-check")
    ("q" nil "quit" :exit t))
  (with-eval-after-load 'minions
    (add-to-list 'minions-direct 'flymake-mode)))

;; Macro for defining Flymake backends with less boilerplate code
(use-package flymake-quickdef
  :after flymake
  :config
  ;; Code security analysis using devskim, https://github.com/microsoft/DevSkim
  ;; To use in Flymake, call `flymake-devskim-setup' and enable Flymake mode, e.g.
  ;;   (add-hook 'python-mode-hook 'flymake-devskim-setup)
  ;;   (add-hook 'python-mode-hook 'flymake-mode)
  (flymake-quickdef-backend flymake-devskim-backend
    :pre-let ((devskim-exec (executable-find "devskim")))
    :pre-check (unless devskim-exec (error "Cannot find devskim executable"))
    :write-type 'file
    :proc-form (list devskim-exec "analyze" "-f" "text" "-o" "%L:%C: %S : [%R] %N" fmqd-temp-file)
    :search-regexp "\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\) : \\(.+\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (lcol (string-to-number (match-string 2)))
                            (severity (downcase (match-string 3)))
                            (msg (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum lcol))
                            (beg (car pos))
                            (end (cdr pos))
                            ;; For more info on the different severity types, see
                            ;; https://github.com/microsoft/DevSkim/wiki/Rule-Object-Schema
                            (type (cond
                                    ((string= severity "critical") :error)
                                    ((string= severity "important") :error)
                                    ((string= severity "moderate") :warning)
                                    ((string= severity "best-practice") :note)
                                    ((string= severity "manual-review") :note)
                                    (t :note))))
                       (list fmqd-source beg end type msg)))
  (defun flymake-devskim-setup ()
    "Enable devskim backend for Flymake."
    (add-hook 'flymake-diagnostic-functions #'flymake-devskim-backend nil t)))


(provide 'init-linter)

;;; init-linter.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
