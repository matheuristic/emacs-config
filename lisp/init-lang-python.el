;;; init-lang-python.el --- Emacs config Python language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Python tooling

;;; Code:

(require 'init-ui-hydra)

;; show function at point
(with-eval-after-load 'which-func
  (add-to-list 'which-func-modes 'python-mode))

;; add Imenu index to menubar
(with-eval-after-load 'imenu
  (add-hook 'python-mode-hook 'imenu-add-menubar-index))

;; enable Python evaluation in Org code blocks
(use-package ob-python
  :ensure nil ;; built-in
  :defer t)

;; lsp-mode client for MS Python LS, https://github.com/emacs-lsp/lsp-python-ms
(use-package lsp-python-ms
  :defer t
  :hook (python-mode . (lambda ()
                         ;; load packages if deferred
                         (require 'lsp-mode)
                         (require 'lsp-python-ms)
                         ;; start LSP client
                         (lsp)))
  :config (with-eval-after-load 'dap-mode
            (require 'dap-python)))

;; manage python envs, prefer conda to virtualenvwrapper
(if (executable-find "conda")
    (use-package conda
      :config
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      ;; display current conda env in the mode line
      (add-to-list 'mode-line-misc-info
                   '(:eval (if conda-env-current-name
                               (format " «%s»"
                                       (truncate-string-to-width
                                         conda-env-current-name
                                         15 nil nil "…"))
                             ""))
                   t)
      (defhydra my-hydra/conda (:color teal :columns 4)
        "conda"
        ("a" conda-env-activate "activate")
        ("d" conda-env-deactivate "deactivate")
        ("l" conda-env-list "list")
        ("q" nil "quit"))
      (global-set-key (kbd "C-c C-M-v e") 'my-hydra/conda/body))
  (use-package virtualenvwrapper
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    ;; display current venv in the mode line
    (add-to-list 'mode-line-misc-info
                 '(:eval (if venv-current-name
                             (format " «%s»"
                                     (truncate-string-to-width
                                       venv-current-name
                                       15 nil nil "…"))
                           ""))
                 t)
    (defhydra my-hydra/virtualenv (:color teal :columns 4)
      "virtualenv"
      ("w" venv-workon "workon")
      ("d" venv-deactivate "deactivate")
      ("m" venv-mkvirtualenv-using "make")
      ("r" venv-rmvirtualenv "remove")
      ("l" venv-lsvirtualenv "list")
      ("g" venv-cdvirtualenv "cd")
      ("c" venv-cpvirtualenv "cp")
      ("q" nil "quit"))
    (global-set-key (kbd "C-c C-M-v e") 'my-hydra/virtualenv/body)))

;; add security analysis linting using devskim (requires it be installed)
(with-eval-after-load 'flymake
  (if (executable-find "devskim")
      ;; Flymake mode is loaded by lsp-mode
      (add-hook 'python-mode-hook 'flymake-devskim-setup)))

(provide 'init-lang-python)

;;; init-lang-python.el ends here
