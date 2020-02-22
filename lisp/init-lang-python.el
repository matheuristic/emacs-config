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

;; enable Python evaluation in org-mode code blocks
(use-package ob-python
  :ensure nil ;; built-in
  :defer t)

;; lsp-mode client for MS Python LS, https://github.com/emacs-lsp/lsp-python-ms
(use-package lsp-python-ms
  :pin "MELPA"
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms) ;; load package if deferred
                         (lsp)))
  :config (with-eval-after-load 'dap-mode
            (require 'dap-python)))

;; manage python envs, prefer conda to virtualenvwrapper
(if (executable-find "conda")
    (use-package conda
      :pin "MELPA"
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
    :pin "MELPA"
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

(provide 'init-lang-python)

;;; init-lang-python.el ends here
