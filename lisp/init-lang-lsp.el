;;; init-lang-lsp.el --- Emacs config LSP layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up Language Server Protocol and Debug Adaptor Protocol tooling

;;; Code:

;; Language Server Protocol
(use-package lsp-mode
  :pin "MELPA"
  :defer t
  :hook (prog-mode . (lambda () (require 'lsp-mode)))
  :bind (:map lsp-mode-map
         ("C-c C-M-l" . my-hydra/lsp/body))
  :config
  (setq lsp-print-io nil ;; disable logging of packets between emacs and the LS
        lsp-eldoc-enable-hover nil ;; don't have eldoc display hover info
        lsp-eldoc-enable-signature-help nil ;; display signature help in minibuffer
        lsp-eldoc-prefer-signature-help nil ;; prefer displaying signature help to hover
        lsp-eldoc-render-all nil ;; don't show all returned from document/onHover, only symbol info
        lsp-enable-on-type-formatting nil ;; don't have the LS automatically format the document when typing
        lsp-prefer-flymake t ;; prefer flymake for syntax checking
        lsp-signature-auto-activate nil) ;; don't automatically show signature, use C-S-SPC to peek; see https://github.com/emacs-lsp/lsp-mode/issues/1223
  ;; user interface modules for lsp-mode
  (use-package lsp-ui
    :pin "MELPA"
    :after lsp-mode
    :config
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-always-show t
          lsp-ui-peek-enable t ;; VSCode alike
          lsp-ui-sideline-enable nil)
    ;; redefine xref-find-definitions and xref-find-references with lsp-mode equivs
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
  ;; company backend for LSP-driven completion
  (use-package company-lsp
    :pin "MELPA"
    :commands company-lsp
    :init (setq company-lsp-cache-candidates t))
  ;; hydras adapted from lsp-mode's default command map, see lsp-command-map in
  ;; https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el
  (defhydra my-hydra/lsp (:color teal :columns 3)
    "Language Server"
    ("s" my-hydra/lsp-session/body "→ Session")
    ("=" my-hydra/lsp-format/body "→ Format")
    ("F" my-hydra/lsp-folder/body "→ Folder")
    ("T" my-hydra/lsp-toggle/body "→ Toggle")
    ("g" my-hydra/lsp-goto/body "→ Goto")
    ("h" my-hydra/lsp-help/body "→ Help")
    ("r" my-hydra/lsp-refactor/body "→ Refactor")
    ("a" my-hydra/lsp-actions/body "→ Actions")
    ("G" my-hydra/lsp-peek/body "→ Peek")
    ("q" nil "quit"))
  (defhydra my-hydra/lsp-session (:color teal :columns 3)
    "Language Server → Session"
    ("r" (condition-case nil (lsp-restart-workspace) (error (lsp))) "(re-)start")
    ("s" lsp-workspace-shutdown "shutdown")
    ("d" lsp-describe-session "describe")
    ("D" lsp-disconnect "disconnect")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-format (:color teal :columns 3)
    "Language Server → Format"
    ("=" lsp-format-buffer "buffer")
    ("r" lsp-format-region "range")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-folder (:color teal :columns 3)
    "Language Server → Folder"
    ("a" lsp-workspace-folders-add "add")
    ("r" lsp-workspace-folders-remove "remove")
    ("b" lsp-workspace-blacklist-remove "un-blacklist")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-toggle (:color amaranth :columns 3)
    "Language Server → Toggle"
    ("l" lsp-lens-mode "lens-mode")
    ("L" lsp-toggle-trace-io "trace-io")
    ("h" lsp-toggle-symbol-highlight "symbol-highlight")
    ("S" lsp-ui-sideline-mode "lsp-ui-sideline-mode")
    ("d" lsp-ui-doc-mode "lsp-ui-doc-mode")
    ("s" lsp-toggle-signature-auto-activate "signature-help")
    ("f" lsp-toggle-on-type-formatting "on-type-formatting")
    ("T" lsp-treemacs-sync-mode "treemacs")
    ("q" my-hydra/lsp/body "←" :exit t))
  (defhydra my-hydra/lsp-goto (:color teal :columns 3)
    "Language Server → Goto"
    ("g" lsp-find-definition "definition")
    ("r" lsp-find-references "references")
    ("i" lsp-find-implementation "implementation")
    ("t" lsp-find-type-definition "type-implementation")
    ("d" lsp-find-declaration "declaration")
    ("h" lsp-treemacs-call-hierarchy "call-hierarchy")
    ("a" xref-find-apropos "workspace-symbol")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-help (:color teal :columns 3)
    "Language Server → Help"
    ("h" lsp-describe-thing-at-point "describe")
    ("s" lsp-signature-activate "signature-activate")
    ("g" lsp-ui-doc-glance "lsp-ui-doc-glance")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-refactor (:color teal :columns 3)
    "Language Server → Refactor"
    ("r" lsp-rename "rename")
    ("o" lsp-organize-imports "organize-imports")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-actions (:color teal :columns 3)
    "Language Server → Actions"
    ("a" lsp-execute-code-action "execute-code-action")
    ("l" lsp-avy-lens "avy-lens")
    ("h" lsp-document-highlight "document-highlight")
    ("q" my-hydra/lsp/body "←"))
  (defhydra my-hydra/lsp-peek (:color teal :columns 3)
    "Language Server → Peek"
    ("g" lsp-ui-peek-find-definitions "definition")
    ("r" lsp-ui-peek-find-references "references")
    ("i" lsp-ui-peek-find-implementation "implementation")
    ("s" lsp-ui-peek-find-workspace-symbol "workspace-symbol")
    ("q" my-hydra/lsp/body "←")))

;; front-end for interacting with debug servers
(use-package dap-mode
  :pin "MELPA"
  :commands dap-mode
  :after lsp-mode
  :bind (:map dap-mode-map
         ("C-c C-M-d b" . my-hydra/dap/body))
  :init
  (require 'dap-hydra)
  (require 'dap-ui)
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (defhydra my-hydra/dap (:color teal :columns 4)
    "Debug Adapter Protocol"
    ;; session management
    ("dd" dap-debug "debug")
    ("dl" dap-debug-last "debug-last")
    ("dr" dap-debug-recent "debug-recent")
    ("A" dap-delete-all-sessions "del-all-sessions")
    ;; windows
    ("wo" dap-go-to-output-buffer "output-buf")
    ("wb" dap-ui-breakpoints "breakpoints")
    ("wl" dap-ui-locals "locals")
    ("ws" dap-ui-sessions "sessions")
    ;; repl
    ("'" dap-ui-repl "repl")
    ;; dap-mode operations
    ("." dap-hydra "dap-hydra")
    ("q" nil "quit")))

(provide 'init-lang-lsp)

;;; init-lang-lsp.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
