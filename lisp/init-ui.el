;;; init-ui.el --- Emacs config UI layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure user interface

;;; Code:

;; Helper functions

(require 'cl-seq)
(require 'bind-key)

(with-eval-after-load 'ibuffer
  (defvar ibuffer-saved-filter-groups
          ;; files are grouped by the first matching filter group in the list
          '(("default"
             ("Emacs" (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")))
             ("Shell" (or (mode . eshell-mode)
                          (mode . shell-mode)
                          (mode . term-mode)))
             ("Programming" (derived-mode . prog-mode))
             ("Web Browsing" (mode . eww-mode))
             ("Org" (or (mode . org-mode)
                        (mode . org-agenda-mode)))
             ("Magit" (or (name . "\*magit.*\\*")
                          (mode . magit-mode)))
             ("Dired" (mode . dired-mode))
             ("Help" (or (derived-mode . apropos-mode)
                         (derived-mode . help-mode)
                         (derived-mode . Info-mode)))))))

(defun my-yank-from-kill-ring ()
  "Yank from the kill ring into buffer at point or region.
Uses `completing-read' for selection, which is set by Ido, Ivy, etc."
  (interactive)
  (let ((to_insert (completing-read
                    "Yank : " (cl-delete-duplicates kill-ring :test #'equal))))
    ;; delete selected buffer region if any
    (if (and to_insert (region-active-p))
      (delete-region (region-beginning) (region-end)))
    ;; insert the selected entry from the kill ring
    (insert to_insert)))

;; Configure user interface

;; basic interface settings
(setq auto-revert-verbose nil ;; suppress auto-revert minibuffer messages
      column-number-mode t ;; show column number in mode line
      inhibit-startup-message t ;; suppress splash screen
      initial-major-mode 'fundamental-mode ;; set minimal *scratch* buffer major-mode
      ring-bell-function 'ignore ;; turn off audio and visual bells
      scroll-conservatively 101 ;; scroll a line at a time at window edge
      show-paren-delay 0) ;; no delay in show-paren-mode
(show-paren-mode t) ;; show matching parentheses

;; indent with soft tabs; use C-q <TAB> for real tabs
(setq-default indent-tabs-mode nil)

;; mouse settings
(when (display-graphic-p)
  ;; use super-left-click as middle-click (trackpad workaround)
  ;; (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
  ;; smooth scrolling in GUI, hold SHIFT/CTRL for 5 line/full window increments
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  ;; horizontal scrolling, hold SHIFT/CTRL for 5 column/full window increments
  (global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
  (global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))
  (global-set-key [S-wheel-right] (lambda () (interactive) (scroll-left 5)))
  (global-set-key [S-wheel-left] (lambda () (interactive) (scroll-right 5)))
  (global-set-key [C-wheel-right] (lambda () (interactive) (scroll-left (window-total-width))))
  (global-set-key [C-wheel-left] (lambda () (interactive) (scroll-right (window-total-width)))))

;; on Mac OS X, use Option keys as Meta and file polling for auto-revert
(when (eq system-type 'darwin)
  (setq auto-revert-use-notify nil ;; OS X does not support file notifications
        mac-option-modifier 'meta
        mac-right-option-modifier 'left))

;; enable customization of mode names in the mode line
(use-package delight)

;; framework for temporary or repeatable bindings, also defines default hydras
(require 'init-ui-hydra)

;; alternative interface for M-x
(use-package amx
  :bind ("M-X" . amx-major-mode-commands)
  :init (amx-mode))

;; show current and total search matches, and preview query replace results
(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :init (global-anzu-mode))

;; draw diagrams with the mouse; use `picture-mode' to draw with the keyboard
(use-package artist
  :if (display-graphic-p)
  :ensure nil ;; built-in
  :defer t
  :bind (("C-c C-M-a" . artist-mode)
         :map artist-mode-map
         ;; "super-click" for a context menu
         (([s-mouse-1] . artist-mouse-choose-operation))))

;; text completion framework
(use-package company
  :defer t
  :delight company-mode
  :init (with-eval-after-load 'prog-mode
          (add-hook 'prog-mode-hook 'company-mode))
  :config
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t ;; use M-<num> to directly choose completion
        company-tooltip-align-annotations t))

;; typing text replaces the active (i.e. selected) region, if any
(use-package delsel
  :ensure nil ;; built-in
  :config (delete-selection-mode))

;; Ediff
(use-package ediff
  :ensure nil ;; built-in
  :commands ediff-setup-keymap
  :init
  (defhydra my-hydra/ediff (:color teal :hint nil)
     "
Ediff

Buffer   _b_ : 2-way       _B_ : 3-way

Files    _f_ : 2-way       _F_ : 3-way       _c_ : current

Region   _l_ : line-wise   _w_ : word-wise

Windows  _L_ : line-wise   _W_ : word-wise

"
    ("b" ediff-buffers)
    ("B" ediff-buffers3)
    ("f" ediff-files)
    ("F" ediff-files3)
    ("c" ediff-current-file)
    ("l" ediff-regions-linewise)
    ("w" ediff-regions-wordwise)
    ("L" ediff-windows-linewise)
    ("W" ediff-windows-wordwise)
    ("q" nil "quit" :exit t))
  (global-set-key (kbd "C-c C-M-e d") 'my-hydra/ediff/body))

;; edit regions in separate buffers, used by other packages like markdown-mode
(use-package edit-indirect)

;; show useful information about object-at-point in minibuffer
(use-package eldoc
  :ensure nil ;; built-in
  :delight eldoc-mode)

;; expand selected region by semantic units
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

;; advanced buffer menu
(use-package ibuffer
  :ensure nil ;; built-in
  :commands ibuffer
  :hook (ibuffer-mode . (lambda ()
                          (progn (ibuffer-auto-mode 1) ;; refresh buffer after interactive commands
                                 (when ibuffer-saved-filter-groups ;; default to first saved group
                                   (ibuffer-switch-to-saved-filter-groups
                                     (car (car ibuffer-saved-filter-groups)))))))
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("C-c C-M-m" . my-hydra/ibuffer/body))
  :config
  (setq ibuffer-expert t ;; skip extraneous confirm messages
        ibuffer-show-empty-filter-groups nil)
  ;; build VC project ibuffer filter groups
  (use-package ibuffer-vc
    :after ibuffer
    :bind (:map ibuffer-mode-map
           ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))
  ;; adapted from https://github.com/abo-abo/hydra/wiki/Ibuffer
  (defhydra my-hydra/ibuffer (:color amaranth :columns 3)
    "Ibuffer"
    ;; navigation
    ("n" ibuffer-forward-line "next")
    ("p" ibuffer-backward-line "prev")
    ("RET" (condition-case nil
               (progn (ibuffer-toggle-filter-group)
                      (my-hydra/ibuffer/body))
             (error (ibuffer-visit-buffer))) "open" :exit t)
    ;; mark
    ("m" ibuffer-mark-forward "mark")
    ("u" ibuffer-unmark-forward "unmark")
    ("*" my-hydra/ibuffer-mark/body "→ Mark" :exit t)
    ;; actions
    ("S" ibuffer-do-save "save")
    ("D" ibuffer-do-delete "delete")
    ("a" my-hydra/ibuffer-action/body "→ Action" :exit t)
    ;; view
    ("g" ibuffer-update "refresh")
    ("s" my-hydra/ibuffer-sort/body "→ Sort" :exit t)
    ("/" my-hydra/ibuffer-filter/body "→ Filter" :exit t)
    ;; other
    ("o" ibuffer-visit-buffer-other-window "open-other" :exit t)
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/ibuffer-mark (:color teal :columns 5
                                   :after-exit (my-hydra/ibuffer/body))
    "Ibuffer → Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("q" nil "←"))
  (defhydra my-hydra/ibuffer-action (:color teal :columns 3
                                     :after-exit (if (eq major-mode 'ibuffer-mode)
                                                   (my-hydra/ibuffer/body)))
    "Ibuffer → Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("q" nil "←"))
  (defhydra my-hydra/ibuffer-sort (:color amaranth :columns 5)
    "Ibuffer → Sort"
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("s" ibuffer-do-sort-by-size "size")
    ("v" ibuffer-do-sort-by-recency "recency")
    ("i" ibuffer-invert-sorting "invert")
    ("q" my-hydra/ibuffer/body "←" :exit t))
  (defhydra my-hydra/ibuffer-filter (:color amaranth :columns 5)
    "Ibuffer → Filter"
    ("a" ibuffer-add-saved-filters "add-saved")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("p" ibuffer-pop-filter "pop")
    (">" ibuffer-filter-by-size-gt "size-gt")
    ("<" ibuffer-filter-by-size-lt "size-lt")
    ("&" ibuffer-and-filter "and")
    ("|" ibuffer-or-filter "or")
    ("V" ibuffer-vc-set-filter-groups-by-vc-root "vc-groups")
    ("R" ibuffer-switch-to-saved-filter-groups "saved-groups")
    ("/" ibuffer-filter-disable "disable")
    ("q" my-hydra/ibuffer/body "←" :exit t)))

;; completion framework
(use-package icomplete
  :ensure nil ;; built-in
  :bind (("C-c C-M-y y" . my-yank-from-kill-ring)
         :map icomplete-minibuffer-map
         ;; C-s and C-r cycles through completion candidates like isearch
         ("C-s" . icomplete-forward-completions)
         ("C-r" . icomplete-backward-completions)
         ;; RET selects the current completion candidate like ido
         ;; M-j uses input as is (e.g. to create new files or dirs)
         ("RET" . icomplete-force-complete-and-exit)
         ("M-j" . exit-minibuffer))
  :init (icomplete-mode)
  :config (setq icomplete-compute-delay .5
                icomplete-hide-common-prefix nil
                icomplete-prospects-height 2
                icomplete-show-matches-on-no-input t
                icomplete-tidy-shadowed-file-names t))

;; menu list of major definitions in the buffer
(use-package imenu
  :ensure nil ;; built-in
  :defer t
  :config (setq imenu-auto-rescan t))

;; menu list of major definitions across several buffers
(use-package imenu-anywhere
  :defer t
  :after imenu
  :bind ("C-c C-M-i m" . imenu-anywhere))

;; multiple cursors
(use-package multiple-cursors
  :commands my-hydra/multiple-cursors/body
  :bind ("C-c C-M-c" . my-hydra/multiple-cursors/body)
  :init (setq mc/always-run-for-all nil
              mc/always-repeat-command nil
              mc/insert-numbers-default 1)
  ;; disable prefix interpretation
  ;; see https://stackoverflow.com/questions/53798055/hydra-disable-interpretation-of-prefix-argument
  :config (defhydra my-hydra/multiple-cursors (:color pink :hint nil
                                               :base-map (make-sparse-keymap)
                                               :post (mc/keyboard-quit))
            "
Multiple-cursors

Mark    _C-<_: add-prev _C->_: add-next _C-%_: add-all  _C-s_: search
        _C-,_: skp-prev _C-._: skp-next _M-<_: rm-prev  _M->_: rm-next
        _C-|_: edit-lns _<mouse-1>_: add/rm
Misc    _C-{_: number   _C-}_: letter                 _C-g_: quit
"
            ("C-<" mc/mark-previous-like-this)
            ("C-," mc/skip-to-previous-like-this)
            ("M-<" mc/unmark-previous-like-this)
            ("C->" mc/mark-next-like-this)
            ("C-." mc/skip-to-next-like-this)
            ("M->" mc/unmark-next-like-this)
            ("C-%" mc/mark-all-like-this)
            ("C-s" mc/mark-all-in-region-regexp)
            ("<mouse-1>" mc/add-cursor-on-click)
            ("<down-mouse-1>" ignore)
            ("<drag-mouse-1>" ignore)
            ("<wheel-up>" scroll-down-line)
            ("<wheel-down" scroll-up-line)
            ("C-{" mc/insert-numbers)
            ("C-}" mc/insert-letters)
            ("C-|" mc/edit-lines)
            ("C-g" nil :exit t)))

;; support code folding
(use-package outline
  :ensure nil ;; built-in
  :defer
  :after hydra
  :init
  (defun my-outline-status-string ()
    "Returns string showing if outline-minor-mode is enabled in the current buffer."
    (if outline-minor-mode
        "yes"
      "no"))
  (defhydra+ my-hydra/visual-extras ()
    ("o" my-hydra/visual-extras/outline/body "→ Outline" :exit t))
  (defhydra my-hydra/visual-extras/outline (:color teal :hint nil
                                            :pre (require 'outline))
    "
Visual → Extras → Outline (minor-mode-enabled=%s(my-outline-status-string))

Mode    _m_ : toggle

Hide    _c_ : entry     _l_ : leaves    _d_ : subtree   _o_ : other
        _t_ : body
        
Show    _e_ : entry     _i_ : children  _k_ : branches  _s_ : subtree
        _a_ : all

"
    ("c" outline-hide-entry)
    ("l" outline-hide-leaves)
    ("d" outline-hide-subtree)
    ("t" outline-hide-body)
    ("o" outline-hide-other)
    ("e" outline-show-entry)
    ("i" outline-show-children)
    ("k" outline-show-branches)
    ("s" outline-show-subtree)
    ("a" outline-show-all)
    ("m" outline-minor-mode :exit nil)
    ("q" my-hydra/visual-extras/body "←")))

;; Paredit
(use-package paredit
  :commands paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode) ;; when in minibuffer via `eval-expression`
         (lisp-interaction-mode . paredit-mode)) ;; *scratch*
  :config
  (with-eval-after-load 'minions
    (add-to-list 'minions-direct 'paredit-mode))
  ;; make delete-selection-mode work within paredit-mode
  (with-eval-after-load 'delsel
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede)
    (put 'paredit-open-round 'delete-selection t)
    (put 'paredit-open-square 'delete-selection t)
    (put 'paredit-doublequote 'delete-selection t)
    (put 'paredit-newline 'delete-selection t))
  )

;; color code by depth
(use-package prism
  :commands (prism-mode prism-whitespace-mode)
  :init
  (defhydra my-hydra/visual-extras-prism (:color teal :columns 4)
    "Visual → Extras → Prism"
    ("p" prism-mode "lisp-c")
    ("w" prism-whitespace-mode "whitespace")
    ("q" my-hydra/visual-extras/body "←"))
  (defhydra+ my-hydra/visual-extras ()
    ("p" my-hydra/visual-extras-prism/body "prism"))
  :config (prism-set-colors :num 16
                            :desaturations (cl-loop for i from 0 below 16
                                                    collect (* i 2.5))
                            :lightens (cl-loop for i from 0 below 16
                                               collect (* i 2.5))
                            :colors (list "saddle brown" "midnight blue" "dark green")

                            :comments-fn
                            (lambda (color)
                              (prism-blend color
                                           (face-attribute 'font-lock-comment-face :foreground) 0.25))

                            :strings-fn
                            (lambda (color)
                              (prism-blend color "white" 0.5))))

;; manage system processes in Linux
(when (eq system-type 'gnu/linux)
  (use-package proced
    :ensure nil ;; built-in
    :commands proced
    :bind ("C-x p" . proced)
    :init (setq proced-format 'medium)))

;; buffer-local `auto-save-visited-mode'
;; example usage: if some directory (e.g. where files in `org-agenda-files' are
;; stored) is a cloud-enabled shared folder and external changes should be
;; reflected with files auto-saved on changes so that the buffer, local and
;; remote files are kept in sync, create a .dir-locals.el file in the folder
;; with contents
;; --
;; ;; Directory-local settings
;; ((nil . ((eval . (auto-revert-mode 1)) ;; auto-revert files
;;          (eval . (real-auto-save-mode 1))))) ;; buffer-local auto-save
;; --
(use-package real-auto-save
  :defer t
  :config (setq real-auto-save-interval 10)) ;; save interval, in seconds

;; recently opened files
(use-package recentf
  :ensure nil ;; built-in
  :commands recentf-open-files
  :bind ("C-c C-M-r f" . recentf-open-files)
  :init (setq recentf-max-menu-items 10
              recentf-max-saved-items 50)
  :config (recentf-mode t))

;; traverse undo history as a tree, default binding is "C-x u"
(use-package undo-tree
  :delight undo-tree-mode
  :init (setq undo-tree-visualizer-relative-timestamps nil)
  :config (global-undo-tree-mode))

;; display function or outline node at point
(use-package which-func
  :ensure nil ;; built-in
  :config
  (setq which-func-modes '() ;; use `which-func-mode' only for given modes
        which-func-unknown "n/a")
  (which-function-mode)
  ;; show current function in header instead of in mode line
  (defun my-narrow-to-defun-toggle ()
    "Toggle narrow to defun."
    (interactive)
    (if (buffer-narrowed-p)
        (widen)
      (narrow-to-defun)))
  (defvar my-which-func-header-keymap-default
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line s-mouse-1] 'my-narrow-to-defun-toggle) ;; trackpad workaround
      (define-key map [header-line mouse-2] 'my-narrow-to-defun-toggle)
      (define-key map [header-line wheel-up] 'beginning-of-defun)
      (define-key map [header-line wheel-down] 'end-of-defun)
      map)
    "Keymap for header line which-func.")
  (defvar my-which-func-header-keymap-help-text-default
    "mouse-2 : toggle rest visibility\n\
wheel-u : go to beginning\n\
wheel-d : go to end"
    "Help text for `my-which-fun-header-keymap-default'.")
  (defvar my-which-func-header-format-default
          `(:propertize which-func-current
                        local-map ,my-which-func-header-keymap-default
                        face which-func
                        mouse-face mode-line-highlight
                        help-echo my-which-func-header-keymap-help-text-default))
  (setq mode-line-misc-info (assq-delete-all 'which-function-mode mode-line-misc-info))
  ;; see init-org.el for an example of a mode-specific which-func header for Org-mode
  (defvar my-which-func-header-formats
    `((nil . ,my-which-func-header-format-default))
    "Association list for looking up mode-specific which-func header-lines.
Keys should be major mode symbols and values should unevaluated mode-line constructs, see https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html for more info.")
  (defun my-which-func-get-header-format ()
    "Gets `header-line-format' associated with the current major mode in `my-which-func-header-formats'."
    (cdr (or (assoc major-mode my-which-func-header-formats) ;; mode-specific
             (assoc nil my-which-func-header-formats)))) ;; default
  ;; advise which-func's file-find hook
  (defadvice which-func-ff-hook (after header-line activate)
    (when (memq major-mode which-func-modes)
      (add-to-list 'header-line-format
                   '(which-function-mode
                      (which-func-mode
                        ("[ " (:eval (my-which-func-get-header-format)) " ]")))))))

;; display available bindings in popup
(use-package which-key
  :delight which-key-mode
  :bind ("C-c C-M-w k" . which-key-show-top-level)
  :init
  (setq which-key-allow-multiple-replacements t
        which-key-compute-remaps t
        ;; configure for manual activation using C-h in the middle of a key seq
        ;; see https://github.com/justbur/emacs-which-key#manual-activation
        which-key-idle-delay 10000
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t)
  (which-key-mode 1))

;; visualize and cleanup whitespace
(use-package whitespace
  :ensure nil ;; built-in
  :commands my-hydra/whitespace/body
  :bind ("C-c C-M-w s" . my-hydra/whitespace/body)
  :config (defhydra my-hydra/whitespace (:color teal :columns 3)
            "Whitespace"
            ("w" whitespace-mode "show-whitespace" :exit nil)
            ("t" (lambda () (interactive)
                   (setq-local show-trailing-whitespace
                               (not show-trailing-whitespace)))
             "show-trailing" :exit nil)
            ("n" whitespace-newline-mode "show-newline" :exit nil)
            ("c" whitespace-cleanup "cleanup")
            ("r" whitespace-report "report")
            ("q" nil "quit")))

;; traverse window config changes, use C-c left/right to undo/redo
(use-package winner
  :ensure nil ;; built-in
  :hook (after-init . winner-mode))

;; expandable snippet template system
(use-package yasnippet
  :defer 1
  :delight yas-minor-mode
  :bind (:map yas-minor-mode-map
         ("C-c C-M-y s" . my-hydra/yasnippet/body))
  :config
  (use-package yasnippet-snippets) ;; official snippets
  (use-package auto-yasnippet) ;; enable creation of temporary snippets
  (defhydra my-hydra/yasnippet (:color teal :columns 4)
    "YASnippet"
    ("SPC" yas-expand "expand") ;; expand snippet
    ("d" yas-describe-tables "describe") ;; snippets for current mode
    ("s" yas-insert-snippet "insert") ;; insert snippet
    ("n" yas-new-snippet "new") ;; create new snippet
    ("v" yas-visit-snippet-file "visit-snippet") ;; visit snippet file
    ("w" aya-create "create-auto") ;; store temp snippet
    ("y" aya-expand "expand-auto") ;; paste temp snippet
    ("?" (message "Current auto-yasnippet:\n%s" aya-current) "current-auto") ;; show temp snippet
    ("q" nil "quit"))
  ;; remove default bindings to avoid conflicts with other packages
  ;; removing prefix bindings also removes bindings that use them
  (unbind-key "\C-c&" yas-minor-mode-map)
  (unbind-key "\C-c" yas-minor-mode-map)
  (yas-global-mode 1))

(require 'init-ui-color)
(require 'init-ui-icons)
(require 'init-ui-modeline)
(require 'init-ui-font)

(provide 'init-ui)

;;; init-ui.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
