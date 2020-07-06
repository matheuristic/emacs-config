;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Generated: Sun Jul  5 21:24:08 2020

;;; Commentary:

;; Emacs initialization configuration file, symlink or copy to
;; ~/.emacs.d/init.el or $XDG_CONFIG_HOME/.emacs.d/init.el

;; In Emacs 27+, the sequence of initialization is
;; 1. early-init.el
;; 2. package.el
;; 3. init.el

;;; Code:

;; Backward compatibility

;; backwards-compatibility code for Emacs versions <27
(when (version< emacs-version "27")
  ;; load early-initialization file ~/.emacs.d/early-init.el
  ;; Emacs 27+ automatically loads this file before rendering UI elements
  (let ((local-f (expand-file-name "early-init.el" user-emacs-directory)))
    (when (file-exists-p local-f) (load-file local-f))))

;; Customize file

;; store Customize settings in a separate file, custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'after-init-hook
          (load custom-file 'noerror))

;; Package management

;; set ELPA-compatible package repositories and their priorities
(setq package-archives '(("GNU"   . "https://elpa.gnu.org/packages/")
                         ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities '(("GNU"   . 1)
                                   ("MELPA" . 2)))

;; initialize package.el
(require 'package)
(package-initialize)

;; bootstrap use-package, provides configuration macros
;; for info, see https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key packages
;; configure imenu support for the `require' and `use-package' keywords
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t)) ;; default to ":ensure t"

;; Environment variables

;; copy environment variables from shell, OS X GUI mode-only
(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :init (if (memq window-system '(mac ns))
                (exec-path-from-shell-initialize))))

;; Backend and frontend frameworks for building user interfaces

;; enable flex completion on Emacs 27+
(when (not (version< emacs-version "27"))
  (with-eval-after-load 'minibuffer
    (add-to-list 'completion-styles 'flex t)))

(use-package helm
  :init
  (setq helm-allow-mouse t
        helm-command-prefix-key "C-c C-M-h"
        helm-prevent-escaping-from-minibuffer nil
        ;; show helm completion buffer using default display function
        ;; instead of always opening a new frame for it
        helm-show-completion-display-function #'helm-show-completion-default-display-function
        ;; show helm buffers by splitting current window instead of
        ;; taking over another window in multi-window layout
        helm-split-window-inside-p t)
  (when (version< emacs-version "27")
    (add-to-list 'completion-styles 'helm-flex t))
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; bind over the standard Emacs commands
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  ;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap switch-to-buffer] 'helm-mini)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (define-key global-map [remap apropos-command] 'helm-apropos)
  ;; make <tab> only complete names during helm completion, instead of
  ;; default behavior that creates new buffer on the second press
  ;; after which a third press kills the newly created buffer
  (setq helm-ff-kill-or-find-buffer-fname-fn #'ignore)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action))

(use-package helm-treemacs-icons
  :after helm
  :ensure nil ;; in site-lisp subdirectory
  :config (helm-treemacs-icons-enable))

;; ;; use Icomplete as the completion backend
;; ;; emulate ido behavior where possible
;; (if (version< emacs-version "27")
;;     ;; no `fido-mode' on older Emacs versions
;;     (progn
;;       (setq completion-category-defaults nil
;;             icomplete-compute-delay 0
;;             icomplete-hide-common-prefix nil
;;             icomplete-prospects-height 2
;;             icomplete-show-matches-on-no-input t
;;             icomplete-tidy-shadowed-file-names t)
;;       (icomplete-mode)
;;       ;; C-s and C-r cycles through completion candidates like isearch
;;       (define-key icomplete-minibuffer-map (kbd "C-s")
;;         #'icomplete-forward-completions)
;;       (define-key icomplete-minibuffer-map (kbd "C-r")
;;         #'icomplete-backward-completions)
;;       ;; RET selects current completion candidate like ido
;;       ;; M-j uses input as is, e.g. to create new files or new dirs
;;       (define-key icomplete-minibuffer-map (kbd "RET")
;;         #'icomplete-force-complete-and-exit)
;;       (define-key icomplete-minibuffer-map (kbd "M-j")
;;         #'exit-minibuffer))
;;   ;; enable `fido-mode'
;;   (fido-mode))

;; framework for defining temporary, repeatable bindings
;; see https://github.com/abo-abo/hydra
(use-package hydra
  :demand t)

;; text completion framework
(use-package company
  :defer t
  :init (with-eval-after-load 'prog-mode
          (add-hook 'prog-mode-hook 'company-mode))
  :config
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t ;; use M-<num> to directly choose completion
        company-tooltip-align-annotations t))

;; edit regions in separate buffers, used by other packages like markdown-mode
(use-package edit-indirect)

;; Visual (part 1)

;; font icons
(when (display-graphic-p)
  (use-package all-the-icons
    :config (setq all-the-icons-color-icons nil
                  ;; workaround for doom-modeline getting truncated in certain conditions
                  ;; https://github.com/hlissner/doom-emacs/issues/2967#issuecomment-619319082
                  all-the-icons-scale-factor 1.1)))

;; set custom mode line in graphical Emacs
(when (display-graphic-p)
  ;; fast and fancy minimalist mode line, requires all-the-icons be installed
  (use-package doom-modeline
    :after all-the-icons
    :config
    (setq doom-modeline-buffer-file-name-style 'buffer-name
          doom-modeline-env-version nil
          doom-modeline-height 23 ;; change this based on mode-line face height
          doom-modeline-icon (display-graphic-p)
          doom-modeline-irc nil
          doom-modeline-minor-modes t
          doom-modeline-persp-name nil
          doom-modeline-unicode-fallback t)
    ;; workaround for modeline getting truncated in certain conditions
    ;; https://github.com/hlissner/doom-emacs/issues/2967#issuecomment-619319082
    (doom-modeline-def-modeline 'main
      '("  " workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
      '(objed-state misc-info grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker "  "))
    ;; hide left margin indicator bar
    (set-face-background 'doom-modeline-bar
                         (face-background 'mode-line))
    (set-face-background 'doom-modeline-bar-inactive
                         (face-background 'mode-line-inactive))
    ;; enable mode line
    (doom-modeline-mode 1)))

(if (display-graphic-p)
    ;; hide minor modes in a menu, access with mouse or `minions-minor-mode-menu'
    (use-package minions
      :init
      ;; modes in minions-direct are always shown
      ;; use UTF-8 mode line lighter
      (setq minions-direct '(overwrite-mode view-mode)
            minions-mode-line-lighter "☰")
      (minions-mode 1)))

;; Backups

;; backup files to ~/.backup/
(let ((backup-dir (expand-file-name "~/.backup/")))
  (when (not (file-directory-p backup-dir))
    (make-directory backup-dir t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        version-control t ;; use version numbers for backups
        kept-new-versions 3 ;; number of newest versions to keep
        kept-old-versions 0 ;; number of oldest versions to keep
        delete-old-versions t ;; don't ask before deleting old backups
        backup-by-copying t)) ;; backup by copying instead of renaming

;; Bookmarks and history

(defhydra my-hydra/bookmarks (:color teal :columns 3)
  "
Bookmarks (_q_: quit)"
  ("q" nil nil)
  ("s" bookmark-set "set")
  ("d" bookmark-delete "delete")
  ("l" list-bookmarks "list")
  ("j" bookmark-jump "jump")
  ("i" bookmark-insert "insert")
  ("I" bookmark-insert-location "insert-loc")
  ("L" bookmark-load "load")
  ("W" bookmark-write "write"))
(global-set-key (kbd "C-c C-M-b m") 'my-hydra/bookmarks/body)

;; alternative interface for M-x
(when (not (featurep 'helm))
  (use-package amx
    :bind ("M-X" . amx-major-mode-commands)
    :init (amx-mode)))

;; recently opened files
(setq recentf-max-menu-items 10
      recentf-max-saved-items 50
      recentf-auto-cleanup 'mode) ;; clean up recent list when turning on mode
(recentf-mode 1)
;; exclude source code files in installed packages from ELPA-compatible repos
(add-to-list 'recentf-exclude
             (concat "^" (expand-file-name "elpa/" user-emacs-directory)))
;; exclude files opened with SSH so TRAMP is not spammed with stat calls
;; exclude files opened as the superuser with su or sudo
(add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
;; exclude files from /var/folder as these are temp files
(add-to-list 'recentf-exclude "^/var/folders")
;; exclude files in `org-agenda-files' and `notdeft-directories'
;; these files are quickly accessible from their respective tooling
(add-hook 'after-init-hook
          (lambda ()
            (dolist (file-list (list org-agenda-files
                                     notdeft-directories))
              (dolist (exclude-file file-list)
                (add-to-list 'recentf-exclude (concat "^" exclude-file))))))

;; binding for recentf, use Helm version if available
(global-set-key (kbd "C-c C-M-r f") #'recentf-open-files)

;; prefer helm-recentf to recentf-open-files
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'helm)
              (define-key global-map [remap recentf-open-files]
                'helm-recentf))))

(save-place-mode 1)

;; save minibuffer and other history across sessions
;; don't persist kill-ring if in the habit of copy-pasting passwords
(setq history-delete-duplicates t
      history-length 100
      savehist-additional-variables '(Info-history-list
                                      ;; kill-ring
                                      kmacro-ring
                                      regexp-search-ring
                                      register-alist
                                      last-kbd-macro
                                      search-ring
                                      shell-command-history))

;; enable save history mode
(savehist-mode 1)

;; Buffers, windows, frames, workspaces / Buffer management

;; bury these buffers on kill command instead of killing them
(setq my-unkillable-buffers '("*scratch*"
                              "*Messages*"))
(defun my-bury-unkillable-buffers ()
  "Buries the current buffer if it is unkillable, otherwise return t."
  (if (member (buffer-name) my-unkillable-buffers)
      (progn
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions #'my-bury-unkillable-buffers)

;; hydra for basic buffer management
(defhydra my-hydra/buffer (:color amaranth :columns 5)
  "
Buffer (_q_: quit)"
  ("q" nil nil :exit t)
  ("p" previous-buffer "previous")
  ("n" next-buffer "next")
  ("R" revert-buffer "revert")
  ("B" bury-buffer "bury")
  ("U" unbury-buffer "unbury")
  ("s" save-buffer "save")
  ("S" save-some-buffers "save-all")
  ("k" kill-this-buffer "kill")
  ("K" kill-matching-buffers "kill-match")
  ("b" switch-to-buffer "switch" :exit t))
(global-set-key (kbd "C-c C-M-b f") 'my-hydra/buffer/body)

;; advanced buffer management with Ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            ;; refresh buffer after interactive commands
            ;; default to first saved group
            (progn (ibuffer-auto-mode 1)
                   (when ibuffer-saved-filter-groups
                     (ibuffer-switch-to-saved-filter-groups
                      (car (car ibuffer-saved-filter-groups)))))))
(setq ibuffer-expert t ;; skip extraneous confirm messages
      ibuffer-show-empty-filter-groups nil)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; configure Ibuffer filter groups
(with-eval-after-load 'ibuffer
  (setq ibuffer-saved-filter-groups
        ;; files are grouped by the first matching filter group in the list
        '(("default"
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
           ("Programming" (derived-mode . prog-mode))
           ("Shell" (or (mode . eshell-mode)
                        (mode . shell-mode)
                        (mode . term-mode)
                        (name . "^vterm .*")))
           ("Org" (or (derived-mode . org-mode)
                      (mode . org-agenda-mode)))
           ("Text" (derived-mode . text-mode))
           ("Dired" (mode . dired-mode))
           ("Web" (or (mode . eww-mode)
                      (mode . eww-bookmark-mode)))
           ("Magit" (or (name . "\*magit.*\\*")
                        (mode . magit-mode)))
           ("Help" (or (derived-mode . apropos-mode)
                       (derived-mode . help-mode)
                       (derived-mode . Info-mode)))))))

;; build VC project ibuffer filter groups
(use-package ibuffer-vc
  :after ibuffer
  :bind (:map ibuffer-mode-map
         ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

;; hydras for Ibuffer commands
;; adapted from https://github.com/abo-abo/hydra/wiki/Ibuffer
(defhydra my-hydra/ibuffer-mode (:color amaranth :columns 3)
  "
Ibuffer (_q_: quit)"
  ("q" nil nil :exit t)
  ;; navigation
  ("n" ibuffer-forward-line "next")
  ("p" ibuffer-backward-line "prev")
  ("RET" (condition-case nil
             (progn (ibuffer-toggle-filter-group)
                    (my-hydra/ibuffer-mode/body))
           (error (ibuffer-visit-buffer))) "open" :exit t)
  ;; mark
  ("m" ibuffer-mark-forward "mark")
  ("u" ibuffer-unmark-forward "unmark")
  ("*" my-hydra/ibuffer-mode/mark/body "→ Mark" :exit t)
  ;; actions
  ("S" ibuffer-do-save "save")
  ("D" ibuffer-do-delete "delete")
  ("a" my-hydra/ibuffer-mode/action/body "→ Action" :exit t)
  ;; view
  ("`" ibuffer-switch-format "format")
  ("g" ibuffer-update "refresh")
  ("s" my-hydra/ibuffer-mode/sort/body "→ Sort" :exit t)
  ("/" my-hydra/ibuffer-mode/filter/body "→ Filter" :exit t)
  ;; other
  ("o" ibuffer-visit-buffer-other-window "open-other" :exit t))
(defhydra my-hydra/ibuffer-mode/mark (:color amaranth :columns 5
                                      :after-exit (my-hydra/ibuffer-mode/body))
  "
Ibuffer → Mark (_q_: ←)"
  ("q" nil nil :exit t)
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed"))
(defhydra my-hydra/ibuffer-mode/action (:color teal :columns 3
                                        :after-exit (if (eq major-mode 'ibuffer-mode)
                                                        (my-hydra/ibuffer-mode/body)))
  "
Ibuffer → Action (_q_: ←)"
  ("q" nil nil)
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
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe"))
(defhydra my-hydra/ibuffer-mode/sort (:color amaranth :columns 5)
  "
Ibuffer → Sort (_q_: ←)"
  ("q" my-hydra/ibuffer-mode/body nil :exit t)
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("s" ibuffer-do-sort-by-size "size")
  ("v" ibuffer-do-sort-by-recency "recency")
  ("i" ibuffer-invert-sorting "invert"))
(defhydra my-hydra/ibuffer-mode/filter (:color amaranth :columns 5
                                        :pre (require 'ibuffer-vc))
  "
Ibuffer → Filter (_q_: ←)"
  ("q" my-hydra/ibuffer-mode/body nil :exit t)
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
  ("\\" ibuffer-clear-filter-groups "clear-groups")
  ("/" ibuffer-filter-disable "disable"))

;; bind Ibuffer hydra
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-c C-M-m") #'my-hydra/ibuffer-mode/body))

;; use font icons in Ibuffer
(when (display-graphic-p)
  (use-package all-the-icons-ibuffer
    :after (all-the-icons ibuffer)
    :config (all-the-icons-ibuffer-mode 1)))

;; Buffers, windows, frames, workspaces / Window management

;; traverse window config changes, C-c left/right to undo/redo
;; uncomment to not bind C-c left/right keys by default
;; (setq winner-dont-bind-my-keys t)
;; enable winner-mode at end of initialization
(add-hook 'after-init-hook #'winner-mode)

(defhydra my-hydra/window (:color amaranth :columns 3)
  "
Window (_q_: quit)"
  ("q" nil nil :exit t)
  ("u" winner-undo "winner-undo")
  ("r" winner-redo "winner-redo")
  ("n" next-window-any-frame "next")
  ("p" previous-window-any-frame "previous")
  ("v" split-window-right "split-v")
  ("s" split-window-below "split-h")
  ("<left>" windmove-left "left")
  ("<down>" windmove-down "down")
  ("<up>" windmove-up "up")
  ("<right>" windmove-right "right")
  ("S-<left>" (my-transpose-windows 'windmove-left) "transpose-l")
  ("S-<down>" (my-transpose-windows 'windmove-down) "transpose-d")
  ("S-<up>" (my-transpose-windows 'windmove-up) "transpose-u")
  ("S-<right>" (my-transpose-windows 'windmove-right) "transpose-r")
  ("-" shrink-window "shrink-v")
  ("+" enlarge-window "enlarge-v")
  ("<" shrink-window-horizontally "shrink-h")
  (">" enlarge-window-horizontally "enlarge-h")
  ("M" minimize-window "minimize")
  ("m" maximize-window "maximize")
  ("=" balance-windows "balance")
  ("_" balance-windows-area "balance-area")
  ("o" delete-other-windows "only")
  ("d" delete-window "delete")
  ("D" kill-buffer-and-window "delete-buf"))
(global-set-key (kbd "C-c C-M-w w") 'my-hydra/window/body)

(defun my-transpose-windows (selector)
  "Call SELECTOR and transpose buffers between current and selected windows."
  (let ((from-win (selected-window))
        (from-buf (window-buffer)))
    (funcall selector)
    (set-window-buffer from-win (window-buffer))
    (set-window-buffer (selected-window) from-buf)))

(defun rotate-window-buffers (rotations)
  "Rotate buffers in the windows of the current frame ROTATIONS times.
ROTATIONS can be negative, which rotates in the opposite direction."
  (interactive "P")
  (let ((num-windows (count-windows)))
    (if (not (> num-windows 1))
        (message "Only one window in the frame. Nothing to rotate.")
      (let* ((windows (window-list))
             ;; original window order properties
             (window-props (mapcar (lambda (w)
                                     `(:buffer ,(window-buffer w)
                                       :start ,(window-start w)
                                       :point ,(window-point w)))
                                   windows))
             ;; new window order after rotation
             (window-moves (mapcar
                            (lambda (k)
                              (elt windows (mod (+ k rotations) num-windows)))
                            (number-sequence 0 (1- num-windows))))
             ;; create alist for easier looping later
             (wins-props (mapcar* #'cons window-moves window-props)))
        ;; iteratively assign orig window props in new window order
        (dolist (w-p wins-props)
          (let ((win (car w-p))
                (prop (cdr w-p)))
            (set-window-buffer-start-and-point
             win
             (plist-get prop :buffer)
             (plist-get prop :start)
             (plist-get prop :point))))))))

;; add entrypoint for `rotate-window-buffers' to window management hydra
(defhydra+ my-hydra/window nil
  ("," (lambda (n) (interactive "p") (rotate-window-buffers (- n))) "rotate-l")
  ("." (lambda (n) (interactive "p") (rotate-window-buffers n)) "rotate-r"))

;; Buffers, windows, frames, workspaces / Frame management

(defhydra my-hydra/frame (:color amaranth :columns 4)
  "
Frame (_q_: quit)"
  ("q" nil nil :exit t)
  ("<up>" (lambda (n) (interactive "p") (my-move-frame-pct 0 (- n))) "move-u")
  ("<down>" (lambda (n) (interactive "p") (my-move-frame-pct 0 n)) "move-d")
  ("<left>" (lambda (n) (interactive "p") (my-move-frame-pct (- n) 0)) "move-l")
  ("<right>" (lambda (n) (interactive "p") (my-move-frame-pct n 0)) "move-r")
  ("+" (lambda (n) (interactive "p") (my-enlarge-frame 0 n)) "enlarge-v")
  ("-" (lambda (n) (interactive "p") (my-enlarge-frame 0 (- n))) "shrink-v")
  (">" (lambda (n) (interactive "p") (my-enlarge-frame n 0)) "enlarge-h")
  ("<" (lambda (n) (interactive "p") (my-enlarge-frame (- n) 0)) "shrink-h")
  ("M" toggle-frame-maximized "maximize")
  ("f" toggle-frame-fullscreen "fullscreen")
  ("p" (other-frame -1) "previous")
  ("n" other-frame "next")
  ("s" select-frame-by-name "select")
  ("m" (lambda () (interactive) (my-make-frame 15 20)) "make")
  ("d" delete-frame "delete")
  ("o" delete-other-frames "only"))
(global-set-key (kbd "C-c C-M-f") 'my-hydra/frame/body)

(defun my-enlarge-frame (w h)
  "Enlarge width, height of selected frame by W, H lines (shrink if negative)."
  (let ((this-frame (selected-frame)))
    (set-frame-width this-frame (+ (frame-width this-frame) w))
    (set-frame-height this-frame (+ (frame-height this-frame) h))))

(defun my-move-frame (x y)
  "Move selected frame by X pixels horizontally and Y pixels vertically."
  (let* ((this-frame (selected-frame))
         (fpos (frame-position this-frame)))
    (set-frame-position this-frame (+ (car fpos) x) (+ (cdr fpos) y))))

(defun my-move-frame-pct (x y)
  "Move selected frame within display by X% horizontally and Y% vertically."
  (my-move-frame (* x (/ (x-display-pixel-width) 100))
                 (* y (/ (x-display-pixel-height) 100))))

(defun my-make-frame (x y)
  "Make new frame, offset by X pixels horizontally and Y pixels vertically."
  (let ((cur-pos (frame-position)))
    (select-frame (make-frame (list (cons 'left (+ x (car cur-pos)))
                                    (cons 'top (+ y (cdr cur-pos))))))))

(use-package transpose-frame)

;; add entrypoint for transpose-frame to frame management hydra
(defhydra+ my-hydra/frame nil
  ("," (lambda (n) (interactive "p") (dotimes (_ n) (rotate-frame-anticlockwise))) "rotate-l")
  ("." (lambda (n) (interactive "p") (dotimes (_ n) (rotate-frame-clockwise))) "rotate-r"))

;; Buffers, windows, frames, workspaces / Workspace management

;; settings for desktop.el
;; desktops are saved to ~/.emacs.d/.emacs.desktop
;; and locks are saved to ~/.emacs.d/.emacs.desktop.lock
;; - enable desktop-save-mode to save on exit and load on entry;
;;   this is added to `after-init-hook' to avoid a prompt on startup
;;   warning about the desktop file being in use that occurs when
;;   `desktop-save-mode' is enabled before initialization is done,
;;   even though the Emacs process PID is the owner of the lock file;
;;   might be specific to emacs-mac port
;; - set `desktop-autosave-timeout' to nil to disable timer auto-saves
;; - restore frames to their original displays
;; - don't re-use frames
(setq desktop-auto-save-timeout nil
      desktop-restore-in-current-display nil
      desktop-restore-reuses-frames t)
(add-hook 'after-init-hook (lambda () (desktop-save-mode 1)))

;; hydra for workspace management
(defhydra my-hydra/workspace (:color teal :columns 3)
  "
Workspace (_q_: quit)"
  ("q" nil nil)
  ("dc" desktop-clear "desktop-clear")
  ("ds" desktop-save "desktop-save")
  ("dr" desktop-read "desktop-read")
  ("dR" desktop-revert "desktop-revert")
  ("dd" desktop-change-dir "desktop-dir"))

;; binding for workspace management hydra
(global-set-key (kbd "C-c C-M-e w") 'my-hydra/workspace/body)

;; Command-line interaction

(setq eshell-history-size 1024
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-where-to-jump 'begin)
(require 'em-smart)

;; enable Eshell to spawn visual commands inside
(require 'em-term)
;; run visual commands and subcommands in term sessions
(dolist (cmd '("htop" "lftp" "ssh" "vi" "vim" "watch"))
  (add-to-list 'eshell-visual-commands cmd))
(dolist (subcmd '(("tail" "-f" "-F")
                  ("sudo" "vi" "vim")
                  ("vagrant" "ssh")))
  (add-to-list 'eshell-visual-subcommands subcmd))

;; ensure Git does not launch a pager for easier usage with eshell
(setenv "GIT_PAGER" "")

;; adapted from https://arte.ebrahimi.org/blog/named-eshell-buffers
(defun my-eshell-with-name ()
  "Prompts for the name of a eshell buffer to open or switch to.
If the NAME given at the prompt is not an existing eshell buffer,
a new one named *eshell*<NAME> will be opened. If no name is
provided, the default interactive `eshell' command is run."
  (interactive)
  (let* ((my-es-bufs (seq-filter
                      (lambda (buf)
                        (string-match-p "*eshell*" (buffer-name buf)))
                      (buffer-list)))
         (my-es-buf-name-list (mapcar #'buffer-name my-es-bufs))
         (my-es-buf-name (completing-read
                          "Eshell Buffer : " my-es-buf-name-list)))
    (if (member my-es-buf-name (mapcar #'buffer-name (buffer-list)))
        (switch-to-buffer my-es-buf-name)
      (if (string= "" my-es-buf-name)
          (eshell)
        (progn
          (eshell 42)
          (rename-buffer (concat "*eshell*<" my-es-buf-name ">")))))))

;; history autosuggestions
;; <right> or C-f completes fully, <M-right> or M-f completes partially
(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

(when (featurep 'helm)
  (add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))))

(when (and (executable-find "fish") (featurep 'helm))
  (use-package helm-fish-completion
    :config
    (setq helm-esh-pcomplete-build-source-fn
          #'helm-fish-completion-make-eshell-source)
    (with-eval-after-load 'shell
      (define-key shell-mode-map (kbd "<tab>") #'helm-fish-completion))
    (add-hook 'eshell-mode-hook
              (lambda ()
                (define-key eshell-mode-map (kbd "<tab>")
                  #'helm-fish-completion)))))

;; ;; extend pcomplete with fish shell
;; (when (executable-find "fish")
;;   (use-package fish-completion
;;     :after eshell
;;     :config (when (not (featurep 'helm))
;;               (add-hook 'eshell-mode-hook #'fish-completion-mode))))

;; make shell prompts read-only
(setq comint-prompt-read-only t)

;; kill term buffers with 'q' after session end
(defun term-handle-exit--close-buffer-on-cmd (&rest args)
  "Kill term buffer with 'q' after session exit."
   (when (null (get-buffer-process (current-buffer)))
     (use-local-map (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "q")
                        (lambda ()
                          (interactive)
                          (kill-buffer (current-buffer))))
                      map))))
(advice-add 'term-handle-exit :after #'term-handle-exit--close-buffer-on-cmd)

;; hydra for term-mode for toggling between char and line modes
(defhydra my-hydra/term-mode (:color amaranth :columns 4)
  "
Term (_q_: quit)"
  ("q" nil nil :exit t)
  ("m" (lambda () (interactive)
         (if (term-in-line-mode)
             (progn (term-char-mode) (message "line → char"))
           (progn (term-line-mode) (message "char → line")))) "toggle-mode"))
;; bindings
(with-eval-after-load 'term
  (define-key term-mode-map (kbd "C-c C-M-m") #'my-hydra/term-mode/body)
  (define-key term-raw-map (kbd "C-c C-M-m") #'my-hydra/term-mode/body))

(use-package vterm
  :if (and module-file-suffix
           (executable-find "cmake")
           (executable-find "libtool"))
  :init
  (setq vterm-buffer-name-string "vterm %s"
        vterm-clear-scrollback-when-clearing t
        vterm-eval-cmds '(("vterm-clear-scrollback" vterm-clear-scrollback))
        vterm-kill-buffer-on-exit t
        vterm-shell (executable-find "fish")))

(defun vterm--switch-to-buffer ()
  "Call `switch-to-buffer' but only for vterm buffers."
  (interactive)
  (let ((completion-regexp-list '("\\`vterm .*")))
    (call-interactively #'switch-to-buffer)))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-b") #'vterm--switch-to-buffer))

;; hydra providing entry points into shell tools
(defhydra my-hydra/shell (:color teal :columns 3)
  "
Shell tools (_q_: quit)"
  ("q" nil nil)
  ("v" vterm "vterm")
  ("V" vterm-other-window "vterm-other")
  ("C-v" vterm--switch-to-buffer "vterm-switchb")
  ("e" my-eshell-with-name "eshell"))

;; binding for spawning or switching to a named Eshell buffer
(global-set-key (kbd "C-c C-M-e s") #'my-hydra/shell/body)

;; Comparison tools

;; hydra for Ediff
(defhydra my-hydra/ediff (:color teal :hint nil)
  "
Ediff (_q_: quit)
Buffer   _b_ : 2-way       _B_ : 3-way
Files    _f_ : 2-way       _F_ : 3-way       _c_ : current
Region   _l_ : line-wise   _w_ : word-wise
Windows  _L_ : line-wise   _W_ : word-wise
"
  ("q" nil nil :exit t)
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise)
  ("L" ediff-windows-linewise)
  ("W" ediff-windows-wordwise))

;; binding for Ediff hydra
(global-set-key (kbd "C-c C-M-d f") #'my-hydra/ediff/body)

;; hydra for smerge-mode
(defhydra my-hydra/smerge-mode (:color pink :hint nil)
  "
Smerge (_q_: quit)
Move   _n_   : next          _p_ : prev
Keep   _b_   : base          _u_   : upper         _l_   : lower
       _a_   : all           _RET_ : current
Diff   _<_   : upper/base    _=_   : upper/lower   _>_   : base/lower
       _R_   : refine        _E_   : ediff
Other  _C_   : combine       _r_   : resolve       _k_   : kill current
"
  ("q" nil nil :exit t)
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current))
;; binding
(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "C-c C-M-m") #'my-hydra/smerge-mode/body))

;; view and compare directory trees, like Beyond Compare
(use-package ztree
  :bind (("C-c C-M-d z" . ztree-diff)
         ("C-c C-M-d t" . ztree-dir))
  :config
  (setq ztree-dir-move-focus t ;; RET in ztree-dir also moves focus
        ztree-draw-unicode-lines t ;; unicode lines
        ztree-show-number-of-children t)) ;; show number of files in subdir tree

;; convenience navigation bindings for `ztreedir-mode' and `ztreediff-mode'
(with-eval-after-load 'ztree-view
  (define-key ztree-mode-map (kbd "n") #'ztree-next-line)
  (define-key ztree-mode-map (kbd "p") #'ztree-previous-line))

;; mode-specific hydra for ztreedir-mode
(defhydra my-hydra/ztreedir-mode (:color pink :columns 3)
  "
ztree-dir (_q_: quit)"
  ("q" nil nil)
  ("RET" ztree-perform-action "toggle/open-other" :exit t)
  ("SPC" ztree-perform-soft-action "toggle/open" :exit t)
  ("x" ztree-toggle-expand-subtree "toggle" :exit t)
  ("g" ztree-refresh-buffer "refresh" :exit t)
  ("DEL" ztree-move-up-in-tree "goto-parent" :exit t)
  ("H" ztree-dir-toggle-show-filtered-files "show-filtered" :exit t)
  (">" ztree-dir-narrow-to-dir "narrow" :exit t)
  ("<" ztree-dir-widen-to-parent "widen" :exit t)
  ("d" ztree-dir-open-dired-at-point "dired" :exit t))
(with-eval-after-load 'ztree-dir
  (define-key ztreedir-mode-map (kbd "C-c C-M-m") #'my-hydra/ztreedir-mode/body))

;; mode-specific hydra for ztreediff-mode
(defhydra my-hydra/ztreediff-mode (:color pink :columns 3)
  "
ztree-diff (_q_: quit)"
  ("q" nil nil)
  ("RET" ztree-perform-action "toggle/ediff" :exit t)
  ("SPC" ztree-perform-soft-action "toggle/diff" :exit t)
  ("TAB" ztree-jump-side "jump-side" :exit t)
  ("x" ztree-toggle-expand-subtree "toggle" :exit t)
  ("g" ztree-refresh-buffer "refresh" :exit t)
  ("DEL" ztree-move-up-in-tree "goto-parent" :exit t)
  ("h" ztree-diff-toggle-show-equal-files "show-equal" :exit t)
  ("H" ztree-diff-toggle-show-filtered-files "show-filtered" :exit t)
  ("d" ztree-diff-simple-diff-files "diff-files" :exit t)
  ("v" ztree-diff-view-file "view" :exit t)
  ("C" ztree-diff-copy "copy" :exit t)
  ("D" ztree-diff-delete-file "delete" :exit t)
  ("r" ztree-diff-partial-rescan "rescan-part" :exit t)
  ("R" ztree-diff-full-rescan "rescan-full" :exit t))
(with-eval-after-load 'ztree-diff
  (define-key ztreediff-mode-map (kbd "C-c C-M-m") #'my-hydra/ztreediff-mode/body))

;; Dired

(require 'dired-x) ;; extra features
(require 'dired-aux) ;; even more extra features
(setq dired-dwim-target t ;; use neighboring dired buffer as default target dir
      dired-listing-switches "-alhvFG" ;; more readable file listings
      dired-omit-files (concat dired-omit-files "\\|^\\..+$") ;; omit dot files in dired-omit-mode
      dired-recursive-copies 'always ;; always copy recursively
      dired-recursive-deletes 'always) ;; always delete recursively
(add-hook 'dired-mode-hook #'auto-revert-mode) ;; auto-refresh on file change
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ;; hide details initially

;; hydras for Dired
(defhydra my-hydra/dired-mode (:color pink :columns 4)
  "
Dired (_q_: quit)"
  ("q" nil nil :exit t)
  ("RET"
   (progn
     (dired-find-file)
     (when (eq major-mode 'dired-mode)
       (my-hydra/dired-mode/body)))
   "open" :exit t)
  ("{" find-name-dired "find-name" :exit t)
  ("}" find-grep-dired "find-grep" :exit t)
  ("(" dired-hide-details-mode "toggle-details")
  (")" dired-omit-mode "toggle-omit")
  ("+" dired-create-directory "mkdir")
  ("=" dired-diff "diff" :exit t)
  ("_" dired-show-file-type "show-file-type")
  ("?" dired-summary "help")
  ("A" dired-do-find-regexp "find-regex" :exit t)
  ("C" dired-do-copy "copy")
  ("c" dired-do-compress-to "compress-to")
  ("D" dired-do-delete "delete")
  ("E" dired-mark-extension "mark-ext")
  ("F" dired-do-find-marked-files "find-marked" :exit t)
  ("G" dired-do-chgrp "chgrp")
  ("g" revert-buffer "refresh")
  ("i" dired-maybe-insert-subdir "insert-subdir")
  ("K" my-dired-kill-and-next-subdir "kill-subdir")
  ("l" dired-do-redisplay "redisplay")
  ("M" dired-do-chmod "chmod")
  ("m" dired-mark "mark")
  ("O" dired-display-file "display")
  ("o" dired-find-file-other-window "find-file-o" :exit t)
  ("Q" dired-do-find-regexp-and-replace "find-regex-sub" :exit t)
  ("R" dired-do-rename "rename")
  ("S" dired-do-symlink "symlink")
  ("s" dired-sort-toggle-or-edit "date-sort")
  ("T" dired-do-touch "touch")
  ("t" dired-toggle-marks "toggle-marks")
  ("U" dired-unmark-all-marks "unmark-all")
  ("u" dired-unmark "unmark")
  ("v" dired-view-file "view-file" :exit t) ;; open file in view-mode
  ("Y" dired-do-relsymlink "symlink-to-dir")
  ("Z" dired-do-compress "compress"))
;; binding for dired hydra
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-M-m") #'my-hydra/dired-mode/body))

;; adapted from https://www.reddit.com/r/emacs/comments/jh1me/keeping_large_dired_buffers_tidy/
(defun my-dired-kill-and-next-subdir ()
  "Kill current subdir in dired, and jump back to its parent dir."
  (interactive)
  (let* ((subdir-name (directory-file-name (dired-current-directory)))
         (parent-dir  (file-name-directory subdir-name))
         (search-term (concat " "
                              (file-name-base subdir-name)
                              (file-name-extension subdir-name t))))
    (dired-kill-subdir)
    (dired-goto-subdir parent-dir)
    (search-forward search-term)))

(use-package dired-filter
  :bind (:map dired-mode-map
         ("/" . dired-filter-map))
  :hook (dired-mode . dired-filter-mode)
  :init (setq-default dired-filter-stack nil))

;; add dired-filter hydra
(defhydra my-hydra/dired-mode/filter (:color pink :columns 4)
  "
Dired → Filter (_q_: ←)"
  ("q" my-hydra/dired-mode/body nil :exit t)
  ("n" dired-filter-by-name "by-name")
  ("r" dired-filter-by-regex "by-regex")
  ("." dired-filter-by-extension "by-ext")
  ("h" dired-filter-by-dot-files "by-hidden")
  ("o" dired-filter-by-omit "by-omit")
  ("g" dired-filter-by-garbage "by-garbage")
  ("e" dired-filter-by-predicate "by-pred")
  ("f" dired-filter-by-file "by-file")
  ("d" dired-filter-by-directory "by-dir")
  ("m" dired-filter-by-mode "by-mode")
  ("s" dired-filter-by-symlink "by-symlink")
  ("x" dired-filter-by-executable "by-exe")
  ("ig" dired-filter-by-git-ignored "by-git-ign")
  ("|" dired-filter-or "or")
  ("!" dired-filter-negate "negate")
  ("*" dired-filter-decompose "decompose")
  ("TAB" dired-filter-transpose "transpose")
  ("p" dired-filter-pop "pop")
  ("/" dired-filter-pop-all "reset")
  ("S" dired-filter-save-filters "save")
  ("D" dired-filter-delete-saved-filters "del")
  ("A" dired-filter-add-saved-filters "add")
  ("L" dired-filter-load-saved-filters "load"))
;; add entrypoint for dired-filter hydra in my-hydra/dired-mode
(defhydra+ my-hydra/dired-mode nil
  ("/" my-hydra/dired-mode/filter/body "→ Filter" :exit t))

;; use font icons in Dired
(use-package all-the-icons-dired
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (set-face-attribute 'all-the-icons-dired-dir-face nil
                      :weight 'normal)
  ;; extra workaround to avoid misalignment in filenames due to with
  ;; varying icon widths
  ;; https://github.com/jtbm37/all-the-icons-dired/issues/10
  (advice-add 'all-the-icons-dired--setup :after
              (lambda () (setq-local tab-width 2))))

;; ;; use Treemacs icons in Dired
;; (when (display-graphic-p)
;;   (use-package treemacs-icons-dired
;;     :after dired
;;     :hook (dired-mode . treemacs-icons-dired-mode)))

;; Editing text

;; indent with soft tabs; use C-q <TAB> for real tabs
(setq-default indent-tabs-mode nil)

(defun my-yank-from-kill-ring ()
  "Yank from the kill ring into buffer at point or region.
Uses `completing-read' for selection, which is set by Ido, Ivy, etc."
  (interactive)
  (let ((to-insert (completing-read
                    "Yank : " (cl-delete-duplicates kill-ring :test #'equal))))
    ;; delete selected buffer region if any
    (if (and to-insert (region-active-p))
      (delete-region (region-beginning) (region-end)))
    ;; insert the selected entry from the kill ring
    (insert to-insert)))

;; yank with completion key binding
(global-set-key (kbd "C-c C-M-y y") #'my-yank-from-kill-ring)

;; typing text replaces the active (i.e. selected) region, if any is selected
(delete-selection-mode)

;; use single spaces after sentences
(setq sentence-end-double-space nil)

;; *commented* currently do not edit GPG files much
;; ;; enable transparent editing of GPG files
;; (require 'epa-file)
;; (epa-file-enable)

(defhydra my-hydra/kmacros (:color teal :columns 3)
  "
Keyboard Macros (_q_: quit)"
  ("q" nil nil)
  ;; start, end and execute macros
  ("(" kmacro-start-macro "start")
  (")" kmacro-end-or-call-macro "end-or-call-last")
  ("r" apply-macro-to-region-lines "call-last-region")
  ;; macro ring
  ("C-n" kmacro-cycle-ring-next "cycle-ring-next" :exit nil)
  ("C-p" kmacro-cycle-ring-previous "cycle-ring-prev" :exit nil)
  ("C-v" kmacro-view-macro "view-last" :exit nil)
  ("C-d" kmacro-delete-ring-head "delete-ring-head" :exit nil)
  ;; macro editing
  ("e" edit-kbd-macro "edit")
  ("RET" kmacro-edit-macro "edit-last")
  ("l" kmacro-edit-lossage "edit-lossage")
  ("SPC" kmacro-step-edit-macro "step-edit")
  ;; naming and binding
  ("b" kmacro-bind-to-key "bind-to-key")
  ("n" kmacro-name-last-macro "name-last")
  ("x" kmacro-to-register "to-register")
  ;; other
  ("i" insert-kbd-macro "insert-named"))
(global-set-key (kbd "C-c C-M-k") 'my-hydra/kmacros/body)

(defhydra my-hydra/registers (:color teal :columns 4)
  "
Registers (_q_: quit)"
  ("q" nil nil)
  ("SPC" point-to-register "save-point")
  ("w" window-configuration-to-register "save-windows")
  ("f" frameset-to-register "save-frames")
  ("j" jump-to-register "jump")
  ("s" copy-to-register "copy-region")
  ("a" append-to-register "append-region")
  ("p" prepend-to-register "prepend-region")
  ("r" copy-rectangle-to-register "copy-rect")
  ("i" insert-register "insert")
  ("l" list-registers "list")
  ("v" view-register "view"))
(global-set-key (kbd "C-c C-M-r r") 'my-hydra/registers/body)

;; display available bindings in popup
(use-package which-key
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

;; expand selected region by semantic units
(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :init (setq iedit-toggle-key-default (kbd "C-;")))

;; multiple cursors
(use-package multiple-cursors
  :defer t
  :init (setq mc/always-run-for-all nil
              mc/always-repeat-command nil
              mc/insert-numbers-default 1))

;; hydra helper for multiple-cursors-mode
;; disable prefix interpretation when multiple-cursors-mode is active
;; see https://stackoverflow.com/questions/53798055
(defhydra my-hydra/multiple-cursors (:color pink :hint nil
                                     :base-map (make-sparse-keymap))
  "
Multiple-cursors (_C-g_: quit)
Mark    _C-<_: add-prev _C->_: add-next _C-%_: add-all  _C-s_: search
        _C-,_: skp-prev _C-._: skp-next _M-<_: rm-prev  _M->_: rm-next
        _C-|_: edit-lns _<mouse-1>_: add/remove
Misc    _C-{_: number   _C-}_: letter
"
  ("C-g" (lambda ()
           (interactive)
           (mc/keyboard-quit)
           (when multiple-cursors-mode
             (my-hydra/multiple-cursors/body))) :exit t)
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
  ("C-|" mc/edit-lines))

;; bind multiple-cursors hydra
(global-set-key (kbd "C-c C-M-c") #'my-hydra/multiple-cursors/body)

;; expandable snippet template system
(use-package yasnippet
  :defer 1 ;; load asynchronously after startup
  :config
  (use-package yasnippet-snippets) ;; official snippets
  (use-package auto-yasnippet) ;; enable creation of temporary snippets
  ;; remove default bindings to avoid conflicts with other packages
  ;; removing prefix bindings also removes bindings that use them
  (unbind-key "\C-c&" yas-minor-mode-map)
  (unbind-key "\C-c" yas-minor-mode-map)
  (yas-global-mode 1))

;; hydra for YASnippet commands
(defhydra my-hydra/yas-minor-mode (:color teal :columns 4)
  "
YASnippet (_q_: quit)"
  ("q" nil nil)
  ("SPC" yas-expand "expand") ;; expand snippet
  ("d" yas-describe-tables "describe") ;; snippets for current mode
  ("s" yas-insert-snippet "insert") ;; insert snippet
  ("n" yas-new-snippet "new") ;; create new snippet
  ("v" yas-visit-snippet-file "visit-snippet") ;; visit snippet file
  ("w" aya-create "create-auto") ;; store temp snippet
  ("y" aya-expand "expand-auto") ;; paste temp snippet
  ("?"
   (message "Current auto-yasnippet:\n%s" aya-current)
   "current-auto")) ;; show temp snippet
(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "C-c C-M-y s") #'my-hydra/yas-minor-mode/body))

;; structured editing of S-expressions with Paredit
(use-package paredit
  :commands paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
         ;; when in minibuffer via `eval-expression`
         (eval-expression-minibuffer-setup . paredit-mode)
         ;; *scratch* default mode
         (lisp-interaction-mode . paredit-mode))
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
    (put 'paredit-newline 'delete-selection t)))

;; traverse undo history as a tree, default binding is "C-x u"
(use-package undo-tree
  :init (setq undo-tree-visualizer-relative-timestamps nil)
  :config (global-undo-tree-mode))

;; Emacs as an edit server

;; server mode restart safety valve
(defun restart-emacs-server ()
  "Restarts an Emacs server."
  (interactive)
  (server-force-delete)
  (server-mode 1)
  (message "Restarted Emacs server."))

;; bind SIGUSR1 signal to call `server-restart'
(define-key special-event-map [sigusr1] #'restart-emacs-server)

;; hydra for Emacs server interaction
(defhydra my-hydra/emacs-client-server (:color teal :hint nil
                                        :pre (require 'server))
  "
Emacs client-server interaction (_q_: quit)
Server  [% 3`server-mode]   _s_ : toggle  _r_ : restart"
  ("q" nil)
  ("s" server-mode :exit nil)
  ("r" restart-emacs-server))

;; binding for Emacs server hydra
(global-set-key (kbd "C-c C-M-e c") #'my-hydra/emacs-client-server/body)

;; Email

;; configure Notmuch email client
(when (executable-find "notmuch")
  (use-package notmuch
    :ensure nil ;; in site-lisp directory
    :bind (("C-c C-M-n" . notmuch)
           :map notmuch-show-mode-map
           ("d" . notmuch-show--toggle-trash-tag)
           :map notmuch-search-mode-map
           ("d" . notmuch-search--toggle-trash-tag)
           :map notmuch-tree-mode-map
           ("d" . notmuch-tree--toggle-trash-tag))
    :init
    (setq notmuch-always-prompt-for-sender t
          notmuch-archive-tags '("-inbox")
          notmuch-hello-recent-searches-max 10
          notmuch-hello-thousands-separator "," ;; US convention
          notmuch-search-oldest-first nil ;; sort date descending
          notmuch-search-result-format `(("date" . "%12s ")
                                         ("count" . "%-7s ")
                                         ("authors" . "%-20s ")
                                         ("tags" . "%s ")
                                         ("subject" . "%s"))
          notmuch-show-logo nil
          ;; workaround for Notmuch using SVG icons when unsupported
          ;; https://emacs.stackexchange.com/questions/14875/notmuch-mode-very-slow-in-emacs-mac-port-railwaycat
          notmuch-tag-formats '(("unread"
                                 (propertize tag 'face 'notmuch-tag-unread))
                                ("flagged"
                                 (propertize tag 'face 'notmuch-tag-flagged)))
          notmuch-tree-result-format `(("date" . "%12s  ")
                                       ("authors" . "%-20s")
                                       ((("tree" . "%s")
                                         ("subject" . "%s"))
                                        . " %-54s ")
                                       ("tags" . "%s")))
    :config
    ;; toggle deletion of message from the Show view
    ;; note that in Gmail, deleted messages are marked with the "trash" label
    (defun notmuch-show--toggle-trash-tag ()
      "Toggle trash tag for message in the Show view."
      (interactive)
      (if (member "trash" (notmuch-show-get-tags))
          (notmuch-show-tag (list "-trash"))
        (notmuch-show-tag (list "+trash" "-inbox"))))
    ;; toggle deletion of thread from the Search view
    ;; note that in Gmail, deleted messages are marked with the "trash" label
    (defun notmuch-search--toggle-trash-tag (&optional beg end)
      "Toggle trash tag for thread(s) in the Search view.
If applying to a selected region, it adds or removes the trash
tag based on the entry at the beginning of the region."
      (interactive (notmuch-interactive-region))
      (if (member "trash" (notmuch-search-get-tags beg))
          (notmuch-search-tag (list "-trash") beg end)
        (notmuch-search-tag (list "+trash" "-inbox") beg end)))
    ;; toggle deletion of thread from the Tree view
    ;; note that in Gmail, deleted messages are marked with the "trash" label
    (defun notmuch-tree--toggle-trash-tag ()
      "Toggle trash tag for message in the Tree view."
      (interactive)
      (if (member "trash" (notmuch-tree-get-tags))
          (notmuch-tree-tag (list "-trash"))
        (notmuch-tree-tag (list "+trash" "-inbox"))))))

;; advise `notmuch-search-insert-authors' so that when a thread has
;; multiple authors, only the first and last message authors are
;; displayed and their names are abbreviated to fit the column width
(with-eval-after-load 'notmuch
  (defvar notmuch--abbreviate-person-name-width
    (let* ((format-string (string-trim
                           (cdr
                            (assoc "authors"
                                   notmuch-search-result-format))))
           (authors-width (string-width (format format-string ""))))
      (- (/ authors-width 2) 1))
    "Width of each author in Notmuch Search view when more than one.
Should be N/2-1, N is the width of the Search view author column.")

  (defun notmuch--abbreviate-person-name (name &optional maxlen)
    "Abbreviates a person NAME.
The result will have `notmuch--abbreviate-person-name-width'
characters or less. This is done by using the initial of the
person's first name and shortening the person's last name as
necessary; also handles emails."
    (let* ((maxlen (or maxlen notmuch--abbreviate-person-name-width))
           (split-idx (string-match-p "\[,@\]" name))
           (split-char (if split-idx
                           (substring name split-idx (+ split-idx 1))
                         "")))
      (cond ((string-equal split-char "@") ;; user.name@server.com -> u name
             (let ((name-part (substring name 0 split-idx)))
               (notmuch--abbreviate-person-name name-part)))
            (t
             ;; is-comma-split t? lastname, firstname -> f lastname
             ;; is-comma-split f? firstname lastname -> f lastname
             ;;                   OR firstname -> firstname
             (let* ((is-comma-split (string-equal split-char ","))
                    (regexp (if is-comma-split
                                "\\(.*?\\), *\\(.\\).*"
                              "\\(.\\).*?[. ]+\\(.*\\)"))
                    (replacement (if is-comma-split
                                     "\\2 \\1"
                                   "\\1 \\2"))
                    (abbrev-name (replace-regexp-in-string regexp
                                                           replacement
                                                           name))
                    (further-truncate (> (length abbrev-name)
                                         maxlen)))
               (if further-truncate
                   (concat
                    (substring abbrev-name
                               0
                               (- maxlen 2))
                    "..")
                 abbrev-name))))))

  (defun notmuch-search-insert-authors--around-abbreviate (orig-fun &rest args)
    "Advice for `notmuch-search-insert-authors' to abbreviate names.
Extracts the authors field from ARGS, abbreviates its elements
using `notmuch--abbreviate-person-name' and calls ORIG-FUN
replacing the original authors with their abbreviated names.
Assumes ', ' is used to separate authors and names are not of the
form 'Lastname, Firstname'."
    (seq-let (format-string authors) args
      (save-match-data
        (let ((author-list (mapcar (lambda (s) (replace-regexp-in-string
                                                "'" "" s)) ;; no single quotes
                                   (split-string authors ", "))))
          (if (> (length author-list) 1)
              (let* ((oldest-newest-authors (cons (car author-list)
                                                  (last author-list)))
                     (abbrev-authors
                      (mapconcat 'identity
                                 (mapcar 'notmuch--abbreviate-person-name
                                         oldest-newest-authors)
                                 ", ")))
                (apply orig-fun (list format-string abbrev-authors)))
            (apply orig-fun args))))))

  ;; abbreviate names when there are multiple authors
  (advice-add 'notmuch-search-insert-authors :around
              'notmuch-search-insert-authors--around-abbreviate))

;; notmuch extension to toggle search tag visibility in results by
;; advising the search listings field insertion function to remove
;; tags in the search query from the displayed tags except for those
;; modified after the search
(with-eval-after-load 'notmuch

  (defun notmuch--extract-search-tags (query)
    "Extracts out a list of tags from a given notmuch search QUERY.
More concretely, it identifies tokens that begin with the prefix
'is:' or 'tag:' and returns them as a list without the prefix.
Returns nil if there are no tags in the query."
    (seq-filter
     'identity
     (mapcar (lambda (x)
               (if (string-match "^\\(tag\\|is\\):\\([^ ]*\\)" x)
                   (match-string 2 x)
                 nil))
             (split-string query))))

  (defun string-equal-except (except-list s1 s2)
    "Tests if strings S1 are S2 the same, but return nil if
either is in EXCEPT-LIST."
    (if (or (member s1 except-list)
            (member s2 except-list))
        nil
      (string-equal s1 s2)))

  (defun notmuch--filter-common-search-tags (tags orig-tags query)
    "Returns '(TAGS ORIG-TAGS) with search tags in QUERY filtered out.
Only query search tags appearing in both TAGS and ORIG-TAGS are
removed."
    (let ((add-tags (cl-set-difference tags orig-tags :test 'string-equal))
          (rem-tags (cl-set-difference orig-tags tags :test 'string-equal))
          (search-tags (notmuch--extract-search-tags query)))
      (list (cl-set-difference tags
                               search-tags
                               :test (apply-partially
                                      'string-equal-except
                                      add-tags))
            (cl-set-difference orig-tags
                               search-tags
                               :test (apply-partially
                                      'string-equal-except
                                      rem-tags)))))

  (defun notmuch-search-insert-field--filter-search-tags (orig-fun &rest args)
    "Advises the `notmuch-search-insert-field' function
to filter search tags from the displayed tags like in Gmail.
ORIG-FUN should be `notmuch-search-insert-field' and ARGS are the
original arguments passed to it."
    (seq-let (field format-string result) args
      (if (string-equal field "tags")
          (let ((base-tags (plist-get result :tags))
                (base-orig-tags (plist-get result :orig-tags))
                (query (if (boundp 'notmuch-search-query-string)
                           notmuch-search-query-string
                         nil)))
            (seq-let (tags orig-tags) (notmuch--filter-common-search-tags
                                       base-tags base-orig-tags query)
             (insert (format format-string
                             (notmuch-tag-format-tags tags orig-tags)))))
        (apply orig-fun args))))

  (defun notmuch-tree-format-field--filter-search-tags (orig-fun &rest args)
    "Advises the `notmuch-tree-format-field' function
to filter search tags from the displayed tags like in Gmail.
ORIG-FUN should be `notmuch-tree-format-field' and ARGS are the
original arguments passed to it."
    (seq-let (field format-string msg) args
      (cond ((listp field) (apply orig-fun args))
            ((string-equal field "tags")
             (let ((base-tags (plist-get msg :tags))
                   (base-orig-tags (plist-get msg :orig-tags))
                   (face (if (plist-get msg :match)
                             'notmuch-tree-match-tag-face
                           'notmuch-tree-no-match-tag-face))
                   (query (if (boundp 'notmuch-tree-basic-query)
                              notmuch-tree-basic-query
                            nil)))
               (seq-let (tags orig-tags) (notmuch--filter-common-search-tags
                                          base-tags base-orig-tags query)
                 (format format-string
                         (notmuch-tag-format-tags tags orig-tags face)))))
            (t (apply orig-fun args)))))

  ;; using a global variable helps in correcting scenarios where
  ;; individual tag visibility states get misaligned
  (defvar notmuch--search-tags-visible t
    "Indicates if search tags are visible in Notmuch Tree and Search views.")

  (defun notmuch--toggle-search-tag-visibility ()
    "Toggle visibility of search tags in the Search and Tree views.
Assumes "
    (interactive)
    (let ((current-hide-search-tags
           (advice-member-p #'notmuch-search-insert-field--filter-search-tags
                            'notmuch-search-insert-field))
          (current-hide-tree-tags
           (advice-member-p #'notmuch-tree-format-field--filter-search-tags
                            'notmuch-tree-format-field)))
      ;; toggle Search view advice as needed
      (cond
       ((and current-hide-search-tags (not notmuch--search-tags-visible))
        (advice-remove 'notmuch-search-insert-field
                       #'notmuch-search-insert-field--filter-search-tags))
       ((and (not current-hide-search-tags) notmuch--search-tags-visible)
        (advice-add 'notmuch-search-insert-field :around
                    #'notmuch-search-insert-field--filter-search-tags)))
      ;; toggle Tree view advice as needed
      (cond
       ((and current-hide-tree-tags (not notmuch--search-tags-visible))
        (advice-remove 'notmuch-tree-format-field
                       #'notmuch-tree-format-field--filter-search-tags))
       ((and (not current-hide-tree-tags) notmuch--search-tags-visible)
        (advice-add 'notmuch-tree-format-field :around
                    #'notmuch-tree-format-field--filter-search-tags)))
      (setq notmuch--search-tags-visible (not notmuch--search-tags-visible))
      (notmuch-refresh-all-buffers)
      (message (if notmuch--search-tags-visible
                   "Search tags visible."
                 "Search tags hidden."))))

  ;; enable filtering of search tags in the Search and Tree views by default
  (notmuch--toggle-search-tag-visibility)

  ;; bindings to toggle visibility of search tags in the results
  (dolist (map '(notmuch-hello-mode-map
                 notmuch-search-mode-map
                 notmuch-tree-mode-map))
    (define-key map (kbd "C-t")
      #'notmuch--toggle-search-tag-visibility)))

;; provides HTML email composition using Org-mode
;; for autogreeting, set `org-msg-greeting-fmt' to "\nHi *%s*,\n\n"
(use-package org-msg
  :config
  (setq org-msg-options (concat "html-postamble:nil H:5 num:nil ^:{} "
                                "toc:nil author:nil email:nil \\n:t")
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt nil
        org-msg-greeting-name-limit 3
        org-msg-text-plain-alternative t)
  (with-eval-after-load 'notmuch
    ;; enable HTML email message composition
    (org-msg-mode 1)
    ;; bindings to toggle HTML email message composition
    (dolist (map '(notmuch-hello-mode-map
                   notmuch-search-mode-map
                   notmuch-show-mode-map
                   notmuch-tree-mode-map))
      (define-key map (kbd "M") #'org-msg-mode))))

;; major mode-specific hydra for OrgMsg edit mode
(with-eval-after-load 'org-msg
  (defhydra my-hydra/org-msg-edit-mode (:color teal :columns 6)
    "
OrgMsg (_q_: quit)"
    ("q" nil nil)
    ("f" message-goto-from "from")
    ("t" message-goto-to "to")
    ("c" message-goto-cc "cc")
    ("B" message-goto-bcc "bcc")
    ("F" message-goto-fcc "fcc")
    ("S" message-goto-subject "subj")
    ("b" org-msg-goto-body "body")
    ("C-a" org-msg-attach "attach")
    ("C-e" org-msg-preview "preview")
    ("C-c" org-ctrl-c-ctrl-c "send")
    ("C-k" org-msg-edit-kill-buffer "kill"))

  ;; binding for org-msg-edit-mode
  (define-key org-msg-edit-mode-map (kbd "C-c C-M-m")
    #'my-hydra/org-msg-edit-mode/body))

(condition-case nil
    (require 'ol-notmuch)
  (error (message "ol-notmuch requires Org 9.2.3+")))

;; Non-programming files

(with-eval-after-load 'doc-view
  (easy-menu-define my-doc-view-menu doc-view-mode-map "Menu for Doc-View Mode."
    '("DocView"
      ["Switch to a different mode" doc-view-toggle-display :help "Switch to a different mode"]
      ["Open Text" doc-view-open-text :help "Display the current doc's contents as text"]
      "--"
      ("Navigate Doc"
       ["Goto Page ..." doc-view-goto-page :help "View the page given by PAGE"]
       "--"
       ["Scroll Down" doc-view-scroll-down-or-previous-page :help "Scroll page down ARG lines if possible, else goto previous page"]
       ["Scroll Up" doc-view-scroll-up-or-next-page :help "Scroll page up ARG lines if possible, else goto next page"]
       "--"
       ["Next Line" doc-view-next-line-or-next-page :help "Scroll upward by ARG lines if possible, else goto next page"]
       ["Previous Line" doc-view-previous-line-or-previous-page :help "Scroll downward by ARG lines if possible, else goto previous page"]
       ("Customize"
        ["Continuous Off"
         (setq doc-view-continuous nil)
         :help "Stay put in the current page, when moving past first/last line" :style radio :selected
         (eq doc-view-continuous nil)]
        ["Continuous On"
         (setq doc-view-continuous t)
         :help "Goto to the previous/next page, when moving past first/last line" :style radio :selected
         (eq doc-view-continuous t)]
        "---"
        ["Save as Default"
         (customize-save-variable 'doc-view-continuous doc-view-continuous)
         t])
       "--"
       ["Next Page" doc-view-next-page :help "Browse ARG pages forward"]
       ["Previous Page" doc-view-previous-page :help "Browse ARG pages backward"]
       "--"
       ["First Page" doc-view-first-page :help "View the first page"]
       ["Last Page" doc-view-last-page :help "View the last page"])
      "--"
      ("Adjust Display"
       ["Enlarge" doc-view-enlarge :help "Enlarge the document by FACTOR"]
       ["Shrink" doc-view-shrink :help "Shrink the document"]
       "--"
       ["Fit Width To Window" doc-view-fit-width-to-window :help "Fit the image width to the window width"]
       ["Fit Height To Window" doc-view-fit-height-to-window :help "Fit the image height to the window height"]
       "--"
       ["Fit Page To Window" doc-view-fit-page-to-window :help "Fit the image to the window"]
       "--"
       ["Set Slice From Bounding Box" doc-view-set-slice-from-bounding-box :help "Set the slice from the document's BoundingBox information"]
       ["Set Slice Using Mouse" doc-view-set-slice-using-mouse :help "Set the slice of the images that should be displayed"]
       ["Set Slice" doc-view-set-slice :help "Set the slice of the images that should be displayed"]
       ["Reset Slice" doc-view-reset-slice :help "Reset the current slice"])
      ("Search"
       ["New Search ..."
        (doc-view-search t)
        :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
       "--"
       ["Search" doc-view-search :help "Jump to the next match or initiate a new search if NEW-QUERY is given"]
       ["Backward" doc-view-search-backward :help "Call `doc-view-search' for backward search"]
       "--"
       ["Show Tooltip" doc-view-show-tooltip :help nil])
      ("Maintain"
       ["Reconvert Doc" doc-view-reconvert-doc :help "Reconvert the current document"]
       "--"
       ["Clear Cache" doc-view-clear-cache :help "Delete the whole cache (`doc-view-cache-directory')"]
       ["Dired Cache" doc-view-dired-cache :help "Open `dired' in `doc-view-cache-directory'"]
       "--"
       ["Revert Buffer" doc-view-revert-buffer :help "Like `revert-buffer', but preserves the buffer's current modes"]
       "--"
       ["Kill Proc" doc-view-kill-proc :help "Kill the current converter process(es)"]
       ["Kill Proc And Buffer" doc-view-kill-proc-and-buffer :help "Kill the current buffer"])
      "--"
      ["Customize"
       (customize-group 'doc-view)]))
  (easy-menu-define my-doc-view-minor-mode-menu doc-view-minor-mode-map "Menu for Doc-View Minor Mode."
    '("DocView*"
      ["Display in DocView Mode" doc-view-toggle-display :help "View"]
      ["Exit DocView Mode" doc-view-minor-mode])))

(use-package csv-mode
    :commands csv-mode
    :bind (:map csv-mode-map
           ("C-c C-M-m" . my-hydra/csv-mode/body)
           ("C-c C-S-a" . csv-align-visible-fields))
    :config
    (setq csv-align-style 'auto) ;; `csv-align-fields' left/right-aligns text/numbers
    (defun csv-align-visible-fields ()
      "Align visible lines in `csv-mode'. Useful for large CSV files where
`csv-align-fields' can take a very long time to run."
      (interactive)
      (csv-align-fields nil (window-start) (window-end))))

;; major mode-specific hydra for csv-mode
(defhydra my-hydra/csv-mode (:color teal :columns 4)
  "
CSV (_q_: quit)"
  ("q" nil nil)
  ("s" csv-sort-fields "sort")
  ("n" csv-sort-numeric-fields "numsort")
  ("r" csv-reverse-region "reverse")
  ("d" csv-toggle-descending "toggle-desc-sort" :exit nil)
  ("t" csv-transpose "transpose")
  ("k" csv-kill-fields "cut")
  ("y" csv-yank-fields "paste")
  ("z" csv-yank-as-new-table "paste-as-new-tab")
  ("A" csv-align-visible-fields "align-visible" :exit nil)
  ("a" csv-align-fields "align" :exit nil)
  ("u" csv-unalign-fields "unalign" :exit nil)
  ("h" csv-header-line "toggle-header" :exit nil)
  ("v" csv-toggle-invisibility "toggle-invis-sep" :exit nil))

;; binding for csv-mode hydra
(with-eval-after-load 'csv-mode
  (define-key csv-mode-map (kbd "C-c C-M-m") #'my-hydra/csv-mode/body))

(use-package dockerfile-mode
  :commands dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; major mode for reading EPUBs
(use-package nov
  :init (add-to-list 'auto-mode-alist
                     '("\\.epub\\'" . nov-mode)))

;; provides a major mode for editing JSON files
(use-package json-mode
  :defer t)

;; major mode for editing Markdown files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; place header markup only at the start of a line
  ;; syntax highlighting in fenced code blocks
  ;; use underscores for italics instead of asterisks
  (setq markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-italic-underscore t)
  ;; if available, use pandoc for converting markdown files
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc --from markdown --to html"
          markdown-command-needs-filename t))
  ;; render mathematical expressions in HTML previews
  (setq markdown-xhtml-header-content
        (concat "<script type=\"text/x-mathjax-config\">"
                "MathJax.Hub.Config({"
                "  tex2jax: {"
                "    inlineMath: [ ['$','$'], [\"\\\\(\",\"\\\\)\"] ],"
                "    processEscapes: true"
                "  }"
                "});"
                "</script>"
                "<script type=\"text/javascript\" async"
                "        src=\"https://cdnjs.cloudflare.com/ajax/libs/"
                "mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML\">"
                "</script>")))

;; major mode-specific hydra for markdown-mode
(defhydra my-hydra/markdown-mode (:color teal :hint nil)
  "
Markdown mode (_q_: quit)
Keymaps     _c_ : commands  _s_ : styles
Outline     _n_ : next      _p_ : prev      _f_ : fwd-level _b_ : bwd-level
            _←_ : promote   _→_ : demote    _↓_ : move-down _↑_ : move-up
Shift-Rgn   _<_ : left      _>_ : right
Toggle      _E_ : math      _F_ : code-font _I_ : images    _L_ : url
            _M_ : markup
Other       _d_ : do        _o_ : follow    _'_ : edit code block
"
  ("q" nil nil)
  ;; keymaps
  ("c" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-c\C-c"))))
  ("s" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-c\C-s"))))
  ;; outline
  ("n" markdown-outline-next :color red)
  ("p" markdown-outline-previous :color red)
  ("f" markdown-outline-next-same-level :color red)
  ("b" markdown-outline-previous-same-level :color red)
  ("<left>" markdown-promote :color red)
  ("<right>" markdown-demote :color red)
  ("<down>" markdown-move-down :color red)
  ("<up>" markdown-move-up :color red)
  ;; shift region
  ("<" markdown-outdent-region :color red)
  (">" markdown-indent-region :color red)
  ;; user interface
  ("E" markdown-toggle-math)
  ("F" markdown-toggle-fontify-code-blocks-natively)
  ("I" markdown-toggle-inline-images)
  ("L" markdown-toggle-url-hiding)
  ("M" markdown-toggle-markup-hiding)
  ;; other
  ("d" markdown-do)
  ("o" markdown-follow-thing-at-point)
  ("'" markdown-edit-code-block))

;; bindings for markdown-mode hydra
(with-eval-after-load 'markdown-mode
  (define-key gfm-mode-map (kbd "C-c C-M-m") #'my-hydra/markdown-mode/body)
  (define-key markdown-mode-map (kbd "C-c C-M-m") #'my-hydra/markdown-mode/body))

(use-package markdown-toc
  :after markdown-mode)

;; add heads to create, update and delete tables of contents in
;; markdown-mode buffers
(with-eval-after-load 'markdown-toc
  (defhydra+ my-hydra/markdown-mode nil
    ("t" markdown-toc-generate-or-refresh-toc "insert-or-refresh-toc")
    ("C-t" markdown-toc-delete-toc "delete-toc")))

;; provides a major mode for editing YAML files
(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; neuron-mode, settings adapted from
;; https://gist.github.com/felko/cdb3fc19b3a60db27eb3c5bd319fc479
(use-package neuron-mode
  :init
  (defface neuron-stub-face
    '((((class color) (min-colors 88) (background dark)) :foreground "#C16069" :underline "#C16069")
      (((class color) (min-colors 88) (background light)) :foreground "#C16069" :underline "#C16069")
      (((class color) :foreground "Red" :underline "Red"))
      (t :inherit neuron-title-overlay-face))
    "Face for stub links."
    :group 'neuron-faces)
  (setq neuron-default-zettelkasten-directory (expand-file-name "~/zettelkasten")
        neuron-default-tags '("stub")
        neuron-id-format 'hash
        neuron-tag-specific-title-faces '(("stub" neuron-stub-face))))

;; entrypoint hydra into neuron Zettelkasten
(defhydra my-hydra/neuron (:color teal :columns 4)
  "
Neuron Zettelkasten (_q_: quit)"
  ("q" nil nil)
  ("z" neuron-new-zettel "new")
  ("e" neuron-edit-zettel "edit")
  ("o" neuron-open-zettel "open")
  ("O" neuron-open-index "index")
  ("j" neuron-open-daily-notes "daily")
  ("t" neuron-query-tags "query-tags")
  ("g" neuron-refresh "refresh")
  ("c" neuron-edit-zettelkasten-configuration "config")
  ("rw" neuron-rib-watch "rib-watch")
  ("rg" neuron-rib-generate "rib-gen")
  ("rs" neuron-rib-serve "rib-serve")
  ("ro" neuron-rib-open-zettel "rib-open")
  ("rz" neuron-rib-open-z-index "rib-z-idx")
  ("rk" neuron-rib-kill "rib-kill"))

;; binding for neuron entrypoint hydra
(with-eval-after-load 'neuron-mode
  (global-set-key (kbd "C-c C-M-z") #'my-hydra/neuron/body))

;; mode-specific hydra for neuron-mode
(defhydra my-hydra/neuron-mode (:color teal :columns 3)
  "
Neuron mode (_q_: quit)"
  ("q" nil nil)
  ("u" neuron-edit-uplink "edit-uplink")
  ("t" neuron-add-tag "add-tag")
  ("T" neuron-add-tags "add-tags")
  ("l" neuron-create-and-insert-zettel-link "create-link")
  ("L" neuron-create-zettel-from-selected-title "create-selected")
  ("s" neuron-insert-static-link "insert-static-link")
  ("c" neuron-toggle-connection-type "toggle-conn-type")
  ("r" neuron-open-current-zettel "open-current")
  ("o" neuron-follow-thing-at-point "follow"))

;; wrapper hydra for accessing markdown and neuron-mode hydras from
;; mode-specific entrypoint
(defhydra my-hydra/markdown-neuron-mode-wrapper (:color teal)
  "
Markdown mode hydra / Neuron mode hydra (_q_: quit)
"
  ("q" nil nil)
  ("m" my-hydra/markdown-mode/body "markdown-mode")
  ("z" my-hydra/neuron-mode/body "neuron-mode"))

;; binding for mode-specific wrapper hydra for markdown/neuron modes
(with-eval-after-load 'neuron-mode
  (add-hook 'neuron-mode-hook
            (lambda ()
              (use-local-map (copy-keymap markdown-mode-map))
              (local-set-key (kbd "C-c C-M-m")
                             #'my-hydra/markdown-neuron-mode-wrapper/body))))

;; Org-mode

;; set Org directory and inbox file
(setq org-directory (file-name-as-directory (file-truename "~/org"))
      my-org-agenda-inbox (concat org-directory "inbox.org"))

;; basic Org-mode settings
(setq org-adapt-indentation nil ;; don't auto-indent when promoting/demoting
      org-catch-invisible-edits 'error
      org-confirm-babel-evaluate nil ;; don't confirm before evaluating code blocks in Org documents
      org-edit-src-content-indentation 2
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-hide-emphasis-markers nil
      org-hide-leading-stars t
      org-highlight-latex-and-related '(latex script entities) ;; highlight LaTeX fragments with the `org-highlight-latex-and-related' face
      org-image-actual-width (list (/ (display-pixel-width) 3)) ;; auto-resize displayed images to one-third of display width
      org-log-into-drawer t
      org-outline-path-complete-in-steps nil
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil ;; don't render sub/superscripts in-buffer
      org-return-follows-link t
      org-src-fontify-natively nil ;; don't syntax color org source blocks
      org-src-preserve-indentation t ;; preserve src code block indentation on export and when switching btw org buffer and edit buffer
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window ;; reuse Org file window for editing source blocks when using "C-c '"
      org-startup-folded t
      org-startup-indented nil
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-use-fast-todo-selection t
      org-use-speed-commands nil)

;; Set possible Org task states
;; Diagram of possible task state transitions
;;     -------------------------
;;     |                       |
;;     |                       v
;; -> TODO....... -> NEXT -> DONE ----->
;;    | ^  |  | ^    | ^      ^     |
;;    v |  |  v |    v |      |     |
;;   HOLD  |  WAIT...... ------     |
;;     |   |  | (note records what  |
;;     v   v  v  it is waiting for) |
;;     CANX.... ---------------------
;;     (note records why it was cancelled)
(setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d!)")
                          (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANX(c@/!)")))

(add-hook 'org-mode-hook #'visual-line-mode)

;; Org capture templates
(setq org-capture-templates '(("t" "Todo" entry (file my-org-agenda-inbox)
                               "* TODO %i%?\n%U")
                              ("r" "Respond" entry (file my-org-agenda-inbox)
                               "* NEXT Respond to %i%?\n%U")
                              ("i" "Interrupt Task" entry (file my-org-agenda-inbox)
                               "* NEXT %i%?\n%U"
                               :jump-to-captured t :clock-in t :clock-resume t)
                              ("n" "Note" entry (file my-org-agenda-inbox)
                               "* %i%? :note:\n%U")
                              ("s" "Someday" entry (file my-org-agenda-inbox)
                               "* %i%? :someday:\n%U")
                              ("l" "Link" entry (file my-org-agenda-inbox)
                               "* %a%?\n%U")
                              ("y" "Paste" entry (file my-org-agenda-inbox)
                               "* %?\n%U\n%c")))

(with-eval-after-load 'org
  ;; maximize org-capture buffer
  (defun my-org-capture-setup (&rest args)
    "Save window configuration prior to `org-capture'."
    (set-frame-parameter
     nil
     'my-org-capture-prior-config
     (current-window-configuration)))
  (defun my-org-capture-teardown ()
    "Restore window configuration prior to `org-capture'."
    (let ((prior-window-configuration (frame-parameter
                                       nil
                                       'my-org-capture-prior-config)))
      (when prior-window-configuration
        (set-window-configuration prior-window-configuration))))
  (advice-add 'org-capture :before 'my-org-capture-setup)
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (add-hook 'org-capture-after-finalize-hook 'my-org-capture-teardown))

;; tags (note that tags within the same group are mutually exclusive)
(setq org-tag-alist '((:startgroup) ;; importance
                      ("important" . ?1)
                      ("unimportant" . ?2)
                      (:endgroup)
                      (:startgroup) ;; time-sensitivity
                      ("urgent" . ?3)
                      ("nonurgent" . ?4)
                      (:endgroup)
                      (:startgroup) ;; location
                      ("@home" . ?7)
                      ("@office" . ?8)
                      ("@travel" . ?9)
                      ("@errands" . ?0)
                      (:endgroup)
                      (:startgroup) ;; export
                      ("export" . ?e)
                      ("noexport" . ?E)
                      (:endgroup)
                      ;; ungrouped
                      ("meeting" . ?m)
                      ("note" . ?n)
                      ;; work-related relationship category
                      ("hiring" . ?H)
                      ("managing" . ?M)
                      ("vendor" . ?V)
                      ("partner" . ?P)
                      ("client" . ?C)
                      ;; work-related project category
                      ("internal" . ?\^n) ; C-n
                      ("healthcare" . ?\^h) ;; C-h
                      ("retail" . ?\^r))) ;; C-r

;; `org-export' macros
(with-eval-after-load 'ox
  ;; color macro, {{{color(colorname, text)}}} to use
  (push `("color"
          .
          ,(concat "@@latex:\\textcolor{$1}{$2}@@"
                   "@@html:<span style=\"color:$1\">$2</span>@@"))
        org-export-global-macros)
  ;; placeholder text, {{{loremipsum}}} to use
  (push `("loremipsum"
          .
          ,(mapconcat 'identity
                      '("Lorem ipsum dolor sit amet, consectetur"
                        "adipisicing elit, sed do eiusmod tempor"
                        "incididunt ut labore et dolore magna"
                        "aliqua. Ut enim ad minim veniam, quis"
                        "nostrud exercitation ullamco laboris nisi"
                        "ut aliquip ex ea commodo consequat. Duis"
                        "aute irure dolor in reprehenderit in"
                        "voluptate velit esse cillum dolore eu"
                        "fugiat nulla pariatur. Excepteur sint"
                        "occaecat cupidatat non proident, sunt in"
                        "culpa qui officia deserunt mollit anim id"
                        "est laborum."
                        "\n\n"
                        "Curabitur pretium tincidunt lacus. Nulla"
                        "gravida orci a odio. Nullam varius, turpis"
                        "et commodo pharetra, est eros bibendum elit,"
                        "nec luctus magna felis sollicitudin mauris."
                        "Integer in mauris eu nibh euismod gravida."
                        "Duis ac tellus et risus vulputate vehicula."
                        "Donec lobortis risus a elit. Etiam tempor."
                        "Ut ullamcorper, ligula eu tempor congue,"
                        "eros est euismod turpis, id tincidunt sapien"
                        "risus a quam. Maecenas fermentum consequat"
                        "mi. Donec fermentum. Pellentesque malesuada"
                        "nulla a mi. Duis sapien sem, aliquet nec,"
                        "commodo eget, consequat quis, neque. Aliquam"
                        "faucibus, elit ut dictum aliquet, felis nisl"
                        "adipiscing sapien, sed malesuada diam lacus"
                        "eget erat. Cras mollis scelerisque nunc."
                        "Nullam arcu. Aliquam consequat. Curabitur"
                        "augue lorem, dapibus quis, laoreet et,"
                        "pretium ac, nisi. Aenean magna nisl, mollis"
                        "quis, molestie eu, feugiat in, orci. In hac"
                        "habitasse platea dictumst.")
                      " "))
        org-export-global-macros)
  ;; flow control for latex-specific text and otherwise
  ;; {{{if-latex-else(latex text, other text)}}} to use
  (push '("if-latex-else"
          .
          "(eval (if (org-export-derived-backend-p
                     org-export-current-backend
                     'latex)
                    $1
                  $2))")
        org-export-global-macros))

;; hydra for org-mode
(defhydra my-hydra/org-mode (:color amaranth :columns 3)
  "
Org-mode (_q_: quit)"
  ("q" nil nil :exit t)
  ("M-s" org-narrow-to-subtree "narrow-subtree")
  ("M-b" org-narrow-to-block "narrow-block")
  ("M-w" widen "widen")
  ("M-l" org-toggle-link-display "toggle-link-disp")
  ("M-i" (lambda ()
           (interactive)
           (if org-image-actual-width
               (setq org-image-actual-width nil)
             (setq org-image-actual-width (list (/ (display-pixel-width) 3))))
           (org-redisplay-inline-images)) "toggle-img-width")
  ("i" org-toggle-inline-images "toggle-images")
  ("I" org-indent-mode "toggle-indent")
  ("P" org-toggle-pretty-entities "toggle-prettify")
  ("<tab>" org-cycle "cycle")
  ("<S-tab>" org-global-cycle "global-cycle")
  ("/" org-sparse-tree "sparse-tree")
  ("c" org-remove-occur-highlights "occur-clear")
  ("p"
   (lambda (n)
     (interactive "p")
     (if org-occur-highlights
         (previous-error n)
       (org-previous-visible-heading n)))
   "previous")
  ("n"
   (lambda (n)
     (interactive "p")
     (if org-occur-highlights
         (next-error n)
       (org-next-visible-heading n)))
   "next")
  ("g" org-goto "goto" :exit t)
  ("s" org-sort "sort" :exit t)
  ("o" org-occur "occur" :exit t)
  ("r" org-refile "refile" :exit t)
  ("t" org-todo "state" :exit t)
  (":" org-set-tags-command "tags" :exit t)
  ("," org-priority "priority" :exit t)
  ("D" org-insert-drawer "drawer" :exit t)
  ("P" org-set-property "property" :exit t)
  ("N" org-add-note "note" :exit t)
  ("F" org-footnote-action "footnote" :exit t)
  ("a" org-archive-subtree-default "archive" :exit t)
  ("<" org-insert-structure-template "structure" :exit t)
  ("'" org-edit-special "edit-special" :exit t)
  ("e" my-hydra/org-mode/emphasize/body "→ Emphasize" :exit t))

;; hydra for org-mode text formatting
(defhydra my-hydra/org-mode/emphasize (:color teal :columns 4)
  "
Org-mode → Emphasize (_q_: ←)"
  ("q" my-hydra/org-mode/body nil)
  ("b" (org-emphasize ?*) "bold")
  ("i" (org-emphasize ?/) "italic")
  ("u" (org-emphasize ?_) "underline")
  ("s" (org-emphasize ?+) "strike-through")
  ("c" (org-emphasize ?~) "code")
  ("v" (org-emphasize ?=) "verbatim"))

;; binding for org-mode hydra
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-M-m") #'my-hydra/org-mode/body))

;; org-agenda settings:
;; - narrow to subtree in org-agenda-follow-mode ("F" in agenda)
;; - full-frame Agenda view
;; - use ~/ORG-DIRECTORY/*.org files as Org agenda files
(setq org-agenda-follow-indirect t
      org-agenda-restore-windows-after-quit t
      org-agenda-start-on-weekday nil
      org-agenda-window-setup 'only-window
      org-agenda-files (file-expand-wildcards (concat org-directory "*.org")))

;; add separator between each day in agenda view
(setq org-agenda-format-date
      (lambda (date)
        (let* ((datestr (org-agenda-format-date-aligned date))
               (separator-width (- (window-width)
                                   (string-width datestr)
                                   1)))
          (concat "\n" datestr " " (make-string separator-width ?_)))))

(with-eval-after-load 'org-agenda
  ;; add custom agenda commands that only show undated tasks in list view
  (dolist (my-custom-cmd
           '(("N" "Three-day agenda and undated TODO entries"
              ((agenda "" ((org-agenda-span 3)))
               (alltodo "" ((org-agenda-todo-ignore-with-date t)
                            (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up category-keep alpha-up))))))
             ("u" "Undated TODO entries"
              (alltodo "" ((org-agenda-todo-ignore-with-date t)
                           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up category-keep alpha-up)))))))
    (add-to-list 'org-agenda-custom-commands my-custom-cmd)))

;; mode-specific hydra for org-agenda-mode
(defhydra my-hydra/org-agenda-mode (:color amaranth :hint nil)
  "
Org agenda (_q_: quit)
Headline    _ht_  : set status   _hk_  : kill         _hr_  : refile
            _hA_  : archive      _h:_  : set tags     _hp_  : set priority
Visit Entry _SPC_ : other window _TAB_ : & go to loc  _RET_ : & del other wins
            _o_   : link
Date        _ds_  : schedule     _dd_  : set deadline _dt_  : timestamp
View        _vd_  : day          _vw_  : week         _vm_  : month
            _vn_  : next span    _vp_  : prev span    _vr_  : reset
Filter      _ft_  : by tag       _fc_  : by category  _fh_  : by top headline
            _fx_  : by regex     _fd_  : reset
Clock       _ci_  : in           _co_  : out          _cq_  : cancel
            _cg_  : goto
Other       _gr_  : reload       _gd_  : go to date   _._   : go to today
            _sd_  : hide done
"
  ("q" nil nil :exit t)
  ("ht" org-agenda-todo)
  ("hk" org-agenda-kill)
  ("hr" org-agenda-refile)
  ("hA" org-agenda-archive-default)
  ("h:" org-agenda-set-tags)
  ("hp" org-agenda-priority)
  ("SPC" org-agenda-show-and-scroll-up)
  ("TAB" org-agenda-goto :exit t)
  ("RET" org-agenda-switch-to :exit t)
  ("o" link-hint-open-link :exit t)
  ("ds" org-agenda-schedule)
  ("dd" org-agenda-deadline)
  ("dt" org-agenda-date-prompt)
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vm" org-agenda-month-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ("ft" org-agenda-filter-by-tag)
  ("fc" org-agenda-filter-by-category)
  ("fh" org-agenda-filter-by-top-headline)
  ("fx" org-agenda-filter-by-regexp)
  ("fd" org-agenda-filter-remove-all)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ("cq" org-agenda-clock-cancel)
  ("cg" org-agenda-clock-goto :exit t)
  ("gr" org-agenda-redo)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("sd" (lambda () (interactive)
          (progn (setq org-agenda-skip-scheduled-if-done
                       (if org-agenda-skip-scheduled-if-done nil t))
                 (org-agenda-redo-all t)))))

;; bind org-agenda-mode hydra
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c C-M-m") #'my-hydra/org-agenda-mode/body))

;; allow refiling up to 9 levels deep in the current buffer
;; and 3 levels deep in Org agenda files
;; allow refiling to the top level
(setq org-refile-targets `((nil . (:maxlevel . 9)) ;; current buffer
                           ;; top-level of regular `org-agenda-files' files
                           (,(seq-filter
                              'file-regular-p
                              org-agenda-files) . (:level . 0)))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm)

;; hydra for Org entrypoints
(defhydra my-hydra/org-entrypoints (:color teal :columns 4)
  "
Org (_q_: quit)"
  ("q" nil nil)
  ("a" org-agenda "agenda")
  ("c" org-capture "capture")
  ("b" org-switchb "switch buffer")
  ("l" org-store-link "store link"))

;; bind Org entrypoints hydra
(global-set-key (kbd "C-c C-M-o") #'my-hydra/org-entrypoints/body)

;; compile Org documents to PDF with LuaTeX and Biber
(when (executable-find "lualatex")
  (with-eval-after-load 'org
    (setq org-latex-pdf-process
          '("lualatex -interaction nonstopmode -output-directory %o %f"
            "lualatex -interaction nonstopmode -output-directory %o %f"))
    (if (executable-find "biber")
        (push "biber %b" org-latex-pdf-process))
    (push "lualatex -interaction nonstopmode -output-directory %o %f"
          org-latex-pdf-process)))

;; use LuaTeX for previewing LaTeX math formula as images
(when (and (executable-find "lualatex")
           (executable-find "dvipng"))
  (with-eval-after-load 'org
    (add-to-list 'org-preview-latex-process-alist
                 '(dvipng :programs ("lualatex" "dvipng")
                          :description "dvi > png"
                          :message "you need to install lualatex and dvipng."
                          :image-input-type "dvi"
                          :image-output-type "png"
                          :image-size-adjust (1.0 . 1.0)
                          :latex-compiler
                          (concat "lualatex -output-format dvi"
                                  " -interaction nonstopmode"
                                  " -output-directory %o %f")
                          :image-converter ("dvipng -D %D -T tight -o %O %f")))))

;; preview LaTeX fragments scaled to font size, requires dvipng from TexLive
(when (and (display-graphic-p)
           (executable-find "dvipng"))
  (with-eval-after-load 'org
    (defvar my-org-latex-scale-base (plist-get org-format-latex-options :scale)
      "Base LaTeX fragment scale.")
    (defun my-org-display-latex-fragments ()
      "Previews LaTeX fragments in the buffer scaled to match font size."
      (interactive)
      (let* ((curr-text-scale (condition-case nil
                                  text-scale-mode-amount
                                (error 0)))
             (new-latex-scale (+ my-org-latex-scale-base curr-text-scale)))
        (when (eq major-mode 'org-mode)
          ;; modify LaTeX scale in a local copy of `org-format-latex-options'
          (if (not (assoc 'org-format-latex-options (buffer-local-variables)))
              (setq-local org-format-latex-options
                          (copy-tree org-format-latex-options)))
          (setq-local org-format-latex-options
                      (plist-put org-format-latex-options :scale new-latex-scale))
          ;; preview LaTeX fragments
          (org--latex-preview-region (point-min) (point-max)))))
    ;; preview LaTeX fragments when opening Org documents ...
    (add-hook 'org-mode-hook (lambda (&optional arg)
                               (my-org-display-latex-fragments)))
    ;; ... and regenerate after changing font size
    (advice-add 'text-scale-mode :after (lambda (&optional arg)
                                          (my-org-display-latex-fragments)))))

;; add mouse support and use variable pitch fonts in graphical Emacs org-mode
(when (display-graphic-p)
  (with-eval-after-load 'org
    (require 'org-mouse) ;; mouse support
    ;; use variable pitch fonts ...
    (add-hook 'org-mode-hook #'variable-pitch-mode)
    (add-hook 'org-mode-hook (lambda () (setq line-spacing 0.1)))
    ;; ... but keep some faces fixed-pitch
    (require 'org-indent) ;; ensure `org-indent' face is defined
    (let ((fixed-pitch-family (face-attribute 'fixed-pitch :family nil 'default)))
      (dolist (curr-face '(org-block
                           org-block-begin-line
                           org-block-end-line
                           org-code
                           org-date
                           org-document-info-keyword
                           org-done
                           org-indent ;; properly align indentation
                           org-latex-and-related
                           org-meta-line
                           org-property-value
                           org-special-keyword
                           org-table
                           org-todo
                           org-verbatim))
        (set-face-attribute curr-face nil :family fixed-pitch-family)))))

;; UTF-8 bullets in Org buffers
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))

;; insert urls from clipboard as links with title of page
(when (display-graphic-p)
  (use-package org-cliplink
    :after org
    :bind (:map org-mode-map
           ("C-c C-S-l" . org-cliplink))))

;; drag and drop images into Org buffers
(when (display-graphic-p)
  (use-package org-download
    :after org
    :config
    ;; Mac screenshot command
    (if (memq window-system '(mac ns))
        (setq org-download-screenshot-method "screencapture -i %s"))
    ;; adapted from https://coldnew.github.io/hexo-org-example/2018/05/22/use-org-download-to-drag-image-to-emacs/
    ;; save drag-and-drop images into folder of the same name as Org file
    ;; with filename prefixed by a timestamp of format `org-download-timestamp'
    ;; e.g. dragging test.png to abc.org saves it to abc/20180522183050-test.png
    (defun my-org-download-method (link)
      """Returns download save path for LINK, for use with `org-download'"""
      (let ((filename (format "%s%s"
                              (format-time-string org-download-timestamp)
                              (file-name-nondirectory
                                (car (url-path-and-query
                                       (url-generic-parse-url link))))))
            (dirname (file-name-sans-extension (buffer-name))))
        ;; create dir if it does not exist
        (unless (file-exists-p dirname)
          (make-directory dirname))
        ;; save path
        (expand-file-name filename dirname)))
    (setq org-download-method 'my-org-download-method
          org-download-timestamp "%Y%m%d%H%M%S-")))

(when (display-graphic-p)
  ;; hydra for org-download
  (defhydra my-hydra/org-mode/download (:color teal :columns 3)
    "
Org-mode → Download (_q_: ←)"
    ("q" my-hydra/org-mode/body nil)
    ("s" org-download-screenshot "screenshot")
    ("y" org-download-yank "yank"))

  ;; add entrypoint to download hydra to the org-mode hydra
  (defhydra+ my-hydra/org-mode nil
    ("d" my-hydra/org-mode/download/body "→ Download" :exit t)))

;; journaling using Org documents
(use-package org-journal
  :after org
  :init
  ;; org-capture helper function from https://github.com/bastibe/org-journal
  (defun my-org-journal-find-location ()
    "Find location of today's Org journal, for use with `org-capture'."
    ;; Open today's journal but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  ;; add org-capture-template for new journal entries
  (push '("j" "Journal" entry (function my-org-journal-find-location)
          "* %(format-time-string org-journal-time-format)%?\n%i")
        org-capture-templates)
  (setq org-journal-date-prefix "#+TITLE: Daily Journal "
        ;; separate journal files into folders by year
        org-journal-file-format "%Y/%Y%m%d.org"
        org-journal-file-type 'daily
        ;; don't carry over TODO items from a previous days
        org-journal-carryover-items nil
        ;; use ORG-DIRECTORY/journal/ as the default journal directory
        org-journal-dir (concat org-directory "journal/"))
  ;; add journal files to Org agenda
  ;; may cause the Org agenda parsing to slow down as the number as
  ;; the number of files grows, so make sure to prune or archive the
  ;; files elsewhere every so often if this is enabled.
  ;; (push org-journal-dir org-agenda-files)
  :config
  ;; workaround on `org-journal-is-journal' `string-match' error when
  ;; exporting to HTML due to `buffer-file-name' func returning nil
  (defun org-journal-is-journal--around-workaround (orig-fun &rest args)
    "Drop-in replacement advice function for `org-journal-is-journal'."
    (let ((buf-file-name (or (buffer-file-name) "")))
      (string-match (org-journal-dir-and-file-format->pattern)
                    buf-file-name)))
  (advice-add 'org-journal-is-journal :around
              #'org-journal-is-journal--around-workaround))

;; in-editor presentations using Org documents
(use-package org-present
  :after org
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-read-only)
                               (my-hide-header-and-mode-lines)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-read-write)
                                    (my-unhide-header-and-mode-lines))))
  :config
  ;; regenerate LaTeX fragment preview images on slide transition
  (when (and (display-graphic-p)
             (executable-find "dvipng"))
    (add-hook 'org-present-after-navigate-functions
              (lambda (&optional name header)
                (my-org-display-latex-fragments))))
  ;; functions for hiding header and mode lines when in a presentation
  (defvar-local my-orig-mode-line-format nil
    "Temporary variable to store original `mode-line-format'.")
  (defvar-local my-orig-header-line-format nil
    "Temporary variable to store original `header-line-format'.")
  (defun my-hide-header-and-mode-lines ()
    "Hide header and mode lines, and store originals in temporary variables."
    (interactive)
    (when mode-line-format
        (setq-local my-orig-mode-line-format mode-line-format)
        (setq-local mode-line-format nil))
    (when header-line-format
        (setq-local my-orig-header-line-format header-line-format)
        (setq-local header-line-format nil)))
  (defun my-unhide-header-and-mode-lines ()
    "Reset header and mode lines using originals in temporary variables."
    (interactive)
    (when (not mode-line-format)
      (setq-local mode-line-format my-orig-mode-line-format)
      (setq-local my-orig-mode-line-format nil))
    (when (not header-line-format)
      (setq-local header-line-format my-orig-header-line-format)
      (setq-local my-orig-header-line-format nil)))
  ;; easier nav keys for read-only presentations
  (define-minor-mode my-org-present-extra-mode
    "Overlay minor mode on top of org-present-mode with easier nav keys."
    :keymap (let ((map (make-sparse-keymap)))
              ;; <left>/<right> = previous/next slide
              (define-key map (kbd "<up>") 'scroll-down-line)
              (define-key map (kbd "<down>") 'scroll-up-line)
              (define-key map (kbd "s-<up>") 'beginning-of-buffer)
              (define-key map (kbd "s-<down>") 'end-of-buffer)
              (define-key map (kbd "s-<left>") 'org-present-beginning)
              (define-key map (kbd "s-<right>") 'org-present-end)
              (define-key map (kbd "f") 'toggle-frame-fullscreen)
              (define-key map (kbd "q") 'org-present-quit)
              (define-key map (kbd "-") 'text-scale-decrease)
              (define-key map (kbd "+") 'text-scale-increase)
              map))
  ;; toggle minor mode after the relevant org-present funcalls
  (advice-add 'org-present-read-only
              :after (lambda () (my-org-present-extra-mode 1)))
  (advice-add 'org-present-read-write
              :after (lambda () (my-org-present-extra-mode 0))))

;; add org-present present head to org-mode hydra
(defhydra+ my-hydra/org-mode nil
  ("C-p" (lambda ()
           (interactive)
           (let ((in-present-mode (condition-case nil
                                      org-present-mode
                                    (error nil))))
             (if in-present-mode (org-present-quit) (org-present))))
   "org-present" :exit t))

;; Org TODOs for projectile projects
;; use `org-capture' to capture and store TODOs for the current project
;; in `org-projectile-per-project-filepath' at the project's root directory
(use-package org-projectile
  :after (org projectile)
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")
  (push (org-projectile-project-todo-entry) org-capture-templates))

;; load Org backend for exporting to Markdown
(with-eval-after-load 'org
  (require 'ox-md))

;; create sychronized external notes in DocView and Nov.el
(use-package org-noter
  :bind ("C-c C-M-S-n" . org-noter)
  :init (setq org-noter-always-create-frame nil))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)
                                  (:name "Backlog"
                                   :scheduled past)
                                  (:name "Upcoming"
                                   :scheduled future)
                                  (:priority<= "B")
                                  (:name "Waiting"
                                   :todo "WAIT")
                                  (:name "On hold"
                                   :todo "HOLD")))
  (org-super-agenda-mode 1))

;; Outlines

(setq imenu-auto-rescan t)

;; menu list of major definitions across several buffers
(use-package imenu-anywhere
  :defer t
  :after imenu
  :bind ("C-c C-M-j" . imenu-anywhere))

;; show imenu as a list in a side buffer
(use-package imenu-list
  :defer t
  :after imenu
  :bind ("C-c C-M-'" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  ;; pulse target after selecting
  (add-hook 'imenu-list-after-jump-hook
            (lambda () (pulse-momentary-highlight-one-line (point))))
  ;; close imenu list after going to entry
  (advice-add 'imenu-list-goto-entry :after 'imenu-list-quit-window))

;; Programming / FlyCheck syntax checker

;; linting support, used in place of FlyMake
(use-package flycheck
  :init
  ;; customizations:
  ;; - remove newlines from events triggering linting checks
  ;; - don't mark error lines in fringe or margin
  (setq flycheck-check-syntax-automatically '(save
                                              idle-change
                                              mode-line)
        flycheck-indication-mode nil)
  :config
  ;; automatically adjust idle delay before automatically checking the
  ;; buffer depending on whether there are outstanding syntax errors;
  ;; check less frequently if there were no errors, and check more
  ;; frequently if there were errors; have this behavior be per-buffer
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (defun flycheck--adjust-flycheck-idle-change-delay ()
    "Adjust `flycheck-idle-change-delay' to check less frequently
when buffer is clean, and more frequently when it has errors."
    (setq flycheck-idle-change-delay (if flycheck-current-errors
                                         0.5
                                       3.0)))
  (add-hook 'flycheck-after-syntax-check-hook
            #'flycheck--adjust-flycheck-idle-change-delay)
  ;; default modes within which to use FlyCheck
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode))

;; flycheck hydra
(defhydra my-hydra/flycheck (:color amaranth :columns 3)
  "
FlyCheck [checker=%s(if flycheck-checker (symbol-name flycheck-checker) \"default\")] (_q_: quit)"
  ("q" nil nil :exit t)
  ("c" flycheck-buffer "run")
  ("C" flycheck-clear "clear")
  ("C-c" flycheck-compile "compile")
  ("n" flycheck-next-error "next")
  ("p" flycheck-previous-error "previous")
  ("l" (condition-case nil (quit-windows-on "*Flycheck errors*" t)
         (error (flycheck-list-errors))) "list")
  ("H" display-local-help "local-help")
  ("h" flycheck-display-error-at-point "display-at-pt")
  ("e" flycheck-explain-error-at-point "explain-at-pt")
  ("s" flycheck-select-checker "select-checker")
  ("?" flycheck-describe-checker "describe-checker" :exit t)
  ("v" flycheck-verify-setup "verify-setup" :exit t)
  ("C-w" flycheck-copy-errors-as-kill "copy-errors")
  ("i" flycheck-manual "web-manual" :exit t))

;; binding for flycheck hydra
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c C-M-e e") #'my-hydra/flycheck/body))

;; Programming / DevSkim and FlyCheck

(when (executable-find "devskim")
  (use-package flycheck-devskim
    :ensure nil ;; in site-lisp subfolder within user emacs directory
    :config
    (setq flycheck-devskim-executable "devskim")
    (with-eval-after-load 'lsp-mode
      (flycheck-add-next-checker 'lsp 'devskim))))

;; Programming / Conda package and environment manager

(when (executable-find "conda")
  (use-package conda
    :init (setq conda-anaconda-home (expand-file-name "~/miniconda3/"))
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
                 t)))

(defhydra my-hydra/conda (:color teal :columns 4)
  "
conda (_q_: quit)"
  ("q" nil nil)
  ("a" conda-env-activate "activate")
  ("d" conda-env-deactivate "deactivate")
  ("l" conda-env-list "list"))
(with-eval-after-load 'conda
  (global-set-key (kbd "C-c C-M-v e") 'my-hydra/conda/body))

;; Programming / lsp-mode Language Server Protocol client

;; lsp-mode Language Server Protocol client
;; auto-signature-help activation is not enabled by default, but to
;; show it activate it using `lsp-toggle-signature-auto-activate' or
;; C-S-SPC to peek, https://github.com/emacs-lsp/lsp-mode/issues/1223
(use-package lsp-mode
  :init (setq lsp-print-io nil ;; disable logging packets between Emacs and LS
              lsp-print-performance nil ;; disable performance logging
              lsp-eldoc-enable-hover nil ;; don't have eldoc display hover info
              lsp-eldoc-render-all nil ;; don't show all returned from document/onHover, only symbol info
              lsp-enable-on-type-formatting nil ;; don't have the LS automatically format the document when typing
              lsp-diagnostic-package :flycheck ;; use FlyCheck for syntax checking
              lsp-signature-auto-activate nil)) ;; don't automatically show signature

;; company backend for LSP-driven completion
(use-package company-lsp
  :after lsp-mode
  :init (setq company-lsp-cache-candidates t))

;; hydras adapted from lsp-mode's default command map, see lsp-command-map in
;; https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-mode.el
(defhydra my-hydra/lsp (:color teal :columns 3)
  "
Language Server (_q_: quit)"
  ("q" nil nil)
  ("s" my-hydra/lsp-session/body "→ Session")
  ("=" my-hydra/lsp-format/body "→ Format")
  ("F" my-hydra/lsp-folder/body "→ Folder")
  ("T" my-hydra/lsp-toggle/body "→ Toggle")
  ("g" my-hydra/lsp-goto/body "→ Goto")
  ("h" my-hydra/lsp-help/body "→ Help")
  ("r" my-hydra/lsp-refactor/body "→ Refactor")
  ("a" my-hydra/lsp-actions/body "→ Actions"))
(defhydra my-hydra/lsp-session (:color teal :columns 3)
  "
Language Server → Session (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("r" (condition-case nil (lsp-restart-workspace) (error (lsp))) "(re-)start")
  ("s" lsp-workspace-shutdown "shutdown")
  ("d" lsp-describe-session "describe")
  ("D" lsp-disconnect "disconnect"))
(defhydra my-hydra/lsp-format (:color teal :columns 3)
  "
Language Server → Format (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("=" lsp-format-buffer "buffer")
  ("r" lsp-format-region "range"))
(defhydra my-hydra/lsp-folder (:color teal :columns 3)
  "
Language Server → Folder (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("a" lsp-workspace-folders-add "add")
  ("r" lsp-workspace-folders-remove "remove")
  ("b" lsp-workspace-blacklist-remove "un-blacklist"))
(defhydra my-hydra/lsp-toggle (:color amaranth :columns 3)
  "
Language Server → Toggle (_q_: ←)"
  ("q" my-hydra/lsp/body nil :exit t)
  ("l" lsp-lens-mode "lens-mode")
  ("L" lsp-toggle-trace-io "trace-io")
  ("h" lsp-toggle-symbol-highlight "symbol-highlight")
  ("b" lsp-headerline-breadcrumb-mode "headerline-breadcrumb")
  ("s" lsp-toggle-signature-auto-activate "signature-help")
  ("f" lsp-toggle-on-type-formatting "on-type-formatting"))
(defhydra my-hydra/lsp-goto (:color teal :columns 3)
  "
Language Server → Goto (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("j" (lambda ()
         (interactive)
         (if (fboundp 'imenu-list-smart-toggle)
             (imenu-list-smart-toggle)
           (message "Requires imenu-list"))) "imenu")
  ("g" lsp-find-definition "definition")
  ("r" lsp-find-references "references")
  ("i" lsp-find-implementation "implementation")
  ("t" lsp-find-type-definition "type-implementation")
  ("d" lsp-find-declaration "declaration")
  ("a" xref-find-apropos "workspace-symbol"))
(defhydra my-hydra/lsp-help (:color teal :columns 3)
  "
Language Server → Help (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("h" lsp-describe-thing-at-point "describe")
  ("s" lsp-signature-activate "signature-activate"))
(defhydra my-hydra/lsp-refactor (:color teal :columns 3)
  "
Language Server → Refactor (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("r" lsp-rename "rename")
  ("o" lsp-organize-imports "organize-imports"))
(defhydra my-hydra/lsp-actions (:color teal :columns 3)
  "
Language Server → Actions (_q_: ←)"
  ("q" my-hydra/lsp/body nil)
  ("a" lsp-execute-code-action "execute-code-action")
  ("l" lsp-avy-lens "avy-lens")
  ("h" lsp-document-highlight "document-highlight"))

;; bind lsp-mode hydra
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-M-l") #'my-hydra/lsp/body))

;; Programming / dap-mode Debug Adaptor Protocol client

;; client for Debug Adaptor Protocol servers
(use-package dap-mode
  :after lsp-mode
  :bind (:map lsp-mode-map
         ("C-c C-M-d d" . dap-debug)
         ("C-c C-M-d D" . dap-debug-edit-template))
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; Programming / Emacs Lisp

;; hydra for built-in Emacs Lisp debugger
(defhydra my-hydra/debugger (:color teal :hint nil
                             :pre (require 'debug))
  "
Emacs debugger settings (_q_: quit)
Toggle    _1_ : debug-on-error (currently: %`debug-on-error)
          _2_ : debug-on-quit  (currently: %`debug-on-quit)
Functions _fl_ : list functions to invoke debugger on entry
          _fa_ : add debugger invocation to function
          _fc_ : cancel debugger invocation from function
Variables _vl_ : list variables to invoke debugger on change
          _va_ : add debugger invocation to variable on change
          _vc_ : cancel debugger invocation from variable on change
"
  ("q" nil nil)
  ("1" toggle-debug-on-error :exit nil)
  ("2" toggle-debug-on-quit :exit nil)
  ("fl" debugger-list-functions)
  ("fa" debug-on-entry)
  ("fc" cancel-debug-on-entry)
  ("vl" (lambda () (interactive) (prin1 (debug--variable-list))))
  ("va" debug-on-variable-change)
  ("vc" cancel-debug-on-variable-change))

;; binding for debugger-settings hydra
(global-set-key (kbd "C-c C-M-d e") 'my-hydra/debugger/body)

;; mode-specific hydra for debugger
(defhydra my-hydra/debugger-mode (:color teal :columns 4)
    "
Emacs debugger (_q_: quit)"
    ("q" nil nil)
    ("c" debugger-continue "continue")
    ("d" debugger-step-through "step")
    ("b" debugger-frame "frame")
    ("u" debugger-frame-clear "no-frame")
    ("j" debugger-jump "jump")
    ("e" debugger-eval-expression "eval-expr")
    ("R" debugger-record-expression "record-expr")
    ("Q" top-level "quit-to-top")
    ("r" debugger-return-value "return-val")
    ("l" debugger-list-functions "list-funs")
    ("v" debugger-toggle-locals "list-vars")
    ("h" describe-mode "help"))

;; binding for debugger hydra
(with-eval-after-load 'debug
 (define-key debugger-mode-map (kbd "C-c C-M-m") #'my-hydra/debugger-mode/body))

;; hydra for built-in Emacs Lisp profiler
(defhydra my-hydra/profiler (:color teal :columns 3
                             :pre (require 'profiler))
  "
Emacs profiler [CPU=%(profiler-running-p) MEM=%(profiler-memory-running-p)] (_q_: quit)"
  ("q" nil nil)
  ("s" profiler-start "start/reset" :exit nil)
  ("p" profiler-report "report")
  ("e" profiler-stop "stop" :exit nil))

;; binding for profiler hydra
(global-set-key (kbd "C-c C-M-e p") 'my-hydra/profiler/body)

(use-package el-patch
  :demand t)

;; Modifies lisp indentation to handle property lists used as
;; data structures
;; --- DEFAULT BEHAVIOR
;;  `(:token ,token
;;           :token-quality ,quality)
;; ---
;; --- DESIRED BEHAVIOR
;;  `(:token ,token
;;    :token-quality ,quality)
;; ---
;; Copied from https://emacs.stackexchange.com/questions/10230/
(with-eval-after-load 'lisp-mode
  (el-patch-defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (el-patch-let (($cond (and (elt state 2)
                               (el-patch-wrap 1 1
                                 (or (not (looking-at "\\sw\\|\\s_"))
                                     (looking-at ":")))))
                   ($then (progn
                            (if (not (> (save-excursion (forward-line 1) (point))
                                        calculate-lisp-indent-last-sexp))
                                (progn (goto-char calculate-lisp-indent-last-sexp)
                                       (beginning-of-line)
                                       (parse-partial-sexp (point)
                                                           calculate-lisp-indent-last-sexp 0 t)))
                            ;; Indent under the list or under the first sexp on the same
                            ;; line as calculate-lisp-indent-last-sexp.  Note that first
                            ;; thing on that line has to be complete sexp since we are
                            ;; inside the innermost containing sexp.
                            (backward-prefix-chars)
                            (current-column)))
                   ($else (let ((function (buffer-substring (point)
                                                            (progn (forward-sexp 1) (point))))
                                method)
                            (setq method (or (function-get (intern-soft function)
                                                           'lisp-indent-function)
                                             (get (intern-soft function) 'lisp-indent-hook)))
                            (cond ((or (eq method 'defun)
                                       (and (null method)
                                            (> (length function) 3)
                                            (string-match "\\`def" function)))
                                   (lisp-indent-defform state indent-point))
                                  ((integerp method)
                                   (lisp-indent-specform method state
                                                         indent-point normal-indent))
                                  (method
                                   (funcall method indent-point state))))))
      (let ((normal-indent (current-column))
            (el-patch-add
              (orig-point (point))))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (el-patch-swap
          (if $cond
              ;; car of form doesn't seem to be a symbol
              $then
            $else)
          (cond
           ;; car of form doesn't seem to be a symbol, or is a keyword
           ($cond $then)
           ((and (save-excursion
                   (goto-char indent-point)
                   (skip-syntax-forward " ")
                   (not (looking-at ":")))
                 (save-excursion
                   (goto-char orig-point)
                   (looking-at ":")))
            (save-excursion
              (goto-char (+ 2 (elt state 1)))
              (current-column)))
           (t $else)))))))

;; Programming / Clojure

;; basic support
(use-package clojure-mode
  :defer t
  :hook ((clojure-mode . paredit-mode)
         (clojure-mode . subword-mode)))

;; Clojure IDE
(use-package cider
  :after clojure-mode
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . paredit-mode))
  :config (setq nrepl-log-messages t))

;; hydras, adapted from https://github.com/clojure-emacs/cider-hydra
(defhydra my-hydra/cider (:color teal :columns 3)
  "
CIDER (_q_: quit)"
  ("q" nil nil)
  ;; start a REPL and connect to it
  ("j" cider-jack-in-clj "jack-in-clj")
  ("s" cider-jack-in-cljs "jack-in-cljs")
  ("b" cider-jack-in-clj&cljs "jack-in-clj&cljs")
  ;; sub-hydras
  ("d" my-hydra/cider-doc/body "→ Documentation")
  ("e" my-hydra/cider-eval/body "→ Evaluation")
  ("T" my-hydra/cider-test/body "→ Test")
  ("D" my-hydra/cider-debug/body "→ Debug")
  ("r" my-hydra/cider-repl/body "→ REPL"))
(defhydra my-hydra/cider-doc (:color teal :columns 4)
  "
CIDER → Documentation (_q_: ←)"
  ("q" my-hydra/cider/body nil)
  ;; CiderDoc
  ("d" cider-doc "cider-docs")
  ;; ClojureDocs
  ("r" cider-clojuredocs "clojure-docs")
  ("h" cider-clojuredocs-web "clojure-docs-web")
  ;; JavaDoc
  ("j" cider-javadoc "java-docs-web")
  ;; apropos
  ("a" cider-apropos "search-symbols")
  ("s" cider-apropos-select "select-symbols")
  ("A" cider-apropos-documentation "search-docs")
  ("e" cider-apropos-documentation-select "select-docs"))
(defhydra my-hydra/cider-eval (:color teal :columns 3)
  "
CIDER → Eval (_q_: ←)"
  ("q" my-hydra/cider/body nil)
  ;; load
  ("k" cider-load-buffer "load-buffer")
  ("l" cider-load-file "load-file")
  ("p" cider-load-all-project-ns "load-all-proj-ns")
  ;; eval
  ("r" cider-eval-region "eval-region")
  ("n" cider-eval-ns-form "eval-ns-form")
  ("e" cider-eval-last-sexp "eval-last-sexp")
  ("P" cider-pprint-eval-last-sexp "eval-last-sexp-pp")
  ("w" cider-eval-last-sexp-and-replace "eval-last-sexp-replace")
  ("E" cider-eval-last-sexp-to-repl "eval-last-sexp-to-repl")
  ("d" cider-eval-defun-at-point "eval-defun-at-point")
  ("f" cider-pprint-eval-defun-at-point "eval-defun-at-point-pp")
  (":" cider-read-and-eval "read-and-eval")
  ;; inspect
  ("i" cider-inspect "inspect")
  ;; macro expansion
  ("m" cider-macroexpand-1 "macroexpand-1")
  ("M" cider-macroexpand-all "macroexpand-all"))
(defhydra my-hydra/cider-test (:color teal :columns 4)
  "
CIDER → Test (_q_: ←)"
  ("q" my-hydra/cider/body nil)
  ("t" cider-test-run-test "run")
  ("l" cider-test-run-loaded-tests "run-loaded")
  ("p" cider-test-run-project-tests "run-project")
  ("n" cider-test-run-ns-tests "run-ns")
  ("r" cider-test-rerun-failed-tests "rerun-failed")
  ("s" cider-test-show-report "show-report"))
(defhydra my-hydra/cider-debug (:color teal :columns 3)
  "
CIDER → Debug (_q_: ←)"
  ("q" my-hydra/cider/body nil)
  ("x" (lambda () (interactive) (cider-eval-defun-at-point t)) "eval-defun-at-pt")
  ("v" cider-toggle-trace-var "toggle-var-trace")
  ("n" cider-toggle-trace-ns "toggle-ns-trace"))
(defhydra my-hydra/cider-repl (:color teal :columns 3)
  "
CIDER → REPL (_q_: ←)"
  ("q" my-hydra/cider/body nil)
  ;; connection
  ("d" cider-display-connection-info "disp-conn-info")
  ("r" cider-rotate-default-connection "rot-default-conn")
  ;; input
  ("z" cider-switch-to-repl-buffer "switch-to-repl")
  ("n" cider-repl-set-ns "set-repl-ns")
  ("p" cider-insert-last-sexp-in-repl "ins-last-sexp-in-repl")
  ("x" cider-refresh "refresh")
  ;; output
  ("o" cider-find-and-clear-repl-output "clear-repl-output")
  ("O" (lambda () (interactive) (cider-find-and-clear-repl-output t)) "clear-repl-all")
  ;; interrupt or quit connected REPL
  ("b" cider-interrupt "interrupt")
  ("Q" cider-quit "quit-cider"))

;; binding for main CIDER hydra
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c C-M-m") #'my-hydra/cider/body))

;; clojure linting, requires clj-kondo be installed on the system
(when (executable-find "clj-kondo")
  (use-package flycheck-clj-kondo
    :after (flycheck clojure-mode)
    :config
    (require 'flycheck-clj-kondo)
    ;; start flycheck-mode
    (add-hook 'clojure-mode-hook (lambda () (flycheck-mode 1)) t)))

;; Programming / fish shell scripts

(use-package fish-mode
  :init (setq fish-enable-auto-indent t
              fish-indent-offset 4))

;; Programming / Python

;; mode-specific hydra for Python mode
(defhydra my-hydra/python-mode (:color teal :columns 4)
  "
Python (_q_: quit)"
  ("q" nil nil)
  ;; python repl
  ("p" run-python "run-python")
  ("s" python-shell-send-string "send-str")
  ("e" python-shell-send-statement "send-stmt")
  ("r" python-shell-send-region "send-rgn")
  ("x" python-shell-send-defun "send-def")
  ("c" python-shell-send-buffer "send-buf")
  ("l" python-shell-send-file "send-file")
  ("z" python-shell-switch-to-shell "switch-to-sh")
  ;; indentation
  ("<" python-indent-shift-left "indent-l")
  (">" python-indent-shift-right "indent-r")
  ;; utilities
  ("v" python-check "check-err")
  ("f" python-eldoc-at-point "eldoc-at-pt")
  ("d" python-describe-at-point "descr-at-pt")
  ;; other
  ("j" imenu "imenu")
  ("D" pdb "pdb"))

;; binding for Python hydra
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-M-m") #'my-hydra/python-mode/body))

;; (add-hook 'python-mode-hook #'flycheck-mode t)

;; lsp-mode client for MS Python LS, https://github.com/emacs-lsp/lsp-python-ms
(use-package lsp-python-ms
  ;; append lsp-mode hooks
  :config (add-hook 'python-mode-hook
                    (lambda ()
                      ;; load packages if deferred
                      (require 'lsp-mode)
                      (require 'lsp-python-ms)
                      ;; start LSP client
                      (lsp-mode))
                    t))

(add-hook 'python-mode-hook
          (lambda ()
            (require 'dap-python))
          t)

;; Programming / R

;; support for R language using Emacs Speaks Statistics
;; linting is configured here to use FlyCheck
(use-package ess
  :mode ("\\.R$" . R-mode)
  :commands (R-mode ess-switch-to-ESS)
  :init (setq ess-eval-visibly 'nowait
              ess-default-style 'RStudio
              ;; disable ESS auto-use of Flymake since using FlyCheck
              ess-use-flymake nil)
  :config (add-hook 'ess-r-mode-hook (lambda () (flycheck-mode 1)) t))

;; forward pipe and assignment R operator shortcuts, adapted from
;; https://emacs.stackexchange.com/questions/8041/how-to-implement-the-piping-operator-in-ess-mode
(defun my-insert-R-forward-pipe-operator ()
  "Insert R magrittr forward pipe operator '%>%'."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
(defun my-insert-R-assignment-operator ()
  "Insert R assigment operator '<-'."
  (interactive)
  (just-one-space 1)
  (insert "<- "))

;; bindings for the above R operator shortcuts
(with-eval-after-load 'ess-mode
  (define-key ess-mode-map (kbd "M--") #'my-insert-R-assignment-operator)
  (define-key ess-mode-map (kbd "C-S-m") #'my-insert-R-forward-pipe-operator))
(with-eval-after-load 'ess-inf
  (define-key inferior-ess-mode-map (kbd "M--") #'my-insert-R-assignment-operator)
  (define-key inferior-ess-mode-map (kbd "C-S-m") #'my-insert-R-forward-pipe-operator))

;; major mode-specific hydra for ess-mode
(defhydra my-hydra/ess-mode (:color teal :hint nil)
  "
Emacs Speaks Statistics (_q_: quit)
Session       _N_ : new       _R_ : request   _s_ : switch    _C-q_ : quit
Eval          _l_ : line      _f_ : func      _r_ : region    _b_   : buffer
Workspace     _D_ : chdir     _d_ : R dired
Help          _h_ : object    _H_ : browser   _A_ : apropos
"
  ("q" nil nil)
  ;; session
  ("N" (lambda () (interactive)
         (cond ((string= ess-dialect "R") (R))
               ((string= ess-dialect "julia") (julia))
               (t (message "Unsupported dialect")))))
  ("R" ess-request-a-process)
  ("s" ess-switch-to-ESS)
  ("C-q" ess-quit)
  ;; eval
  ("l" ess-eval-line)
  ("f" ess-eval-function)
  ("r" ess-eval-region)
  ("b" ess-eval-buffer)
  ;; workspace
  ("D" ess-change-directory)
  ("d" ess-rdired)
  ;; help
  ("h" ess-display-help-on-object)
  ("H" ess-display-help-in-browser)
  ("A" ess-display-help-apropos))

;; binding for ess-mode hydra
(with-eval-after-load 'ess-mode
  (define-key ess-mode-map (kbd "C-c C-M-m") #'my-hydra/ess-mode/body))

(use-package poly-R)

;; Programming / Racket

(use-package racket-mode
  :defer t)

;; major mode-specific hydra for racket-mode
(defhydra my-hydra/racket-mode (:color teal :columns 4)
  "
Racket Mode (_q_: quit)"
  ("q" nil nil)
  ;; refactoring requires
  ("Rt" racket-tidy-requires "tidy-req")
  ("RT" racket-trim-requires "trim-req")
  ("Rb" racket-base-requires "base-req")
  ;; compile Racket Mode's .rkt files for faster startup
  ("S" racket-mode-start-faster "mode-compile")
  ;; racket modes
  ("x" racket-xp-mode "xp-mode" :exit nil)
  ;; repl
  ("rr" racket-run "run")
  ("rm" racket-run-module-at-point "run-module")
  ("rR" racket-racket "racket")
  ;; profiling and logging
  ("rp" racket-profile "profile")
  ("rl" racket-logger "logger")
  ;; testing
  ("t" racket-test "test")
  ("T" racket-raco-test "raco-test")
  ;; misc
  ("f" racket-find-collection "find-coll")
  ;; help
  ("." racket-xp-visit-definition "visit-defn")
  ("C-." racket-visit-module "visit-modl")
  ("," racket-unvisit "unvisit")
  ("h" racket-xp-describe "desc")
  ("H" racket-xp-documentation "docs")
  ;; editing
  ("a" racket-align "align")
  ("A" racket-unalign "unalign"))

;; bindings for racket-mode hydra
(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "C-c C-M-m") #'my-hydra/racket-mode/body))

;; Project interaction

;; project interaction library
(use-package projectile
  :demand t
  :config
  (setq projectile-completion-system (if (featurep 'helm) 'helm 'default)
        projectile-create-missing-test-files t ;; create test file if none is found when toggling
        projectile-switch-project-action 'projectile-commander
        projectile-use-git-grep t) ;; use git grep to skip backup, object, and untracked files when in a Git project
  (projectile-mode)) ;; enable mode globally

;; hydra for Projectile
(defhydra my-hydra/projectile-mode (:color teal :hint nil)
  "
Projectile: %s(projectile-project-name) (_q_: quit)
Buffer _←_ : previous proj buf  _→_ : next proj buf      _b_ : switch
       _I_ : ibuffer            _S_ : save proj bufs     _k_ : kill proj bufs
File   _f_ : find (curr proj)   _F_ : find (known projs) _g_ : find (context)
       _t_ : goto impl/test     _e_ : recent             _E_ : dir-locals-file
Dir    _d_ : find dir           _D_ : dired
Search _o_ : multi-occur        _s_ : grep               _r_ : replace string
Tags   _j_ : find tag           _R_ : regenerate tags
Shell  _x_ : eshell             _!_ : run command        _&_ : run command async
Other  _C_ : configure proj     _c_ : compile proj       _u_ : run proj
       _P_ : test proj          _z_ : cache curr file    _i_ : clear cache
"
  ("q" nil nil)
  ;; buffer
  ("b" projectile-switch-to-buffer)
  ("<left>" projectile-previous-project-buffer :exit nil)
  ("<right>" projectile-next-project-buffer :exit nil)
  ("I" projectile-ibuffer)
  ("S" projectile-save-project-buffers)
  ("k" projectile-kill-buffers)
  ;; file
  ("f" projectile-find-file)
  ("F" projectile-find-file-in-known-projects)
  ("g" projectile-find-file-dwim)
  ("t" projectile-toggle-between-implementation-and-test)
  ("e" projectile-recentf)
  ("E" projectile-edit-dir-locals)
  ;; dir
  ("d" projectile-find-dir)
  ("D" projectile-dired)
  ;; search
  ("o" projectile-multi-occur)
  ("s" projectile-grep)
  ("r" projectile-replace)
  ;; tags
  ("j" projectile-find-tag)
  ("R" projectile-regenerate-tags)
  ;; other
  ("C" projectile-configure-project)
  ("c" projectile-compile-project)
  ("u" projectile-run-project)
  ("P" projectile-test-project)
  ("z" projectile-cache-current-file)
  ("i" projectile-invalidate-cache)
  ("x" projectile-run-eshell)
  ("!" projectile-run-shell-command-in-root)
  ("&" projectile-run-async-shell-command-in-root)
  ;; misc
  ("m" projectile-commander "commander")
  ("p" projectile-switch-project "switch project"))

;; binding for projectile hydra
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c C-M-p") #'my-hydra/projectile-mode/body))

;; binding for calling Magit
(use-package magit
    :commands magit-status
    :bind ("C-c C-M-g s" . magit-status))

;; Uncomment to check VC info on file auto-revert (increases I/O load)
;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
;; (setq auto-revert-check-vc-info t)

;; call `magit-status' for file at point in Ibuffer, uses `ibuffer-vc'
;; adapted from https://www.manueluberti.eu/emacs/2019/08/06/ibuffer-magit/
(defun my-ibuffer-magit-status-at-pt ()
  "Call `magit-status' for the buffer at point while in Ibuffer."
  (interactive)
  (condition-case nil
      (progn
        (require 'ibuffer-vc)
        (let ((buf (ibuffer-current-buffer t)))
          (magit-status (cdr (ibuffer-vc-root buf)))))
    (message "requires `ibuffer-vc' package be installed.")))

;; bind the above function to the "G" key in Ibuffer
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "G") #'my-ibuffer-magit-status-at-pt))

;; the "I" key in Magit opens a Git identity management interface
(use-package git-identity
  :after magit
  :bind (:map magit-status-mode-map
              ("I" . git-identity-info))
  :config
  (require 'git-identity-magit)
  (git-identity-magit-mode 1))

;; Browse older versions of Git-controlled files
(use-package git-timemachine
  :commands git-timemachine
  :bind ("C-c C-M-g t" . git-timemachine))

;; per-project file trees
(use-package treemacs
  :bind ("C-c C-M-t" . treemacs)
  :init
  ;; resize treemacs icon sizes to 75% of line-height
  (add-hook 'after-init-hook
            (lambda ()
              (when (and (display-graphic-p)
                         (eq system-type 'darwin))
                (treemacs-resize-icons
                 (truncate (* (line-pixel-height) 0.75)))))))

;; treemacs projectile integration
(use-package treemacs-projectile
  :after (treemacs projectile))

;; treemacs magit integration
(use-package treemacs-magit
  :after (treemacs magit))

;; Reference management

;; manager for BibTeX bibliographic databases
(use-package ebib
  :bind (("C-c C-M-b e" . ebib)))

;; configure Ebib Org-mode support
;;
;; this setup supports exporting Org to PDF with BibTeX bibliographies via
;; lualatex and biber, so they will need to be installed on the system
;;
;; org-mode documents should include the LaTeX headers for
;; bibliographies via "#+LATEX_HEADER:" structural markup elements,
;; and "\printbibliography" should be added at the desired location
;; for the bibliography (usually at the end of an article or book
;; chapter or before the index)
;;
;; references to bibliography entries in org-mode can be inserted by
;; pressing `i' when on an entry in ebib or by calling
;; `ebib-insert-citation'
;;
;; to export references from Org to LaTeX, ebib needs to be opened with the
;; bibliographies for the references that appear in the document
;;
;; use "::" in the Org link description to separate the preamble text,
;; pre-note, and post-note elements (all optional) for export to LaTeX,
;; i.e. "[[ebib:key][Preamble text::Pre-note::Post-note]]"
;; will export to "Preamble text\cite[Pre-note][Post-note]{key}"
;;
;; example:
;; ---
;; ...
;; #+LATEX_HEADER: \usepackage[backend=biber]{biblatex}
;; #+LATEX_HEADER: \addbibresource{path/to/bibtex_file.bib}
;; ...
;; [[ebib:some_ebib_entry_key]]
;; [[ebib:some_ebib_entry_key][Preamble]
;; [[ebib:some_ebib_entry_key][Preamble::::Post-note]
;; [[ebib:some_ebib_entry_key][Preamble::Pre-note::Post-note]]
;; [[ebib:incognito_1970][::see::pg. 99]]
;; ...
;; \printbibliography
;; ...
;; ---
;;
(with-eval-after-load 'org
  ;; ebib configuration for org-mode
  (with-eval-after-load 'ebib
    (require 'org-ebib)
    (defun my-org-ebib-export (path desc format)
      "Export an ebib link. See `org-link-parameters' for details about PATH, DESC and FORMAT."
      (let* ((my-desc (or desc ""))
             (desc-parts (split-string my-desc "::"))
             (desc-name (car desc-parts))
             (desc-pre-note (or (nth 1 desc-parts) ""))
             (desc-post-note (mapconcat 'identity (nthcdr 2 desc-parts) "::")))
        (cond
         ((eq format 'html)
          (if desc
              (format "(%s<cite>%s</cite>%s)"
                      (if (string= "" desc-pre-note) "" (concat desc-pre-note " "))
                      (if (string= "" desc-name) path desc-name)
                      (if (string= "" desc-post-note) "" (concat ", " desc-post-note)))
            (format "(<cite>%s</cite>)" path)))
         ((eq format 'latex)
          (if desc
              (format "%s\\cite%s%s{%s}"
                      (concat desc-name " ")
                      (if (string= "" desc-pre-note) "" (format "[%s]" desc-pre-note))
                      (if (string= "" desc-post-note) "" (format "[%s]" desc-post-note))
                      path)
            (format "\\cite{%s}" path))))))
    (org-link-set-parameters "ebib" :export 'my-org-ebib-export))
  ;; binding for `ebib-insert-citation'
  (define-key org-mode-map (kbd "C-c C-M-b i") #'ebib-insert-citation))

;; Search and navigation

(defhydra my-hydra/search (:color teal :columns 3)
  "
Search (_q_: quit)"
  ("q" nil nil)
  ("gg" grep "grep")
  ("gr" rgrep "rgrep")
  ("gl" lgrep "lgrep")
  ("gf" grep-find "grep-find")
  ("gz" rzgrep "rzgrep")
  ("oo" occur "occur")
  ("om" multi-occur "multi-occur")
  ("ob" multi-occur-in-matching-buffers "multi-occur-match-buf")
  ("oO" org-occur "org-occur")
  ("rs" query-replace "replace string")
  ("rr" query-replace-regexp "replace regexp")
  ("kg" kill-grep "kill-grep"))
(global-set-key (kbd "C-c C-M-/") 'my-hydra/search/body)

;; support for ripgrep if installed on the system
(when (executable-find "rg")
  (use-package deadgrep
    :defer t
    :bind ("<f5>" . deadgrep))
  (defhydra+ my-hydra/search nil
    ("gR" deadgrep "ripgrep" :exit t)))

;; show current and total search matches, and preview query replace results
(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :init (global-anzu-mode))

;; jump to definition using ag or rg and applying heuristics
(use-package dumb-jump
  :config (setq dumb-jump-aggressive nil
                dumb-jump-default-project "./"
                dumb-jump-selector 'completing-read))

;; hydra for dumb-jump
;; adapted from https://github.com/jacktasia/dumb-jump/blob/master/README.md
(defhydra my-hydra/search/dumb-jump (:color teal :columns 3
                                     :pre (require 'dumb-jump))
  "
Dumb Jump [mode-enabled=% 3`dumb-jump-mode] (_q_: ←)"
  ("q" my-hydra/search/body nil)
  ("j" dumb-jump-go "go")
  ("o" dumb-jump-go-other-window "go-other")
  ("e" dumb-jump-go-prefer-external "go-ext")
  ("x" dumb-jump-go-prefer-external-other-window "go-ext-other")
  ("i" dumb-jump-go-prompt "prompt")
  ("l" dumb-jump-quick-look "peek")
  ("b" dumb-jump-back "back")
  ("m" dumb-jump-mode "toggle-mode"))

;; add entrypoint for dumb-jump hydra in my-hydra/search
(defhydra+ my-hydra/search nil
  ("j" my-hydra/search/dumb-jump/body "dumb-jump"))

;; jump to visible text using char-based decision tree
(use-package avy
  :config
  ;; bind over `goto-line' since it can be invoked by entering numbers
  ;; for `avy-goto-line' input instead characters in the decision tree
  (global-set-key [remap goto-line] #'avy-goto-line))

;; display, select and jump to links in various major modes
(use-package ace-link
  :config
  ;; bind "o" to calling ace-link in compilation-mode, Custom-mode,
  ;; eww-mode, help-mode, Info-mode and woman-mode
  (ace-link-setup-default)
  ;; bind "M-o" to jump to link in Org mode
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "M-o") #'ace-link-org))
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "M-o") #'ace-link-org-agenda)))

;; load notdeft, make sure this comes after org-directory is set
(require 'notdeft-autoloads)
(setq notdeft-directories `(,(concat org-directory "journal/")
                            ,(concat org-directory "scratchpad/"))
      notdeft-extension "org"
      notdeft-secondary-extensions '("md" "txt")
      notdeft-directory (concat org-directory "scratchpad/")
      notdeft-xapian-program (concat (file-name-directory
                                      (file-truename
                                       (locate-library "notdeft")))
                                     "xapian/notdeft-xapian"))

;; binding to access Notdeft
(global-set-key (kbd "C-c C-M-s") #'notdeft)

;; load and bind the Notdeft mode-specific hydra
(autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra")
(with-eval-after-load 'notdeft
  (define-key notdeft-mode-map (kbd "C-c h") 'notdeft-mode-hydra/body))

;; Visual (part 2)

;; hydra for visual settings
(defhydra my-hydra/visual (:color amaranth :hint nil
                           :pre (progn
                                  (require 'follow)
                                  (require 'hilit-chg)
                                  (require 'hl-line)
                                  (require 'display-line-numbers)
                                  (require 'face-remap)))
  "
Visual (_q_: quit)
_b_ : blink-cursor [% 5`blink-cursor-mode]   _F_ : follow       [% 5`follow-mode]   _f_ : font-lock    [% 5`font-lock-mode]
_H_ : hl-changes   [% 5`highlight-changes-mode]   _h_ : hl-line      [% 5`hl-line-mode]   _l_ : line-nums    [% 5`display-line-numbers-mode]
_p_ : show-paren   [% 5`show-paren-mode]   _s_ : scroll-bar   [% 5(frame-parameter nil 'vertical-scroll-bars)]   _S_ : hscroll-bar  [% 5(frame-parameter nil 'horizontal-scroll-bars)]
_T_ : transient-mk [% 5`transient-mark-mode]   _t_ : truncate-lns [% 5`truncate-lines]   _v_ : visual-line  [% 5`visual-line-mode]
_nr_ / _np_ / _nd_ / _nw_ : narrow to-region / to-page / to-defun / widen      [% 5(buffer-narrowed-p)]
_+_  / _-_  / _0_       : zoom   in        / out     / reset                 [% 5(if text-scale-mode text-scale-mode-amount nil)]
"
  ("q" nil :exit t)
  ("b" blink-cursor-mode)
  ("F" follow-mode)
  ("f" font-lock-mode)
  ("H" highlight-changes-mode)
  ("h" hl-line-mode)
  ("l" display-line-numbers-mode)
  ("p" show-paren-mode)
  ("s" toggle-scroll-bar)
  ("S" toggle-horizontal-scroll-bar)
  ("T" transient-mark-mode)
  ("t" toggle-truncate-lines)
  ("v" visual-line-mode)
  ("nr" narrow-to-region)
  ("np" narrow-to-page)
  ("nd" narrow-to-defun)
  ("nw" widen)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0)))

(defvar-local my-hydra/visual/emphasis--face-remap-cookies '()
  "Alist storing cookies for `face-remap-add-relative' calls.")

(defun my-hydra/visual/emphasis--toggle-lighten-face (face)
  "Toggle lightening of FACE color for emphasis or emphasis."
  (let ((face-remap-cookie-old (alist-get face my-hydra/visual/emphasis--face-remap-cookies)))
    (if face-remap-cookie-old
        (progn
          (face-remap-remove-relative face-remap-cookie-old)
          (setq my-hydra/visual/emphasis--face-remap-cookies
                (assq-delete-all
                 face
                 my-hydra/visual/emphasis--face-remap-cookies)))
      (let* ((light-color (color-lighten-name
                           (face-attribute face :foreground)
                           50)) ;; lighten color by 50 percent
             (face-remap-cookie-new (face-remap-add-relative
                                     face
                                     :foreground light-color)))
        (push `(,face . ,face-remap-cookie-new)
              my-hydra/visual/emphasis--face-remap-cookies)))))

(defhydra my-hydra/visual/emphasis (:color amaranth :hint nil)
  "
Visual → Emphasis (_q_: quit)
_c_ : comments      [% 3(null (assq 'font-lock-comment-face my-hydra/visual/emphasis--face-remap-cookies))]   _C_ : comment-delim  [% 3(null (assq 'font-lock-comment-delimiter-face my-hydra/visual/emphasis--face-remap-cookies))]   _d_ : doc            [% 3(null (assq 'font-lock-doc-face my-hydra/visual/emphasis--face-remap-cookies))]
"
  ("q" my-hydra/visual/body :exit t)
  ("c" (lambda ()
         (interactive)
         (my-hydra/visual/emphasis--toggle-lighten-face
          'font-lock-comment-face)))
  ("C" (lambda ()
         (interactive)
         (my-hydra/visual/emphasis--toggle-lighten-face
          'font-lock-comment-delimiter-face)))
  ("d" (lambda ()
         (interactive)
         (my-hydra/visual/emphasis--toggle-lighten-face
          'font-lock-doc-face))))

;; bind visual hydra
(global-set-key (kbd "C-c C-M-v i") 'my-hydra/visual/body)

;; add entrypoint to visual emphasis hydra to visual hydra
(defhydra+ my-hydra/visual nil
  ("e" my-hydra/visual/emphasis/body "→ Emphasis" :exit t))

;; provides toggleable modes that remove visual distractions
(use-package darkroom
  :config (setq darkroom-text-increase-scale 2))

;; extend visual hydra to support darkroom-mode and darkroom-tentative-mode
(eval
 `(defhydra+ my-hydra/visual
    ,(append my-hydra/visual/params '(:pre (require 'darkroom)))
    ,(concat my-hydra/visual/docstring
             "_dm_ : darkroom-mode            [% 3`darkroom-mode]   _dt_ : darkroom-tentative-mode  [% 3`darkroom-tentative-mode]
")
    ("dm" darkroom-mode :exit nil)
    ("dt" darkroom-tentative-mode :exit nil)))

;; color code by depth
(use-package prism
  :config
  (prism-set-colors :num 16
                    :desaturations (cl-loop for i from 0 below 16
                                            collect (* i 2.5))
                    :lightens (cl-loop for i from 0 below 16
                                       collect (* i 2.5))
                    :colors (list "saddle brown"
                                  "midnight blue"
                                  "dark green")
                    :comments-fn
                    (lambda (color)
                      (prism-blend color
                                   (face-attribute 'font-lock-comment-face
                                                   :foreground)
                                   0.25))
                    :strings-fn
                    (lambda (color)
                      (prism-blend color "white" 0.5))))

;; extend visual hydra to support prism-mode and prism-whitespace-mode
(eval
 `(defhydra+ my-hydra/visual
    ,(append my-hydra/visual/params '(:pre (require 'prism)))
    ,(concat my-hydra/visual/docstring
             "_Pm_ : prism-mode               [% 3`prism-mode]   _Pw_ : prism-whitespace-mode    [% 3`prism-whitespace-mode]
")
    ("Pm" prism-mode :exit nil)
    ("Pw" prism-whitespace-mode :exit nil)))

;; provides semantic coloring where same keywords are also colored the same
(use-package color-identifiers-mode
  :config (setq color-identifiers-coloring-method 'sequential
                color-identifiers:num-colors 10))

;; extend visual hydra to support color-identifiers-mode
(eval
 `(defhydra+ my-hydra/visual
    ,(append my-hydra/visual/params '(:pre (require 'color-identifiers-mode)))
    ,(concat my-hydra/visual/docstring
             "_C_  : color-identifiers-mode   [% 3`color-identifiers-mode]
")
    ("C" color-identifiers-mode :exit nil)))

;; hydra for toggling outline-minor-mode and running its commands
(defhydra my-hydra/visual/outline (:color amaranth :hint nil
                                   :pre (require 'outline))
  "
Visual → Outline [minor-mode-enabled=%`outline-minor-mode] (_q_: ←)
Mode    _m_ : toggle
Hide    _c_ : entry     _l_ : leaves    _d_ : subtree   _o_ : other
        _t_ : body
Show    _e_ : entry     _i_ : children  _k_ : branches  _s_ : subtree
        _a_ : all
"
  ("q" my-hydra/visual/body nil :exit t)
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
  ("m" outline-minor-mode))

;; add entry-point into outline hydra from visual hydra
(defhydra+ my-hydra/visual nil
  ("o" my-hydra/visual/outline/body "→ Outline" :exit t))

;; hydra for whitespace visualization and cleanup
(defhydra my-hydra/whitespace (:color teal :columns 3)
  "
Whitespace (_q_: quit)"
  ("q" nil nil)
  ("w" whitespace-mode "show-whitespace" :exit nil)
  ("t" (lambda () (interactive)
         (setq-local show-trailing-whitespace
                     (not show-trailing-whitespace))
         (message "show-trailing-whitespace: %s"
                  (if show-trailing-whitespace "yes" "no")))
   "show-trailing" :exit nil)
  ("n" whitespace-newline-mode "show-newline" :exit nil)
  ("c" whitespace-cleanup "cleanup")
  ("r" whitespace-report "report"))
(global-set-key (kbd "C-c C-M-w s") #'my-hydra/whitespace/body)

;; show pointer location column number in mode line
(setq column-number-mode t)

;; show matching parentheses with no delay
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Web

;; built-in Emacs text web browser
(use-package eww
  :ensure nil ;; built-in
  :commands (eww eww-follow-link)
  :bind (:map eww-mode-map
         ("I" . my-eww-toggle-images))
  :init (setq eww-search-prefix "https://duckduckgo.com/lite?q=")
  ;; don't render images in HTML pages by default
  :config (setq-default shr-inhibit-images t))

;; hydra for Emacs Web Wowser
(defhydra my-hydra/eww-mode (:color teal :columns 3)
  "
Emacs Web Wowser (_q_: quit)"
  ("q" nil nil)
  ("d" eww-download "download-link")
  ("G" eww "search")
  ("o" eww-open-file "open-file")
  ("l" eww-back-url "back")
  ("r" eww-forward-url "forward")
  ("g" eww-reload "reload")
  ("v" eww-view-source "view-source")
  ("w" eww-copy-url "copy-url")
  ("&" eww-browse-with-external-browser "browse-ext")
  ("b" eww-add-bookmark "bookmark-page")
  ("B" eww-list-bookmarks "bookmark-list")
  ("R" eww-readable "readable-only")
  ("F" eww-toggle-fonts "toggle-fonts")
  ("I" my-eww-toggle-images "toggle-images")
  ("M-C" eww-toggle-colors "toggle-colors")
  ("D" eww-toggle-paragraph-direction "toggle-text-dir")
  ("s" eww-switch-to-buffer "eww-switch-buf")
  ("S" eww-list-buffers "eww-list-buf")
  ("H" eww-list-histories "history")
  ("C" url-cookie-list "cookie-list"))

;; binding
(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "C-c C-M-m") #'my-hydra/eww-mode/body))

;; helper function for toggling images in Emacs Web Wowser
(defun my-eww-toggle-images ()
  "Toggle displaying of images when rendering HTML."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload)
  (message "Images are now %s" (if shr-inhibit-images "off" "on")))

(use-package restclient
  :defer t
  ;; assume request source files have ".http" suffix
  :mode ("\\.http\\'" . restclient-mode)
  :config
  ;; pulse *HTTP Response* buffer after receiving request response
  ;; adapted from https://github.com/jordonbiondo/.emacs.d/blob/master/init.el
  (defun my-restclient-pulse-buffer ()
    "Pulses the current buffer."
    (save-excursion
      (goto-char (point-min))
      (pulse-momentary-highlight-region (point-min) (point-max))))
  (add-hook 'restclient-response-loaded-hook #'my-restclient-pulse-buffer))

;; hydra for restclient
(defhydra my-hydra/restclient-mode (:color teal :columns 3)
  "
REST client (_q_: quit)"
  ("q" nil nil)
  ("c" restclient-http-send-current "send")
  ("r" restclient-http-send-current-raw "send-raw")
  ("v" restclient-http-send-current-stay-in-window "send-bg")
  ("n" restclient-jump-next "next" :exit nil)
  ("p" restclient-jump-prev "prev" :exit nil)
  ("." restclient-mark-current "mark")
  ("u" restclient-copy-curl-command "copy-curl")
  ("N"
   (lambda ()
     (interactive)
     (if (buffer-narrowed-p)
         (widen)
       (restclient-narrow-to-current)))
   "narrow" :exit nil)
  ("f"
   (lambda ()
     (interactive)
     (require 'json-mode nil t)
     (if (fboundp 'json-mode-pretty-print-dwim)
         (call-interactively 'json-mode-pretty-print-dwim)
       (message "Requires the `json-mode' package be installed.")))
   "fmt-json-rgn"))

;; binding for restclient hydra
(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-M-m") #'my-hydra/restclient-mode/body))

;; increase network security settings
(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 1024)
(setq network-security-level 'high)
(setq nsm-save-host-names t)

;; HTTP requests privacy settings
(setq url-cookie-untrusted-urls '(".*")) ;; no cookies
(setq url-privacy-level 'paranoid) ;; more private HTTP requests
(url-setup-privacy-info) ;; apply `url-privacy-level'

;; Writing

;; hydra for writing functions
(defhydra my-hydra/writing (:color amaranth :hint nil
                            :pre (require 'flyspell))
  "
Writing (_q_: quit)
Flyspell   [% 4(if flyspell-mode (if (eq flyspell-generic-check-word-predicate #'flyspell-generic-progmode-verify) 'prog t) nil)]   _f_ : toggle  _F_ : prog
"
  ("q" nil :exit t)
  ("f" flyspell-mode)
  ("F" flyspell-prog-mode))

;; bindings for writing hydra
(global-set-key (kbd "C-c C-M-w r") #'my-hydra/writing/body)

;; provides word lookups from a dictionary server
;; `dictionary-server' can be set to "localhost" to use a local
;; dictionary server like dictd or GNU Dico that implements RFC 2229
(use-package dictionary
  :init (setq dictionary-server "dict.org"
              dictionary-default-dictionary "*"))

;; add dictionary entrypoints to writing hydra
(eval
 `(defhydra+ my-hydra/writing
    ,(append my-hydra/writing/params '(:pre (require 'dictionary)))
    ,(concat my-hydra/writing/docstring "Dictionary          _s_ : search  _m_ : match
")
    ("s" dictionary-search :exit t)
    ("m" dictionary-match-words :exit t)))

;; thesaurus functions using Synosaurus
(use-package synosaurus
  :init (setq synosaurus-choose-method 'default
              synosaurus-backend 'synosaurus-backend-wordnet))

;; add synosaurus entrypoints to writing hydra
(eval
 `(defhydra+ my-hydra/writing
    ,(append my-hydra/writing/params '(:pre (require 'synosaurus)))
    ,(concat my-hydra/writing/docstring "Synosaurus [% 4`synosaurus-mode]   _S_ : toggle  _L_ : lookup  _R_ : replace _I_ : insert
")
    ("S" synosaurus-mode :exit nil)
    ("L" synosaurus-lookup :exit t)
    ("R" synosaurus-choose-and-replace :exit t)
    ("I" synosaurus-choose-and-insert :exit t)))

;; grammar checking functions using LanguageTool
(use-package langtool
  :init (setq langtool-default-language "en-US"
              langtool-language-tool-jar (expand-file-name "~/jars/languagetool-commandline.jar")))

;; add langtool functions to writing hydra
(eval
 `(defhydra+ my-hydra/writing
    ,(append my-hydra/writing/params '(:pre (require 'langtool)))
    ,(concat my-hydra/writing/docstring "LangTool            _w_ : check   _W_ : done    _l_ : lang    _c_ : correct-buf
")
    ("w" langtool-check nil :exit nil)
    ("W" langtool-check-done nil :exit nil)
    ("l" langtool-switch-default-language nil :exit nil)
    ("c" langtool-correct-buffer nil :exit nil)))

;; Other

;; *commented* currently do not store secrets using Emacs mechanisms
;; ;; Auth Sources, https://www.gnu.org/software/emacs/manual/auth.html
;; (if (file-exists-p "~/.authinfo.gpg")
;;     (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
;;   (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))

;; buffer-local `auto-save-visited-mode'
(use-package real-auto-save
  :defer t
  :config (setq real-auto-save-interval 10)) ;; save interval, in seconds

;; mouse settings
(when (display-graphic-p)
  (add-hook
   'after-init-hook
   (lambda ()
     ;; use super-left-click as middle-click (trackpad workaround)
     ;; (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
     ;; smooth scrolling, hold SHIFT/CONTROL for 5 line/full window increments
     (setq mouse-wheel-scroll-amount '(1
                                       ((shift) . 5)
                                       ((control) . nil)))
     ;; enable horizontal scrolling
     (setq mouse-wheel-flip-direction t ;; t/nil for trackpad/mouse
           mouse-wheel-tilt-scroll t))
   t))

;; useful extensions
(use-package crux
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

;; hydra for CRUX commands
(defhydra my-hydra/crux (:color teal :columns 3)
  "
CRUX (_q_: quit)"
  ("q" nil nil)
  ("M-o" crux-smart-open-line "newline" :exit nil)
  ("C-M-o" crux-smart-open-line-above "newline-above" :exit nil)
  ("J" crux-top-join-line "join-line" :exit nil)
  ("C-y" crux-duplicate-current-line-or-region "duplicate")
  ("C-;" crux-duplicate-and-comment-current-line-or-region "duplicate+comment")
  ("C" crux-cleanup-buffer-or-region "cleanup-buf/rgn")
  ("R" crux-rename-file-and-buffer "rename-file+buf")
  ("D" crux-delete-file-and-buffer "delete-file+buf")
  ("K" crux-kill-other-buffers "kill-other-bufs")
  ("S" crux-reopen-as-root "sudo-edit")
  ("V" crux-view-url "view-url")
  ("o" crux-open-with "open-external"))
(global-set-key (kbd "C-c C-M-x") #'my-hydra/crux/body)

;; hydra for help entrypoints
(defhydra my-hydra/help (:color teal :columns 4)
  "
Help (_q_: quit)"
  ("q" nil nil)
  ("a" apropos-command "apropos-cmd")
  ("d" apropos-documentation "apropos-doc")
  ("f" describe-function "desc-fun")
  ("v" describe-variable "desc-var")
  ("c" describe-key-briefly "desc-key-brief")
  ("k" describe-key "desc-key")
  ("b" describe-bindings "desc-bind")
  ("m" describe-mode "desc-mode")
  ("p" describe-package "desc-pkg")
  ("y" describe-syntax "desc-syntax")
  ("e" view-echo-area-messages "messages")
  ("l" view-lossage "lossage")
  ("i" info "info")
  ("s" info-lookup-symbol "info-symbol")
  ("w" where-is "where-is"))

;; bind help hydra
(global-set-key (kbd "C-c C-M-S-h") 'my-hydra/help/body)

;; set *scratch* buffer major-mode to fundamental-mode
(setq initial-major-mode 'fundamental-mode)

;; scroll a line at a time at window edge
(setq scroll-conservatively 101)

;; turn off audio and visual bells
(setq ring-bell-function 'ignore)

;; suppress auto-revert minibuffer messages
(setq auto-revert-verbose nil)

;; suppress splash screen that appears on startup by default
(setq inhibit-startup-message t)

;; manage system processes in Linux
(when (eq system-type 'gnu/linux)
  (setq proced-format 'medium)
  (global-set-key (kbd "C-x p") #'proced))

;; visit large files without loading it entirely
(use-package vlf
  :config (require 'vlf-setup))

;; automatically disable major and minor modes that can slow down
;; Emacs when visiting files with long lines, Emacs 27+ only
(when (require 'so-long nil :noerror)
  (global-so-long-mode 1)
  ;; leave major modes alone, only disable minor modes
  ;; increase threshold before so-long action is invoked
  (setq so-long-action 'so-long-minor-mode
        so-long-max-lines 10
        so-long-threshold 500))

(defhydra+ my-hydra/buffer nil
  ("l" so-long-mode "so-long")
  ("L" so-long-minor-mode "so-long-mm"))

;; OS-specific / Mac OS X

;; on Mac OS X, use Option keys as Meta and file polling for auto-revert
(when (eq system-type 'darwin)
  (setq auto-revert-use-notify nil ;; OS X does not support file notifications
        mac-option-modifier 'meta ;; use Option key as Meta
        mac-right-option-modifier 'left ;; right Option uses left's mapping
        mac-command-modifier 'super)) ;; keep Super key as is

(when (eq window-system 'ns)
  (setq mac-option-modifier 'meta ;; use Option key as Meta
        mac-right-option-modifier 'left ;; right Option uses left's mapping
        mac-command-modifier 'super)) ;; keep Super key as is

;; revert Command keys in Emacs Mac Port to match Emacs for Mac OS X bindings
(when (eq window-system 'mac)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'left
        mac-command-modifier 'super)
  (global-set-key (kbd "s-'") 'next-multiframe-window)
  (global-set-key (kbd "s-,") 'customize)
  (global-set-key (kbd "s-`") 'other-frame)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save) ;; ns-copy-including-secondary
  (global-set-key (kbd "s-d") 'isearch-repeat-backward)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-g") 'isearch-repeat-forward)
  ;; (global-set-key (kbd "s-h") 'ns-do-hide-emacs) ;; done by default
  (global-set-key (kbd "s-j") 'exchange-point-and-mark)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-m") 'iconify-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  ;; (global-set-key (kbd "s-o") 'ns-open-file-using-panel) ;; no equivalent
  ;; (global-set-key (kbd "s-p") 'ns-print-buffer) ;; no equivalent
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-u") 'revert-buffer)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-w") 'delete-frame)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-y") 'yank) ;; ns-paste-secondary
  (global-set-key (kbd "s-z") 'undo))

;; case-insensitive sorting in Dired
;; http://pragmaticemacs.com/emacs/case-insensitive-sorting-in-dired-on-os-x/
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-ignore-case t)
  (setq ls-lisp-use-string-collate nil)
  ;; customise the appearance of the listing
  (setq ls-lisp-verbosity '(links uid))
  ;; (setq ls-lisp-format-time-list '("%b %e %H:%M" "%b %e %Y"))
  (setq ls-lisp-use-localized-time-format t))

;; open files in dired mode using 'open' when using Mac OS X,
;; bindings are 'z' in Dired which is also in the Dired mode hydra
(with-eval-after-load 'dired
  (defun dired--mac-open-file-at-pt ()
    "Opens file at point in Dired using Mac OS X 'open' command."
    (interactive)
      (let ((filename (dired-get-file-for-visit)))
        (start-process "default-app" nil "open" filename)))
  (define-key dired-mode-map (kbd "z") #'dired--mac-open-file-at-pt)
  (defhydra+ my-hydra/dired-mode nil
    ("z" dired--mac-open-file-at-pt "mac-open")))

;; exclude Emacs source files from recentf history on Mac OS X
(add-to-list 'recentf-exclude "^/Applications/Emacs.app/")

;; add my-hydra/buffer head to open Finder at current buffer directory in OS X
(when (eq system-type 'darwin)
  (defun my-open-finder (&optional path)
    "Opens a new Finder window to PATH if provided,
or the current buffer file or directory if not (Mac OS X)."
    (interactive)
    (let* ((my-path (cl-some 'identity (list path
                                             (buffer-file-name)
                                             default-directory)))
           (my-full-path (expand-file-name my-path))
           (my-process-args (list "my-open-finder" nil
                                  "open" "-R" my-full-path)))
      (if (eq system-type 'darwin)
          (apply 'start-process my-process-args)
        (message "my-open-finder is Mac OS X-only"))))
  (defhydra+ my-hydra/buffer nil
    ("e" my-open-finder "open-finder" :exit t)))

;; scale up LaTeX fragment preview images on OS X
(if (and (display-graphic-p)
         (eq system-type 'darwin)
         (executable-find "dvipng"))
    (setq org-format-latex-options (plist-put org-format-latex-options
                                              :scale 1.5)))

;; enable toggling of ligatures in visual hydra when using emacs-mac port
(when (fboundp 'mac-auto-operator-composition-mode)
  (defhydra+ my-hydra/visual nil
    ("L" mac-auto-operator-composition-mode "toggle-ligature" :exit nil)))

;; increment DocView DPI resolution for Mac Retina screens
;; when using emacs-mac port
(when (eq window-system 'mac)
  (setq doc-view-resolution 300))

;; workaround for problematic entries in `load-history' which affects
;; Emacs 27+ on some systems, probably to do with the portable dumper
;; mainly affecting Emacs for OS X
(when (and (not (version< emacs-version "27"))
           (eq window-system 'ns))
  (defun load-history-filename-element (file-regexp)
    "Get the first elt of `load-history' whose car matches FILE-REGEXP.
Return nil if there isn't one."
    (let* ((loads load-history)
           (load-elt (and loads (car loads))))
      (save-match-data
        (while (and loads
                    (or (null (car load-elt))
                        (not (and (stringp (car load-elt)) ;; skip non-strings
                                  (string-match file-regexp (car load-elt))))))
          (setq loads (cdr loads)
                load-elt (and loads (car loads)))))
      load-elt)))

(provide 'init)
;;; init.el ends here
