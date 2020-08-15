;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Generated: Sat Aug 15 15:41:16 2020

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

;; copy environment variables from shell
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

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

(use-package helm-icons
  :after helm
  :config (helm-icons-enable))

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

;; Custom variables and utility functions / Custom variables

(defcustom my-system-open-command "xdg-open"
  "System command to open file/URL according to preferred app by filetype.
Usually \"xdg-open\" on Linux and \"open\" on Mac."
  :type 'string
  :group 'convenience)

;; Custom variables and utility functions / Utility functions

(defun my-after-jump-context-actions (&rest _)
  "Useful context actions to perform after jumping to a new location.
This is meant for use with `advice-add' with the :after
combinator.

One useful context action example is to run `org-show-context'
after jumping to an Org buffer location to ensure the region
around the new point location is visible."
  (cond ((eq major-mode 'org-mode) (org-show-context))))

;; helper function for pulsing the current line, adapted from
;; https://protesilaos.com/dotemacs/#h:6bbc41d6-da7c-4301-84c6-c5887c29283f
(defun my-pulse-line (&rest _)
    "Pulse the current line .
If the point is at the newline at the end of the buffer, pulse
the line before that. Additionally, the current line is not pulsed
if the point is in the minibuffer."
    (unless (minibufferp)
      (let ((start (if (and (eobp)
                            (= (point) (line-beginning-position)))
                       (line-beginning-position 0)
                     (line-beginning-position)))
            (end (line-beginning-position 2))
            (pulse-delay .1))
        (pulse-momentary-highlight-region start end nil))))

(defun my-save-and-bury-buffer (&rest _)
  "Save and bury the current buffer."
  (save-buffer)
  (bury-buffer))

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

;; recently opened files
(setq recentf-max-menu-items 10
      recentf-max-saved-items 100
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
(global-set-key (kbd "C-c C-M-r") #'recentf-open-files)

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

;; protect these buffers, locking them to make them unkillable
(dolist (buf '("*scratch*" "*Messages*"))
  (with-current-buffer buf
    (emacs-lock-mode 'kill)))

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

;; use font icons in Ibuffer
(when (display-graphic-p)
  (use-package all-the-icons-ibuffer
    :after (all-the-icons ibuffer)
    :config (all-the-icons-ibuffer-mode 1)))

;; quick buffer switching (configured to be within a project)
(use-package nswbuff
  :after projectile
  :bind (("<C-tab>" . nswbuff-switch-to-next-buffer)
         ("<C-S-tab>" . nswbuff-switch-to-previous-buffer))
  :init
  (setq nswbuff-buffer-list-function #'nswbuff-projectile-buffer-list
        nswbuff-clear-delay 2
        nswbuff-display-intermediate-buffers t
        ;; exclude all internal buffers from the nswbuff switch list
        nswbuff-exclude-buffer-regexps '("^ "
                                         "^\\*.*\\*"
                                         "org-src-fontification")
        nswbuff-exclude-mode-regexp (mapconcat
                                     'identity
                                     '("dired-mode"
                                       "gnus-mode")
                                     "\\|")
        nswbuff-start-with-current-centered nil)
  :config
  ;; unbind C-tab in org-mode to not conflict with nswbuff global binding
  (with-eval-after-load 'org
    (unbind-key "<C-tab>" org-mode-map)))

;; Buffers, windows, frames, workspaces / Window management

;; traverse window config changes, C-c left/right to undo/redo
;; uncomment to not bind C-c left/right keys by default
;; (setq winner-dont-bind-my-keys t)
;; enable winner-mode at end of initialization
(add-hook 'after-init-hook #'winner-mode)

;; popup window manager, also auto-closes special buffers like
;; *compilation* and *Completions*
(use-package popwin
  :config
  (popwin-mode 1)
  (global-set-key (kbd "C-z") popwin:keymap))

;; window navigation and management
(use-package ace-window
  :config
  (setq aw-background t
        aw-char-position 'left
        aw-ignore-current nil
        aw-scope 'frame)
  (global-set-key (kbd "M-o") #'ace-window))

;; Buffers, windows, frames, workspaces / Frame management

(use-package transpose-frame
  :bind (("C-x 5 [" . rotate-frame-anticlockwise)
         ("C-x 5 ]" . rotate-frame-clockwise)))

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

(use-package eshell-z
  :after eshell)

;; make shell prompts read-only
(setq comint-prompt-read-only t)

;; kill term buffers with 'q' after session end
(defun term-handle-exit--close-buffer-on-cmd (&rest args)
  "Kill term buffer with 'q' after session exit."
  (when (null (get-buffer-process (current-buffer)))
    (use-local-map (let ((keymap (make-sparse-keymap)))
                     (define-key keymap (kbd "q")
                       (lambda ()
                         (interactive)
                         (kill-buffer (current-buffer))))
                     keymap))))
(advice-add 'term-handle-exit :after #'term-handle-exit--close-buffer-on-cmd)

(use-package vterm
  :if (and module-file-suffix
           (executable-find "cmake")
           (executable-find "libtool"))
  :init
  (setq vterm-buffer-name-string "vterm %s"
        vterm-clear-scrollback-when-clearing t
        vterm-eval-cmds '(("vterm-clear-scrollback" vterm-clear-scrollback))
        vterm-kill-buffer-on-exit t
        vterm-shell (or (executable-find "fish") shell-file-name)))

(defun vterm-switchb ()
  "Call `switch-to-buffer' but only for vterm buffers."
  (interactive)
  (let ((completion-regexp-list '("\\`vterm .*")))
    (call-interactively #'switch-to-buffer)))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-b") #'vterm-switchb))

;; convenience functions for sent commands to an active tmux session
;; adapted from https://explog.in/notes/tmux.html

;; track previously sent tmux commands on per-buffer basis
(setq tmux-send--last-command nil)
(make-variable-buffer-local 'tmux-send--last-command)

(defun tmux-send (command)
  "Sends the specified COMMAND to the currently active tmux pane."
  (interactive "sCommand: ")
  (setq tmux-send--last-command command)
  (call-process "tmux" nil nil nil "send-keys" command "Enter"))

(defun tmux-resend ()
  "Resends previously sent command to currently active tmux pane."
  (interactive)
  (if tmux-send--last-command
      (call-process "tmux" nil nil nil "send-keys" tmux-send--last-command "Enter")
    (message "No previously sent command from the current buffer!")))

;; Comparison tools

;; always set up Ediff control window in the same frame as the diff,
;; open with horizontal window split instead of the default vertical
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; copy diff hunk from buffers A and B to C in 3-way Ediff
;; adapted from https://stackoverflow.com/a/29757750
(defun ediff-copy-A-and-B-to-C (arg)
  "Copies ARGth diff region from both buffers A and B to C.
ARG is a prefix argument.  If nil, copy the current difference region."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (if (eq arg '-) (setq arg -1)) ;; translate neg arg to -1
  (if (numberp arg) (ediff-jump-to-difference arg))
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A
                                               ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B
                                               ediff-control-buffer)))
  ;; recenter with rehighlighting, but no messages
  (ediff-recenter))
(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (when ediff-3way-job
              (define-key ediff-mode-map "d" 'ediff-copy-A-and-B-to-C))))
(with-eval-after-load 'ediff-help
  (setq ediff-long-help-message-compare3
        (concat ediff-long-help-message-compare3
                "                                                 |"
                "  d -copy A + B regions to C
"
)))

;; view and compare directory trees, like Beyond Compare
(use-package ztree
  :bind (("C-x D" . ztree-dir)
         ("C-c C-M--" . ztree-diff))
  :config
  (setq ztree-dir-move-focus t ;; RET in ztree-dir also moves focus
        ztree-draw-unicode-lines t ;; unicode lines
        ztree-show-number-of-children t)) ;; show number of files in subdir tree

;; convenience navigation bindings for `ztreedir-mode' and `ztreediff-mode'
(with-eval-after-load 'ztree-view
  (define-key ztree-mode-map (kbd "n") #'ztree-next-line)
  (define-key ztree-mode-map (kbd "p") #'ztree-previous-line))

;; DevOps

(when (executable-find "docker")
  (use-package docker
    :bind ("C-c C-M-d" . docker)))

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

;; bind "z" in dired-mode to open file at point using system command
;; to open files by type
(with-eval-after-load 'dired
  (defun dired--open-file-at-pt ()
    "Opens file at point in Dired using system open command.
This opens the file using the preferred application by filetype."
    (interactive)
    (let ((filename (dired-get-file-for-visit)))
      (start-process "default-app"
                     nil
                     my-system-open-command
                     filename)))
  (define-key dired-mode-map (kbd "z") #'dired--open-file-at-pt))

;; have recentf track dired buffers as well
;; from https://www.emacswiki.org/emacs/RecentFiles#toc21

(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)

(defun recentd-track-closed-file ()
  "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-remove-if-non-kept default-directory)))

(add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
(add-hook 'kill-buffer-hook 'recentd-track-closed-file)

(use-package dired-filter
  :bind (:map dired-mode-map
         ("/" . dired-filter-map))
  :hook (dired-mode . dired-filter-mode)
  :init (setq-default dired-filter-stack nil))

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
(global-set-key (kbd "C-c C-M-y") #'my-yank-from-kill-ring)

;; typing text replaces the active (i.e. selected) region, if any is selected
(delete-selection-mode)

;; use single spaces after sentences
(setq sentence-end-double-space nil)

;; enable transparent editing of GPG files
(require 'epa-file)
(epa-file-enable)

;; display available bindings in popup
(use-package which-key
  :bind ("C-c C-M-?" . which-key-show-top-level)
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
  :init (setq iedit-toggle-key-default (kbd "C-;"))
  :config
  ;; advise iedit functions that jump to new point locations to
  ;; perform context actions after they are run
  (dolist (jump-fun '(iedit-next-occurrence
                      iedit-prev-occurrence
                      iedit-goto-first-occurrence
                      iedit-goto-last-occurrence
                      iedit-expand-to-occurrence))
    (advice-add jump-fun :after #'my-after-jump-context-actions)))

(use-package symbol-overlay
  :demand t
  :init
  ;; don't use `symbol-overlay-map' as it conflicts with `iedit-mode',
  ;; a transient is be defined later to access symbol-overlay commands
  (setq symbol-overlay-inhibit-map t)
  :config
  ;; advise symbol-overlay jump functions to perform context actions
  ;; after they are run
  (dolist (jump-fun '(symbol-overlay-jump-next
                      symbol-overlay-jump-prev
                      symbol-overlay-switch-forward
                      symbol-overlay-switch-backward))
    (advice-add jump-fun :after #'my-after-jump-context-actions)))

;; multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
  :init (setq mc/always-run-for-all nil
              mc/always-repeat-command nil
              mc/insert-numbers-default 1)
  :config
  ;; decrease width of the multiple-cursors bar
  ;; setting a height of 1 ends up rendering a thick bar
  ;; probably because it is too small a value
  (set-face-attribute 'mc/cursor-bar-face nil :height 10))

;; expandable snippet template system
(use-package yasnippet
  :defer 1 ;; load asynchronously after startup
  :config
  ;; (use-package yasnippet-snippets) ;; official snippets
  (use-package auto-yasnippet) ;; enable creation of temporary snippets
  ;; remove default bindings to avoid conflicts with other packages
  ;; removing prefix bindings also removes bindings that use them
  (unbind-key "\C-c&" yas-minor-mode-map)
  (unbind-key "\C-c" yas-minor-mode-map)
  (yas-global-mode 1))

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

;; bind over `zap-to-char' (defaults to "M-x") with `zap-up-to-char'
(global-set-key [remap zap-to-char] #'zap-up-to-char)

(global-set-key [remap just-one-space] #'cycle-spacing)

;; Join next line to end of current line, like "J" in Vim
(defun my-join-next-line ()
  "Join the next line to the end of the current line."
  (interactive)
  (let ((col (current-column)))
    (join-line -1)
    (move-to-column col)))

(global-set-key (kbd "C-S-j") #'my-join-next-line)

(defun my-open-line-below (n)
  "Open a new line below and go to it.
With arg N, insert N newlines."
  (interactive "*p")
  (end-of-line)
  (newline n)
  (indent-according-to-mode))

(defun my-open-line-above (n)
  "Open a new line above and go to it.
With arg N, insert N newlines."
  (interactive "*p")
  (beginning-of-line)
  (newline n)
  (forward-line (- n))
  (indent-according-to-mode))

;; bind over `open-line' ("C-o") with `my-open-line-below'
(global-set-key [remap open-line] #'my-open-line-below)
;; binding for `my-open-line-above
(global-set-key (kbd "C-S-o") #'my-open-line-above)

;; show keyboard macros and latest commands as Elisp, adapted from
;; https://emacsnotes.wordpress.com/2018/11/15/elmacro-write-emacs-lisp-snippet-even-when-you-arent-a-programmer/
(use-package elmacro
  :config
  (elmacro-mode 1)
  ;; add Elmacro entry under Tools in the menu bar
  (easy-menu-define my-elmacro-menu nil
    "Menu for Elmacro."
    '("Elmacro"
      ["Elmacro Mode"
       (customize-save-variable 'elmacro-mode (not elmacro-mode))
       :style toggle
       :selected elmacro-mode
       :help "(elmacro-mode &optional ARG)\n\nToggle emacs activity recording (elmacro mode).\nWith a prefix argument ARG, enable elmacro mode if ARG is\npositive, and disable it otherwise. If called from Lisp, enable\nthe mode if ARG is omitted or nil."]
      "--"
      ["Show Last Commands"
       elmacro-show-last-commands
       :active elmacro-mode
       :help "(elmacro-show-last-commands &optional COUNT)\n\nTake the latest COUNT commands and show them as emacs lisp.\n\nThis is basically a better version of `kmacro-edit-lossage'.\n\nThe default number of commands shown is modifiable in variable\n`elmacro-show-last-commands-default'.\n\nYou can also modify this number by using a numeric prefix argument or\nby using the universal argument, in which case it'll ask for how many\nin the minibuffer."]
      ["Show Last Macro"
       elmacro-show-last-macro
       :active elmacro-mode
       :help "(elmacro-show-last-macro NAME)\n\nShow the last macro as emacs lisp with NAME."]
      "--"
      ["Clear Command History"
       elmacro-clear-command-history
       :active elmacro-mode
       :help "(elmacro-clear-command-history)\n\nClear the list of recorded commands."]))
  (dolist (menu-item '(["--" nil] my-elmacro-menu ["--" nil]))
    (easy-menu-add-item
     (current-global-map)
     '("menu-bar" "Tools")
     menu-item)))

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
    (seq-let [format-string authors] args
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
    (seq-let [field format-string result] args
      (if (string-equal field "tags")
          (let ((base-tags (plist-get result :tags))
                (base-orig-tags (plist-get result :orig-tags))
                (query (if (boundp 'notmuch-search-query-string)
                           notmuch-search-query-string
                         nil)))
            (seq-let [tags orig-tags] (notmuch--filter-common-search-tags
                                       base-tags base-orig-tags query)
              (insert (format format-string
                              (notmuch-tag-format-tags tags orig-tags)))))
        (apply orig-fun args))))

  (defun notmuch-tree-format-field--filter-search-tags (orig-fun &rest args)
    "Advises the `notmuch-tree-format-field' function
to filter search tags from the displayed tags like in Gmail.
ORIG-FUN should be `notmuch-tree-format-field' and ARGS are the
original arguments passed to it."
    (seq-let [field format-string msg] args
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
               (seq-let [tags orig-tags] (notmuch--filter-common-search-tags
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
  (dolist (keymap '(notmuch-hello-mode-map
                    notmuch-search-mode-map
                    notmuch-tree-mode-map))
    (define-key keymap (kbd "C-t")
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
    (dolist (keymap '(notmuch-hello-mode-map
                      notmuch-search-mode-map
                      notmuch-show-mode-map
                      notmuch-tree-mode-map))
      (define-key keymap (kbd "M") #'org-msg-mode))))

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

;; Marks and markers

;; backtrack through the entire xref--marker-ring in a single action
(defun xref-pop-marker-stack-all ()
  "Pop back to where `xref-find-definitions' was first invoked.
\\[xref-find-definitions] is the current binding for `xref-find-definitions'."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let ((marker (ring-remove ring nil))) ;; oldest marker
      (switch-to-buffer (or (marker-buffer marker)
                            (user-error "The marked buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil)
      (run-hooks 'xref-after-return-hook)
      (xref-clear-marker-stack)))) ;; clear the rest of the marker stack

;; hydra for manipulating and managing marks and markers
(defhydra my-hydra/marks-and-markers (:color amaranth :columns 3)
  "
Marks / Markers (_q_: quit)"
  ("q" nil nil :exit t)
  ("SPC" (lambda ()
           (interactive)
           (push-mark))
   "mark-push" :exit t)
  ("S-SPC" (lambda ()
             (interactive)
             (set-mark-command t))
   "mark-pop")
  (")" mark-sexp "mark-sexp")
  ("}" mark-paragraph "mark-paragraph")
  ("]" mark-defun "mark-defun")
  ("b" mark-whole-buffer "mark-whole-buf")
  ("x" exchange-point-and-mark "exchange-pt-mk")
  ("." (lambda ()
         (interactive)
         (xref-push-marker-stack))
   "xref-push-marker" :exit t)
  ("," xref-pop-marker-stack "xref-pop-marker")
  ("<" xref-pop-marker-stack-all "xref-pop-markers")
  ("c" (lambda ()
         (interactive)
         (xref-clear-marker-stack)
         (message "Cleared xref--marker-ring"))
   "xref-clear-markers"))
(global-set-key (kbd "C-c C-M-,") 'my-hydra/marks-and-markers/body)

(with-eval-after-load 'helm
  (defhydra+ my-hydra/marks-and-markers nil
    ("m" helm-mark-ring "helm-marks-buf" :exit t)
    ("M" helm-all-mark-rings "helm-marks-all" :exit t)))

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
         ("C-c C-S-a" . csv-align-visible-fields))
  :config
  (setq csv-align-style 'auto) ;; `csv-align-fields' left/right-aligns text/numbers
  (defun csv-align-visible-fields ()
    "Align visible lines in `csv-mode'. Useful for large CSV files where
`csv-align-fields' can take a very long time to run."
    (interactive)
    (csv-align-fields nil (window-start) (window-end))))

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

(use-package markdown-toc
  :after markdown-mode)

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
        neuron-tag-specific-title-faces '(("stub" neuron-stub-face)))
  :config
  ;; push location on to marker stack before following neuron link
  ;; so backtracking is possible via `xref-pop-marker-stack' or "M-,"
  (advice-add #'neuron-follow-thing-at-point :before #'xref-push-marker-stack))

;; Org-mode

;; set Org directory and inbox file
(setq org-directory (file-name-as-directory (expand-file-name "~/org"))
      my-org-agenda-inbox (concat org-directory "inbox.org"))

;; basic Org-mode settings
(setq org-adapt-indentation nil ;; don't auto-indent when promoting/demoting
      org-attach-dir-relative t ;; use relative directories when setting DIR property using `org-attach-set-directory'
      ;; org-blank-before-new-entry '((heading . nil) ;; don't auto-add new lines
      ;;                              (plain-list-item . nil)) ;; same as above
      org-catch-invisible-edits 'show-and-error
      org-confirm-babel-evaluate nil ;; don't confirm before evaluating code blocks in Org documents
      org-cycle-separator-lines 2 ;; collapse single item separator lines when cycling
      org-edit-src-content-indentation 2
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-hide-emphasis-markers nil
      org-hide-leading-stars nil
      org-highlight-latex-and-related '(latex script entities) ;; highlight LaTeX fragments with the `org-highlight-latex-and-related' face
      org-image-actual-width (list (/ (display-pixel-width) 3)) ;; auto-resize displayed images to one-third of display width
      org-link-file-path-type 'adaptive ;; use relative paths for links to files in Org file dir or subdirs, absolute otherwise
      org-log-done 'time ;; log time that task was marked DONE
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
      org-use-speed-commands t)

;; make sure UUIDs generated for Org usage are alway upcased, which
;; solves issues with synced directories, for example Linux generates
;; lower case UUIDs while Mac generates upper case UUIDs.
(with-eval-after-load 'org-id
  (defun org-id-uuid--around-upcase (orig-fun &rest args)
    "Advice for `org-id-uuid' to upcase the uuids it outputs.
ORIG-FUN is the original function.
ARGS are the arguments provided to ORIG-FUN."
    (let ((uuid (apply orig-fun args)))
      (upcase uuid)))
  (advice-add 'org-id-uuid :around
              'org-id-uuid--around-upcase))

(defun my-org-open-line-below (n)
  "Insert a new row in tables, call `my-open-line-below' elsewhere.
If `org-special-ctrl-o' is nil, call `my-open-line-below' everywhere.
As a special case, when a document starts with a table, allow to
call `open-line' on the very first character."
  (interactive "*p")
  (if (and org-special-ctrl-o (/= (point) 1) (org-at-table-p))
      (org-table-insert-row)
    (my-open-line-below n)))

;; bind over `org-open-line' to call `my-org-open-line-below' instead
;; making it consistent with customized global-mode-map "C-o"
(with-eval-after-load 'org-keys
  (define-key org-mode-map (kbd "C-o") #'my-org-open-line-below))

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
                      ;; work-related meeting type
                      ("internal" . ?I)
                      ("external" . ?X)
                      ;; work-related project category
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
  ("p" (lambda (n)
         (interactive "p")
         (if org-occur-highlights
             (previous-error n)
           (org-previous-visible-heading n)))
   "previous")
  ("n" (lambda (n)
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

(add-hook 'org-mode-hook #'visual-line-mode)

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
    :keymap (let ((keymap (make-sparse-keymap)))
              ;; <left>/<right> = previous/next slide
              (define-key keymap (kbd "<up>") 'scroll-down-line)
              (define-key keymap (kbd "<down>") 'scroll-up-line)
              (define-key keymap (kbd "s-<up>") 'beginning-of-buffer)
              (define-key keymap (kbd "s-<down>") 'end-of-buffer)
              (define-key keymap (kbd "s-<left>") 'org-present-beginning)
              (define-key keymap (kbd "s-<right>") 'org-present-end)
              (define-key keymap (kbd "f") 'toggle-frame-fullscreen)
              (define-key keymap (kbd "q") 'org-present-quit)
              (define-key keymap (kbd "-") 'text-scale-decrease)
              (define-key keymap (kbd "+") 'text-scale-increase)
              keymap))
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

;; load Org backend for exporting to Markdown
(with-eval-after-load 'org
  (require 'ox-md))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init (setq org-superstar-headline-bullets-list '("◉" "◇" "○" "▷")
              ;; don't prettify plain lists, which can be slow
              org-superstar-prettify-item-bullets nil))

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

;; start server and load org-protocol
(server-mode 1)
(require 'org-protocol)

;; add capture template for web snippets
(setq org-websnippet-capture-file "scratch/websnippets.org")
(push `("W" "Capture web snippet using org-protocol" entry
        (file+headline ,org-websnippet-capture-file "Unsorted")
        "* %?%:description\n:PROPERTIES:\n:URL: %:link\n:ADDED: %U\n:END:\n%:initial\n")
      org-capture-templates)

;; Programming / Buffer reformatter macro

;; defines the `reformatter-define' macro that allows definition of
;; commands that run reformatters on the current buffer
(use-package reformatter)

;; Programming / Flycheck syntax checker

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
  ;; default modes within which to use Flycheck
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode))

;; Programming / DevSkim and Flycheck

(when (executable-find "devskim")
  (use-package flycheck-devskim
    :ensure nil ;; in site-lisp subfolder within user emacs directory
    :config
    (setq flycheck-devskim-executable "devskim")
    (with-eval-after-load 'lsp-diagnostics
      (defun lsp-diagnostics--flycheck-enable--after-add-devskim (&rest _)
       "Chain devskim checker on the lsp checker after it is enabled."
       (flycheck-add-next-checker 'lsp 'devskim))
     (advice-add 'lsp-diagnostics--flycheck-enable :after
                 #'lsp-diagnostics--flycheck-enable--after-add-devskim))))

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
              lsp-diagnostic-package :flycheck ;; use Flycheck for syntax checking
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
  ("a" my-hydra/lsp-actions/body "→ Actions")
  ("I" (lambda ()
         (interactive)
         (lsp-install-server t))
   "install"))
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
  :config (add-hook 'dap-stopped-hook
                    (lambda (arg) (call-interactively #'dap-hydra))))

;; add dap-mode debugging function entry points to lsp-mode hydra
(with-eval-after-load 'lsp-mode
  (defhydra+ my-hydra/lsp nil
    ("d" dap-debug "dap-debug" :exit t)
    ("D" dap-debug-edit-template "dap-template" :exit t)))

;; Programming / Emacs Lisp

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

(setq python-shell-interpreter "py"
      python-shell-interpreter-args ""
      python-shell-prompt-detect-failure-warning nil)

(with-eval-after-load 'python
  (add-to-list 'python-shell-completion-native-disabled-interpreters "py"))

;; modify Python syntax table to keep underscores within a word
;; boundary when editing Python buffers
(with-eval-after-load 'python
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))

(with-eval-after-load 'reformatter
  ;; define `python-black-format-buffer', `python-black-format-region'
  ;; and `python-black-format-on-save-mode'
  (reformatter-define python-black-format
    :program "black"
    :args '("-")
    :group 'python
    :lighter 'PyBlFmt)
  ;; dwim function that calls `python-black-format-region' if a region
  ;; is selected, or `python-black-format-buffer' otherwise
  (defun python-black-format-buffer-or-region ()
       "Format the current Python buffer or a region if selected.
Formatting a selected region only works on top-level objects."
       (interactive)
       (cond
        ((use-region-p) (python-black-format-region (region-beginning)
                                                    (region-end)))
        (t (python-black-format-buffer)))))

;; live coding in python
(use-package live-py-mode)

(use-package lsp-pyright
  :defer t
  :init
  (defun lsp-pyright--setup ()
    "Convenience function for setting up lsp-pyright."
    ;; load packages if deferred
    (require 'lsp-mode)
    (require 'lsp-pyright)
    ;; start LSP client
    (lsp-mode))
  (add-hook 'python-mode-hook #'lsp-pyright--setup t))

(add-hook 'python-mode-hook
          (lambda ()
            (require 'dap-python))
          t)

;; Programming / R

;; support for R language using Emacs Speaks Statistics
;; linting is configured here to use Flycheck
(use-package ess
  :mode ("\\.R$" . R-mode)
  :commands (R-mode ess-switch-to-ESS)
  :init (setq ess-eval-visibly 'nowait
              ess-default-style 'RStudio
              ;; disable ESS auto-use of Flymake since using Flycheck
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

(use-package poly-R)

;; Programming / Racket

(use-package racket-mode
  :defer t)

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

;; Org TODOs for projectile projects
;; use `org-capture' to capture and store TODOs for the current project
;; in `org-projectile-per-project-filepath' at the project's root directory
(use-package org-projectile
  :after (org projectile)
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")
  (push (org-projectile-project-todo-entry) org-capture-templates))

;; binding for calling Magit
(use-package magit
  :commands magit-status
  :bind ("C-c C-M-g" . magit-status))

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

;; Browse older versions of Git-controlled files
(use-package git-timemachine
  :commands git-timemachine
  :bind ("C-c C-M-S-g" . git-timemachine))

;; per-project file trees
(use-package treemacs
  :demand t
  :bind ("C-c C-M-S-t" . treemacs)
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

;; use icons from all-the-icons in Treemacs
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons")
  ;; reduce tab-width to fix spacing between icons and text
  (add-hook 'treemacs-mode-hook
            (lambda () (setq-local tab-width 2)))
  ;; do the same if `helm-icons' is being used
  (with-eval-after-load 'helm-icons
    (add-hook 'helm-after-initialize-hook
              (lambda ()
                (with-helm-buffer
                  (setq tab-width 2))))))

;; Search

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
  ("rs" query-replace "replace string")
  ("rr" query-replace-regexp "replace regexp")
  ("kg" kill-grep "kill-grep")
  ("." (lambda ()
         (interactive)
         (call-interactively #'xref-find-definitions))
   "xref-find-def"))
(global-set-key (kbd "C-c C-M-/") 'my-hydra/search/body)

;; bind over occur entry with helm-occur in search hydra
(with-eval-after-load 'helm
  (defhydra+ my-hydra/search nil
    ("oo" helm-occur "occur")
    ("ov" helm-occur-visible-buffers "occur-visible")))

;; "C-c C-p" in grep bufs allow writing with changes pushed to files
(use-package wgrep
  :config (setq wgrep-auto-save-buffer nil
                wgrep-too-many-file-length 10))

(when (executable-find "rg")
  (use-package rg
    :bind ("<f6>" . rg-menu)))

(when (executable-find "rg")
  (defhydra+ my-hydra/search nil
    ("R" rg-menu "ripgrep" :exit t)))

;; show current and total search matches, and preview query replace results
(use-package anzu
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :init (global-anzu-mode))

;; jump to definition using ag or rg and applying heuristics
(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive nil
        dumb-jump-default-project "./"
        dumb-jump-prefer-searcher 'rg
        dumb-jump-selector (if (fboundp 'helm)
                               'helm
                             'completing-read))
  :config
  ;; add dumb-jump to the end of the list of backends for
  ;; `xref-find-definitions' so it is used as a fallback option
  ;; when no better finders are available
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t))

;; jump to visible text using char-based decision tree
(use-package avy
  :config
  ;; bind over `goto-line' since it can be invoked by entering numbers
  ;; for `avy-goto-line' input instead characters in the decision tree
  (global-set-key [remap goto-line] #'avy-goto-line)
  ;; jump to location in frame using "C-:"
  (global-set-key (kbd "C-:") #'avy-goto-char-timer))

;; display, select and jump to links in various major modes
(use-package ace-link
  :config
  ;; bind "o" to calling ace-link in compilation-mode, Custom-mode,
  ;; eww-mode, help-mode, Info-mode and woman-mode
  (ace-link-setup-default)
  ;; bind "M-O" (Meta-CapitalOh) to jump to link in Org mode
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "M-O") #'ace-link-org))
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "M-O") #'ace-link-org-agenda)))

;; load notdeft, make sure this comes after org-directory is set
(require 'notdeft-autoloads)
(setq notdeft-directories `(,(concat org-directory "journal/")
                            ,(concat org-directory "scratch/"))
      notdeft-extension "org"
      notdeft-secondary-extensions '("md" "txt")
      notdeft-directory (concat org-directory "scratch/")
      notdeft-xapian-program (concat (file-name-directory
                                      (locate-library "notdeft"))
                                     "xapian/notdeft-xapian"))

;; binding to access Notdeft
(global-set-key (kbd "C-c C-M-s") #'notdeft)

(setq imenu-auto-rescan t)

;; menu list of major definitions across several buffers
(use-package imenu-anywhere
  :defer t
  :after imenu
  :bind ("C-c C-M-;" . imenu-anywhere))

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

;; Visual (part 2)

;; hydra for visual settings
(defhydra my-hydra/visual (:color amaranth :hint nil
                           :pre (progn
                                  (require 'follow)
                                  (require 'hilit-chg)
                                  (require 'hl-line)
                                  (require 'display-line-numbers)
                                  (require 'face-remap)
                                  (require 'whitespace)))
  "
Visual (_q_: quit)
_b_ : blink-cursor [% 5`blink-cursor-mode]^^^^^   _F_ : follow       [% 5`follow-mode]^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   _f_ : font-lock    [% 5`font-lock-mode]
_H_ : hl-changes   [% 5`highlight-changes-mode]   _h_ : hl-line      [% 5`hl-line-mode]^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   _l_ : line-nums    [% 5`display-line-numbers-mode]
_p_ : show-paren   [% 5`show-paren-mode]^^^^^^^   _s_ : scroll-bar   [% 5(frame-parameter nil 'vertical-scroll-bars)]   _S_ : hscroll-bar  [% 5(frame-parameter nil 'horizontal-scroll-bars)]
_T_ : transient-mk [% 5`transient-mark-mode]^^^   _t_ : truncate-lns [% 5`truncate-lines]^^^^^^^^^^^^^^^^^^^^^^^^^^^^   _v_ : visual-line  [% 5`visual-line-mode]
_W_ : whitespace   [% 5`whitespace-mode]^^^^^^^   _w_ : trailing-ws  [% 5`show-trailing-whitespace]^^^^^^^^^^^^^^^^^^   _m_ : menu-bar     [% 5`menu-bar-mode]
_nr_ / _np_ / _nd_ / _nw_ : narrow to-region / to-page / to-defun / widen      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^[% 5(buffer-narrowed-p)]
_+_  / _-_  / _0_    ^  ^ : zoom   in        / out     / reset                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^[% 5(if text-scale-mode text-scale-mode-amount nil)]
"
  ("q" nil :exit t)
  ("b" blink-cursor-mode)
  ("F" follow-mode)
  ("f" font-lock-mode)
  ("H" highlight-changes-mode)
  ("h" hl-line-mode)
  ("l" display-line-numbers-mode)
  ("m" menu-bar-mode)
  ("p" show-paren-mode)
  ("s" toggle-scroll-bar)
  ("S" toggle-horizontal-scroll-bar)
  ("T" transient-mark-mode)
  ("t" toggle-truncate-lines)
  ("v" visual-line-mode)
  ("W" whitespace-mode)
  ("w" (lambda ()
         (interactive)
         (setq-local show-trailing-whitespace
                     (not show-trailing-whitespace))
         (message "show-trailing-whitespace: %s"
                  (if show-trailing-whitespace "yes" "no"))))
  ("nr" narrow-to-region)
  ("np" narrow-to-page)
  ("nd" narrow-to-defun)
  ("nw" widen)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("0" (text-scale-adjust 0))
  ("r" (lambda ()
         (interactive)
         ;; refocus doom-modeline just in case
         (when (and (boundp 'doom-modeline-mode)
                    doom-modeline-mode)
           (doom-modeline-focus))
         ;; redraw display
         (redraw-display))
   "redraw"))

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
(global-set-key (kbd "C-c C-M-v") 'my-hydra/visual/body)

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

;; display line numbers by default when editing code
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;; show point location column number in mode line
(setq column-number-mode t)

;; show matching parentheses with no delay
(setq show-paren-delay 0)
(show-paren-mode 1)

(use-package volatile-highlights
  :hook (after-init . volatile-highlights-mode))

;; add internal frame border
(add-to-list 'default-frame-alist
             `(internal-border-width . 12))
(defun my-default-frame-border-teardown ()
  "Removes internal-border-width entries from `default-frame-alist'."
  (setq default-frame-alist
        (assq-delete-all 'internal-border-width default-frame-alist)))
;; add teardown function to be run before closing Emacs, which needs
;; to run early when closing so add it to the end of `after-init-hook'
(add-hook 'after-init-hook
          (lambda ()
            (add-hook 'kill-emacs-hook #'my-default-frame-border-teardown))
          t)

;; add non-visisible bottom window dividers for mouse-based vertical resizing
(setq window-divider-default-bottom-width (if (eq system-type 'darwin)
                                              6
                                            3)
      window-divider-default-places 'bottom-only)
(let ((fg-color (face-attribute 'default :foreground))
      (bg-color (face-attribute 'default :background)))
  (set-face-attribute 'window-divider nil :foreground bg-color)
  (set-face-attribute 'window-divider-first-pixel nil :foreground bg-color)
  (set-face-attribute 'window-divider-last-pixel nil :foreground bg-color))
(window-divider-mode 1)

(require 'censor)

;; add `censor-mode' and `global-censor-mode' toggles to visual hydra
(eval
 `(defhydra+ my-hydra/visual
    ,(append my-hydra/visual/params '(:pre (require 'censor)))
    ,(concat my-hydra/visual/docstring
             "_X_  : global-censor-mode       [% 3`global-censor-mode]   _x_  : censor-mode              [% 3`censor-mode]
")
    ("X" global-censor-mode :exit nil)
    ("x" censor-mode :exit nil)))

;; add visual indentation guides
(use-package highlight-indent-guides
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-character ?\x2502)
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (highlight-indent-guides-mode 1))))

;; add `highlight-indent-guides-mode' toggle to visual hydra
(eval
 `(defhydra+ my-hydra/visual
    ,(append my-hydra/visual/params
             '(:pre (progn
                      (require 'symbol-overlay)
                      (require 'highlight-indent-guides))))
    ,(concat my-hydra/visual/docstring
             "_O_  : symbol-overlay-mode      [% 3`symbol-overlay-mode]   _i_  : highlight-indent-guides  [% 3`highlight-indent-guides-mode]
")
    ("O" symbol-overlay-mode :exit nil)
    ("i" highlight-indent-guides-mode :exit nil)))

;; pulse line after changing focused window using `ace-window'
(with-eval-after-load 'ace-window
  (advice-add 'ace-window :after #'my-pulse-line))

;; also pulse line when Emacs regains focus
(add-hook 'focus-in-hook #'my-pulse-line)

(require 'too-long-lines-mode)
(too-long-lines-mode 1)

;; add `too-long-lines-mode' toggle to visual hydra
(eval
 `(defhydra+ my-hydra/visual
    ,(append my-hydra/visual/params
             '(:pre (progn
                      (require 'too-long-lines-mode))))
    ,(concat my-hydra/visual/docstring
             "_L_  : too-long-lines-mode      [% 3`too-long-lines-mode]
")
    ("L" too-long-lines-mode :exit nil)))

;; Web

;; built-in Emacs text web browser
(use-package eww
  :ensure nil ;; built-in
  :commands (eww eww-follow-link)
  :init (setq eww-search-prefix "https://duckduckgo.com/lite?q=")
  ;; don't render images in HTML pages by default
  :config (setq-default shr-inhibit-images t))

;; add eww entry point to search hydra
(defhydra+ my-hydra/search nil
  ("w" eww "eww" :exit t))

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

;; increase network security settings
(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 1024)
(setq network-security-level 'high)
(setq nsm-save-host-names t)

;; HTTP requests privacy settings
(setq url-cookie-untrusted-urls '(".*")) ;; no cookies
(setq url-privacy-level 'paranoid) ;; more private HTTP requests
(url-setup-privacy-info) ;; apply `url-privacy-level'

;; read-it-later functionality in Org mode
;; custom package in the lisp subfolder of the user emacs directory
(require 'org-readitlater)
(define-key org-mode-map (kbd "C-c o") org-readitlater-keymap)

;; add capture template for org-readitlater
(setq org-readitlater-capture-file "readitlater/readitlater.org")
(push `("a" "Archive page to read-it-later list" entry
        (file+headline ,org-readitlater-capture-file "Unsorted")
        "* %?%:description\n:PROPERTIES:\n:URL: %:link\n:READITLATER_BACKEND_OPTIONS: --isolate --no-css --no-fonts --no-frames --no-images --no-js\n:ADDED: %U\n:END:\n%:initial\n")
      org-capture-templates)
;; auto-download page after capturing with org-readitlater template
(defun do-org-readitlater-dl-hook ()
  (when (equal (buffer-name)
               (concat "CAPTURE-"
                       (file-name-nondirectory org-readitlater-capture-file)))
    (org-readitlater-archive)))
(add-hook 'org-capture-before-finalize-hook #'do-org-readitlater-dl-hook)

;; Writing

;; provides word lookups from a dictionary server
;; `dictionary-server' can be set to "localhost" to use a local
;; dictionary server like dictd or GNU Dico that implements RFC 2229
(use-package dictionary
  :init (setq dictionary-server "dict.org"
              dictionary-default-dictionary "*"))

;; thesaurus functions using Synosaurus
(use-package synosaurus
  :init (setq synosaurus-choose-method 'default
              synosaurus-backend 'synosaurus-backend-wordnet))

;; grammar checking functions using LanguageTool
(use-package langtool
  :init (setq langtool-default-language "en-US"
              langtool-language-tool-jar (expand-file-name "~/jars/languagetool-commandline.jar")))

(use-package typo)

;; Other

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq calc-multiplication-has-precedence nil
      calc-ensure-consistent-units t
      calc-context-sensitive-enter t
      calc-undo-length 100
      calc-highlight-selections-with-faces nil)

;; Enable some functions disabled by default.
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "<f5>") #'revert-buffer)

(defun open-gnutls-stream--after-sleep-250ms (&rest args)
  "Workaround for race condition bug in `open-gnutls-stream'.

Adds 250ms to the opening of GnuTLS connections.

ARGS is a list of the original arguments passed to
`open-gnutls-stream' and is ignored.

See https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=930573#10
for more information."
  (sleep-for 0 250))

;; add advice to `open-gnutls-stream' to have it sleep for 250ms
;; as a workaround for GnuTLS race condition bug
(advice-add #'open-gnutls-stream :after #'open-gnutls-stream--after-sleep-250ms)

;; enable auth-source integration with pass
(when (executable-find "pass")
  (use-package auth-source-pass
    :demand t
    :config (auth-source-pass-enable)))

;; emacs integration with pass password-store
(when (executable-find "pass")
  (use-package password-store))

;; extend `tabulated-list-mode' with more functionality, adapted from
;; https://emacsnotes.wordpress.com/2019/04/16/how-i-shortlist-add-ons-for-my-emacs-introducing-tablist/
(use-package tablist
  :config
  ;; enable `tablist-minor-mode' automatically
  (add-hook 'tabulated-list-mode-hook 'tablist-minor-mode)
  ;; add tablist entries to the menu bar
  (easy-menu-define my-tablist-minor-mode-menu tablist-minor-mode-map
    "Menu for Tablist Minor Mode Map."
    '("Tablist"
      ("Mark"
       ["Mark Items Regexp" tablist-mark-items-regexp :help "(tablist-mark-items-regexp COLUMN-NAME REGEXP)\n\nMark entries matching REGEXP in column COLUMN-NAME."]
       ["Mark Items Numeric" tablist-mark-items-numeric :help "(tablist-mark-items-numeric BINOP COLUMN-NAME OPERAND)\n\nMark items fulfilling BINOP with arg OPERAND in column COLUMN-NAME.\n\nFirst the column's value is coerced to a number N.  Then the test\nproceeds as (BINOP N OPERAND)."]
       "--"
       ["Mark Forward" tablist-mark-forward :help "(tablist-mark-forward &optional ARG INTERACTIVE)\n\nMark ARG entries forward.\n\nARG is interpreted as a prefix-arg.  If interactive is non-nil,\nmaybe use the active region instead of ARG.\n\nSee `tablist-put-mark' for how entries are marked."]
       ["Unmark Forward" tablist-unmark-forward :help "(tablist-unmark-forward &optional ARG INTERACTIVE)\n\nUnmark ARG entries forward.\n\nSee `tablist-mark-forward'."]
       ["Unmark Backward" tablist-unmark-backward :help "(tablist-unmark-backward &optional ARG INTERACTIVE)\n\nUnmark ARG entries backward.\n\nSee `tablist-mark-forward'."]
       "--"
       ["Change Marks" tablist-change-marks :help "(tablist-change-marks OLD NEW)\n\nChange all OLD marks to NEW marks.\n\nOLD and NEW are both characters used to mark files."]
       "--"
       ["Toggle Marks" tablist-toggle-marks :help "(tablist-toggle-marks)\n\nUnmark all marked and mark all unmarked entries.\n\nSee `tablist-put-mark'."]
       ["Unmark All Marks" tablist-unmark-all-marks :help "(tablist-unmark-all-marks &optional MARKS INTERACTIVE)\n\nRemove alls marks in MARKS.\n\nMARKS should be a string of mark characters to match and defaults\nto all marks.  Interactively, remove all marks, unless a prefix\narg was given, in which case ask about which ones to remove.\nGive a message, if interactive is non-nil.\n\nReturns the number of unmarked marks."])
      "--"
      ("Filter"
       ["Push Regexp Filter" tablist-push-regexp-filter :help "(tablist-push-regexp-filter COLUMN-NAME REGEXP)\n\nAdd a new filter matching REGEXP in COLUMN-NAME.\n\nThe filter is and'ed with the current filter.  Use\n`tablist-toggle-first-filter-logic' to change this."]
       ["Push Equal Filter" tablist-push-equal-filter :help "(tablist-push-equal-filter COLUMN-NAME STRING)\n\nAdd a new filter whre string equals COLUMN-NAME's value.\n\nThe filter is and'ed with the current filter.  Use\n`tablist-toggle-first-filter-logic' to change this."]
       ["Push Numeric Filter" tablist-push-numeric-filter :help "(tablist-push-numeric-filter OP COLUMN-NAME 2ND-ARG)\n\nAdd a new filter matching a numeric predicate.\n\nThe filter is and'ed with the current filter.  Use\n`tablist-toggle-first-filter-logic' to change this."]
       ["Pop Filter" tablist-pop-filter :help "(tablist-pop-filter &optional N INTERACTIVE)\n\nRemove the first N filter components."]
       "--"
       ["Negate Filter" tablist-negate-filter :help "(tablist-negate-filter &optional INTERACTIVE)\n\nNegate the current filter."]
       ["Suspend Filter" tablist-suspend-filter :style toggle :selected tablist-filter-suspended :help "(tablist-suspend-filter &optional FLAG)\n\nTemporarily disable filtering according to FLAG.\n\nInteractively, this command toggles filtering."]
       ["Clear Filter" tablist-clear-filter :help "(tablist-clear-filter)"]
       ["Toggle First Filter Logic" tablist-toggle-first-filter-logic :help "(tablist-toggle-first-filter-logic)\n\nToggle between and/or for the first filter operand."]
       ["Display Filter" tablist-display-filter :style toggle :selected (assq 'tablist-display-filter-mode-line-tag mode-line-format) :help "(tablist-display-filter &optional FLAG)\n\nDisplay the current filter according to FLAG.\n\nIf FLAG has the value 'toggle, toggle it's visibility.\nIf FLAG has the 'state, then do nothing but return the current\nvisibility."]
       ["Edit Filter" tablist-edit-filter :help "(tablist-edit-filter)"]
       "--"
       ["Name Current Filter" tablist-name-current-filter :help "(tablist-name-current-filter NAME)"]
       ["Push Named Filter" tablist-push-named-filter :help "(tablist-push-named-filter NAME)\n\nAdd a named filter called NAME.\n\nNamed filter are saved in the variable `tablist-named-filter'."]
       ["Delete Named Filter" tablist-delete-named-filter :help "(tablist-delete-named-filter NAME &optional MODE)"]
       ["Deconstruct Named Filter" tablist-deconstruct-named-filter :help "(tablist-deconstruct-named-filter)"])
      "--"
      ("Column"
       ["Forward Column" tablist-forward-column :help "(tablist-forward-column N)\n\nMove n columns forward, while wrapping around."]
       ["Backward Column" tablist-backward-column :help "(tablist-backward-column N)\n\nMove n columns backward, while wrapping around."]
       "--"
       ["Move To Major Column" tablist-move-to-major-column :help "(tablist-move-to-major-column &optional FIRST-SKIP-INVISIBLE-P)\n\nMove to the first major column."]
       "--"
       ["Shrink Column" tablist-shrink-column :help "(tablist-shrink-column &optional COLUMN WIDTH)"]
       ["Enlarge Column" tablist-enlarge-column :help "(tablist-enlarge-column &optional COLUMN WIDTH)\n\nEnlarge column COLUMN by WIDTH.\n\nThis function is lazy and therfore pretty slow."])
      "--"
      ["Sort" tablist-sort :help "(tablist-sort &optional COLUMN)\n\nSort the tabulated-list by COLUMN.\n\nCOLUMN may be either a name or an index.  The default compare\nfunction is given by the `tabulated-list-format', which see.\n\nThis function saves the current sort column and the inverse\nsort-direction in the variable `tabulated-list-sort-key', which\nalso determines the default COLUMN and direction.\n\nThe main difference to `tabulated-list-sort' is, that this\nfunction sorts the buffer in-place and it ignores a nil sort\nentry in `tabulated-list-format' and sorts on the column\nanyway (why not ?)."]
      ["Do Kill Lines" tablist-do-kill-lines :help "(tablist-do-kill-lines &optional ARG INTERACTIVE)\n\nRemove ARG lines from the display."]
      "--"
      ["Export Csv" tablist-export-csv :help "(tablist-export-csv &optional SEPARATOR ALWAYS-QUOTE-P INVISIBLE-P OUT-BUFFER DISPLAY-P)\n\nExport a tabulated list to a CSV format.\n\nUse SEPARATOR (or ;) and quote if necessary (or always if\nALWAYS-QUOTE-P is non-nil).  Only consider non-filtered entries,\nunless invisible-p is non-nil.  Create a buffer for the output or\ninsert it after point in OUT-BUFFER.  Finally if DISPLAY-P is\nnon-nil, display this buffer.\n\nReturn the output buffer."]
      "--"
      ["Previous Line" tablist-previous-line :help "(tablist-previous-line &optional N)"]
      ["Next Line" tablist-next-line :help "(tablist-next-line &optional N)"]
      "--"
      ["Revert" tablist-revert :help "(tablist-revert)\n\nRevert the list with marks preserved, position kept."]
      ["Quit" tablist-quit :help "(tablist-quit)"])))

;; OS-specific / macOS

;; on macOS, use Option keys as Meta and file polling for auto-revert
(when (eq system-type 'darwin)
  (setq auto-revert-use-notify nil ;; macOS does not support file notifications
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

;; configure system open file by type command for macOS
(when (eq system-type 'darwin)
  (setq my-system-open-command "open"))

;; exclude Emacs source files from recentf history on macOS
(add-to-list 'recentf-exclude "^/Applications/Emacs.app/")

;; scale up LaTeX fragment preview images on macOS
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

;; Transient commands

(use-package transient
  :init
  ;; convenience function for specifying transient toggle descriptions
  (defun transient--make-description (desc is-enabled)
    "Return a string description for transient descriptions.
The returned string has format \"DESC [ ]\" if IS-ENABLED is nil
or \"DESC [X]\" if is-enabled is non-nil.

Examples:

  (transient--make-description symbol-overlay-mode \"highlight\")
  => \"highlight [x]\"

Example of use with transient suffix definitions in a
`transient-define-prefix' macro:

  ...
  (\"m\" (lambda () (transient--describe-toggle
                       \"highlight-at-pt\"
                       symbol-overlay-mode))
   symbol-overlay-mode)
  ...

  => \"m highlight-at-pt [ ]\"
"
    (concat desc " " (if is-enabled "[X]" "[ ]")))
  :config
  ;; bind "q" to quit transient popups by default, transient defns
  ;; binding "q" will attempt to bind "Q" or "M-q" instead
  (transient-bind-q-to-quit))

;; Transient commands / Global transients

;; add transient popup for bookmark commands, bind to "C-c C-M-j"
(transient-define-prefix transient/bookmarks ()
  "Various bookmark commands."
  ["Bookmarks"
   ["Navigate"
    ("j" "Jump" helm-bookmarks) ;; helm `bookmark-jump' replacement
    ("l" "List" list-bookmarks)
    ]
   ["Add/Remove"
    ("s" "Set" bookmark-set)
    ("d" "Delete" bookmark-delete)
    ("i" "Insert" bookmark-insert)
    ]
   ["File"
    ("L" "Load" bookmark-load)
    ("W" "Write" bookmark-write)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-j") #'transient/bookmarks)

(defun transient/buffer--tramp-cleanup-buffers ()
  "Clean up all TRAMP buffers and connections with confirm prompt."
  (interactive)
  (when (y-or-n-p "Cleanup all TRAMP buffers and connections? ")
    (tramp-cleanup-all-buffers)))

(defun transient/buffer--kill-other-buffers ()
  "Kill all file buffers except the current one."
  (interactive)
  (when (y-or-n-p "Kill all file buffers except the current one? ")
    (seq-each
     #'kill-buffer
     (delete (current-buffer)
             (seq-filter #'buffer-file-name (buffer-list))))))

(defun transient/buffer--indent-region-or-buffer ()
  "Indent a selected region, or the buffer otherwise."
  (interactive)
  (cond
   ((use-region-p) (indent-region (region-beginning) (region-end)))
   (t (indent-region (point-min) (point-max)))))

(defun transient/buffer--untabify-region-or-buffer ()
  "Convert tabs to spaces in a selected region, or the buffer otherwise."
  (interactive)
  (cond
   ((use-region-p) (untabify (region-beginning) (region-end)))
   (t (untabify (point-min) (point-max)))))

(defun transient/buffer--apply-all-hygiene-ops-region-or-buffer ()
  "Apply standard hygiene operations for selected region, or buffer otherwise.
The standard hygiene operations include removing trailing
whitespace, indenting and untabifying."
  (interactive)
  (progn
    (whitespace-cleanup)
    (transient/buffer--indent-region-or-buffer)
    (transient/buffer--untabify-region-or-buffer)))

(defun transient/buffer--open-containing-dir-externally (&optional path)
  "Opens the directory containing PATH or the buffer if unspecified externally."
  (interactive)
  (let* ((my-path (cl-some 'identity (list path
                                           (buffer-file-name)
                                           default-directory)))
         (my-full-path (expand-file-name my-path))
         (my-dir-path (file-name-directory my-full-path))
         (my-process-args (list "my-open-dir" nil
                                my-system-open-command my-dir-path)))
    (apply 'start-process my-process-args)))

;; add transient for buffer management commands, bind to "C-c C-M-b"
(transient-define-prefix transient/buffer ()
  "Buffer management commands."
  ["Buffer"
   ["Select"
    ("b" "Switch" switch-to-buffer)
    ("n" "Next" next-buffer :transient t)
    ("p" "Previous" previous-buffer :transient t)
    ("e" "Open external" transient/buffer--open-containing-dir-externally)
    ""
    "Hygiene"
    ("cr" "Whitespace report" whitespace-report)
    ("cw" "Whitespace cleanup" whitespace-cleanup)
    ("ci" "Indent" transient/buffer--indent-region-or-buffer)
    ("ct" "Untabify" transient/buffer--untabify-region-or-buffer)
    ("ca" "All hygiene ops" transient/buffer--apply-all-hygiene-ops-region-or-buffer)
    ]
   ["File operations"
    ("R" "Revert" revert-buffer)
    ("B" "Bury" bury-buffer)
    ("U" "Unbury" unbury-buffer)
    ("s" "Save" save-buffer)
    ("S" "Save all" save-some-buffers)
    ("k" "Kill" kill-this-buffer)
    ("K" "Kill matching" kill-matching-buffers)
    ("o" "Kill others" transient/buffer--kill-other-buffers)
    ("t" "TRAMP cleanup" transient/buffer--tramp-cleanup-buffers)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-b") #'transient/buffer)

;; add transient popup for conda commands, bind to "C-c C-M-S-v"
(with-eval-after-load 'conda
  (transient-define-prefix transient/conda ()
    "Conda commands."
    ["Conda environments"
     ("a" "Activate" conda-env-activate)
     ("b" "Activate for buffer" conda-env-activate-for-buffer)
     ("d" "Deactivate" conda-env-deactivate)
     ("l" "List" conda-env-list)
     ]
    )
  (global-set-key (kbd "C-c C-M-S-v") #'transient/conda))

(require 'debug)

(defun transient/debugger--list-variables ()
  "Print variables configured to invoke the debugger to the minibuffer."
  (interactive)
  (prin1 (debug--variable-list)))

;; add transient popup for conda commands, bind to "C-c C-M-S-v"
(transient-define-prefix transient/debugger ()
  "Emacs debugger settings."
  ["Emacs debugger settings"
   ["Toggle"
    ("1" (lambda ()
           (transient--make-description "Debug on error" debug-on-error))
     toggle-debug-on-error :transient t)
    ("2" (lambda ()
           (transient--make-description "Debug on quit" debug-on-quit))
     toggle-debug-on-quit :transient t)
    ]
   ["Invoke on function entry"
    ("fl" "List" debugger-list-functions)
    ("fa" "Add" debug-on-entry)
    ("fc" "Cancel" cancel-debug-on-entry)
    ]
   ["Invoke on variable change"
    ("vl" "List" transient/debugger--list-variables)
    ("va" "Add" debug-on-variable-change)
    ("vc" "Cancel" cancel-debug-on-variable-change)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-S-d") #'transient/debugger)

;; add transient popup for Ediff commands, bind to "C-c C-M-="
(transient-define-prefix transient/ediff ()
  "Various Ediff launch commands."
  ["Ediff"
   ["2 Way"
    ("b" "Buffers" ediff-buffers)
    ("f" "Files" ediff-files)
    ("d" "Directories" ediff-directories)
    ("c" "Buffer vs File" ediff-current-file)
    ("~" "File vs Backup" ediff-backup)
    ]
   ["3 Way"
    ("3b" "Buffers" ediff-buffers3)
    ("3f" "Files" ediff-files3)
    ("3d" "Directories" ediff-directories3)
    ]
   ["Patches"
    ("pb" "Buffer" ediff-patch-buffer)
    ("pf" "File" ediff-patch-file)
    ]
   ["Regions"
    ("rl" "Linewise" ediff-regions-linewise)
    ("rw" "Wordwise" ediff-regions-wordwise)
    ]
   ["Windows"
    ("wl" "Linewise" ediff-windows-linewise)
    ("ww" "Wordwise" ediff-windows-wordwise)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-=") #'transient/ediff)

(defun transient/frame--previous-frame ()
  "Select previous frame."
  (interactive)
  (other-frame -1))

;; add transient popup for frame commands, bind to "C-c C-M-f"
(transient-define-prefix transient/frame ()
  "Frame management commands."
  ["Frame"
   ["Select"
    ("n" "Next" other-frame)
    ("p" "Previous" transient/frame--previous-frame)
    ("s" "By name" select-frame-by-name)
    ]
   ["Layout"
    ("50" "Delete frame" delete-frame)
    ("51" "Delete other frames" delete-other-frames)
    ("52" "Create new frame" make-frame-command)
    ]
   ["Resize"
    ("M" "Toggle maximized" toggle-frame-maximized :transient t)
    ("f" "Toggle fullscreen" toggle-frame-fullscreen :transient t)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-f") #'transient/frame)

;; add transient popup for help commands, bind to "C-c C-M-S-h"
(transient-define-prefix transient/help ()
  "Various help commands."
  ["Help"
   ["Apropos"
    ("aa" "Symbol" apropos)
    ("ac" "Command" apropos-command)
    ("ad" "Documentation" apropos-documentation)
    ("al" "Library" apropos-library)
    ("av" "Variable" apropos-variable)
    ("aV" "Value" apropos-value)
    ]
   ["Describe"
    ("db" "Bindings" describe-bindings)
    ("df" "Function" describe-function)
    ("dk" "Key" describe-key)
    ("dm" "Mode" describe-mode)
    ("dp" "Package" describe-package)
    ("ds" "Syntax" describe-syntax)
    ("dv" "Variable" describe-variable)
    ]
   ["Info"
    ("ia" "Apropos" info-apropos)
    ("ib" "Browse" info)
    ("if" "File" info-lookup-file)
    ("ik" "Keywords" info-finder)
    ("is" "Symbol" info-lookup-symbol)
    ]
   ["Other"
    ("ve" "View messages" view-echo-area-messages)
    ("vl" "View lossage" view-lossage)
    ("w" "Where is" where-is)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-S-h") #'transient/help)

;; add transient for keyboard macros, bind to "C-c C-M-k"
(with-eval-after-load 'elmacro
  (transient-define-prefix transient/keyboard-macros ()
    "Keyboard macro commands."
    ["Keyboard Macros"
     ["Actions"
      ("(" "Start" kmacro-start-macro)              ;; also "C-x ("
      (")" "End/Call last" kmacro-end-or-call-macro) ;; also "C-x )"
      ("r" "Call last on region" apply-macro-to-region-lines)
      ]
     ["Ring"
      ("C-n" "Cycle next" kmacro-cycle-ring-next :transient t)
      ("C-p" "Cycle prev" kmacro-cycle-ring-previous :transient t)
      ("C-v" "View last" kmacro-view-macro :transient t)
      ("C-d" "Delete head" kmacro-delete-ring-head :transient t)
      ]
     ["Edit"
      ("e" "Named" edit-kbd-macro)
      ("RET" "Last" kmacro-edit-macro)
      ("l" "Lossage" kmacro-edit-lossage)
      ("SPC" "Step" kmacro-step-edit-macro)
      ]
     ]
    [
     ["Bind/Name"
      ("b" "Bind to key" kmacro-bind-to-key)
      ("n" "Name last" kmacro-name-last-macro)
      ("x" "To register" kmacro-to-register)
      ]
     [:description (lambda ()
                     (transient--make-description "Elmacro" elmacro-mode))
      ("Em" "Toggle mode" elmacro-mode :transient t)
      ("Ec" "Show last commands" elmacro-show-last-commands)
      ("El" "Show last macro" elmacro-show-last-macro)
      ("EC" "Clear history" elmacro-clear-command-history)
      ]
     ["Other"
      ("i" "Insert named" insert-kbd-macro)
      ]
     ]
    )
  (global-set-key (kbd "C-c C-M-k") #'transient/keyboard-macros))

;; add transient for neuron commands, bind to "C-c C-M-z"
(with-eval-after-load 'neuron-mode
  (transient-define-prefix transient/neuron ()
    "Neuron Zettelkasten commands."
    ["Neuron Zettelkasten"
     ["File"
      ("z" "New" neuron-new-zettel)
      ("e" "Edit" neuron-edit-zettel)
      ("j" "Daily" neuron-open-daily-notes)
      ("o" "Open" neuron-open-zettel)
      ("i" "Open index" neuron-open-index)
      ]
     ["Server"
      ("rw" "Watch files" neuron-rib-watch)
      ("rg" "Generate site" neuron-rib-generate)
      ("rs" "Start" neuron-rib-serve)
      ("ro" "Open" neuron-rib-open-zettel)
      ("ri" "Open z-index" neuron-rib-open-z-index)
      ("rk" "Kill" neuron-rib-kill)
      ]
     ["Other"
      ("t" "Query tags" neuron-query-tags)
      ("g" "Refresh cache" neuron-refresh)
      ("c" "Configuration" neuron-edit-zettelkasten-configuration)
      ]
     ]
    )
  (global-set-key (kbd "C-c C-M-z") #'transient/neuron))

;; add transient for password-store commands, bind to "C-c C-M-S-p"
(with-eval-after-load 'password-store
  (transient-define-prefix transient/password-store ()
    "Various password-store commands."
    ["Password store"
     ["Copy"
      ("c" "Password" password-store-copy)
      ("f" "Field" password-store-copy-field)
      ("u" "URL" password-store-url)
      ]
     ["Add/Remove/Modify"
      ("g" "Generate" password-store-generate)
      ("i" "Insert" password-store-insert)
      ("e" "Edit" password-store-edit)
      ("r" "Rename" password-store-rename)
      ("R" "Remove" password-store-remove)
      ]
     ["Other"
      ("C" "Clear" password-store-clear)
      ("I" "Init store" password-store-init)
      ("v" "Version" password-store-version)
      ]
     ]
    )
  (global-set-key (kbd "C-c C-M-S-p") #'transient/password-store))

;; add transient for Emacs profiler, bind to "C-c C-M-S-e"
(transient-define-prefix transient/profiler ()
  "Emacs profiler commands."
  [:description (lambda ()
                  (concat "Profiler | "
                          (transient--make-description
                           "CPU"
                           (profiler-running-p))
                          " "
                          (transient--make-description
                           "Memory"
                           (profiler-memory-running-p))))
   ("s" "Start/Reset" profiler-start :transient t)
   ("p" "Report" profiler-report)
   ("e" "Stop" profiler-stop :transient t)
   ]
  )
(global-set-key (kbd "C-c C-M-S-e") #'transient/profiler)

;; add transient popup for projectile, bind to "C-c C-M-p"
(with-eval-after-load 'projectile
  (transient-define-prefix transient/projectile ()
    "Projectile commands"
    [:description (lambda ()
                    (concat "Projectile ["
                            (projectile-project-name)
                            "]"))
     ["Project"
      ("C" "Configure" projectile-configure-project)
      ("c" "Compile" projectile-compile-project)
      ("u" "Run" projectile-run-project)
      ("P" "Test" projectile-test-project)
      ("z" "Cache file" projectile-cache-current-file)
      ("i" "Invalidate cache" projectile-invalidate-cache)
      ("x" "Run Eshell" projectile-run-eshell)
      ("!" "Run command" projectile-run-shell-command-in-root)
      ("&" "Run command async" projectile-run-async-shell-command-in-root)
      ""
      "Search"
      ("o" "Occur" projectile-multi-occur)
      ("s" "Grep" projectile-grep)
      ("r" "Replace" projectile-replace)
      ]
     ["Buffer"
      ("b" "Switchb" projectile-switch-to-buffer)
      ("<left>" "Previous" projectile-previous-project-buffer :transient t)
      ("<right>" "Next" projectile-next-project-buffer :transient t)
      ("I" "Ibuffer" projectile-ibuffer)
      ("S" "Save" projectile-save-project-buffers)
      ("k" "Kill" projectile-kill-buffers)
      ""
      "File"
      ("f" "Find file" projectile-find-file)
      ("F" "Find file (known prjs)" projectile-find-file-in-known-projects)
      ("g" "Find file (dwim)" projectile-find-file-dwim)
      ("t" "Toggle impl/test" projectile-toggle-between-implementation-and-test)
      ("e" "Recentf" projectile-recentf)
      ("E" "Edit dir-locals" projectile-edit-dir-locals)
      ]
     ["Dir"
      ("d" "Find dir" projectile-find-dir)
      ("D" "Dired" projectile-dired)
      ""
      "Tags"
      ("j" "Find tag" projectile-find-tag)
      ("R" "Regen tags" projectile-regenerate-tags)
      ""
      "Other"
      ("m" "Commander" projectile-commander)
      ("p" "Switch project" projectile-switch-project)
      ]
     ]
    )
  (define-key projectile-mode-map (kbd "C-c C-M-p")
    #'transient/projectile))

;; add transient popup for register commands, bind to "C-c C-M-S-v"
(transient-define-prefix transient/registers ()
  "Register commands."
  ["Registers"
   [("SPC" "Save point" point-to-register)
    ("w" "Save windows" window-configuration-to-register)
    ("f" "Save frames" frameset-to-register)
    ("j" "Jump" jump-to-register)
    ]
   [("s" "Copy region" copy-to-register)
    ("a" "Append region" append-to-register)
    ("p" "Prepend region" prepend-to-register)
    ("r" "Copy rectangle" copy-rectangle-to-register)
    ]
   [("i" "Insert" insert-register)
    ("l" "List" list-registers)
    ("v" "View" view-register)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-\"") #'transient/registers)

;; add transient popup for shell tools, bind to "C-c C-M-t"
(transient-define-prefix transient/shell ()
  "Various shell tools."
  ["Shell tools"
   ["Eshell"
    ("e" "New/Switch" my-eshell-with-name)
    ]
   ["Vterm"
    ("vv" "New" vterm)
    ("vo" "Other" vterm-other-window)
    ("vc" "Switch" vterm-switchb)
    ]
   ["Tmux"
    ("ts" "Send" tmux-send)
    ("tr" "Resend" tmux-resend)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-t") #'transient/shell)

;; add symbol-overlay transient popup and bind to "C-M-;"
(with-eval-after-load 'symbol-overlay
  (transient-define-prefix transient/symbol-overlay ()
    "Symbol overlay commands"
    ;; suffix actions don't exit the transient popup by default
    :transient-suffix 'transient--do-stay
    ["Symbol overlays"
     ["Navigation"
      ("n" "Jump next" symbol-overlay-jump-next)
      ("p" "Jump prev" symbol-overlay-jump-prev)
      ("f" "Switch fwd" symbol-overlay-switch-forward)
      ("b" "Switch bwd" symbol-overlay-switch-backward)
      ]
     ["Operations"
      ("i" "Put/Remove" symbol-overlay-put)
      ("t" "Toggle scope" symbol-overlay-toggle-in-scope)
      ("k" "Remove all" symbol-overlay-remove-all :transient nil)
      ("r" "Rename" symbol-overlay-rename :transient nil)
      ("q" "Query/Replace" symbol-overlay-query-replace :transient nil)
      ]
     ["Other"
      ("m" (lambda () (transient--make-description
                       "symbol-overlay-mode"
                       symbol-overlay-mode))
       symbol-overlay-mode)
      ("w" "Copy" symbol-overlay-save-symbol)
      ("s" "Search" symbol-overlay-isearch-literally :transient nil)
      ("d" "Defn" symbol-overlay-jump-to-definition :transient nil)
      ]
     ]
    )
  (global-set-key (kbd "C-M-;") 'transient/symbol-overlay))

(defun transient/system--display-current-datetime ()
  "Display the current time in the minibuffer."
  (interactive)
  (message (format-time-string "%Y-%b-%d %l:%M:%S%p %Z %A")))

(defun transient/system--display-emacs-pid ()
  "Display the process id of current Emacs process in the minibuffer."
  (interactive)
  (message (format "%d" (emacs-pid))))

(defun transient/system--display-emacs-build-config ()
  "Display the Emacs build configuration in the minibuffer."
  (interactive)
  (message (mapconcat 'identity
                      `("Emacs build configuration"
                        ,(format "  Build target:     %s"
                                 system-configuration)
                        ,(format "  Enabled features: %s"
                                 system-configuration-features))
                      "\n")))

;; add transient popup for system info, process and Emacs runtime
;; commands (including `server-mode'), bind to "C-c C-M-S-s"
(transient-define-prefix transient/system ()
  "System info, process and Emacs runtime/server-related commands."
  ;; suffix actions don't exit the transient popup by default
  :transient-suffix 'transient--do-stay
  ["System, process and Emacs runtime/server"
   [:description (lambda ()
                   (concat "Emacs ("
                           (transient--make-description
                            "server-mode"
                            server-mode)
                           ")"))
    ("eb" "Build config" transient/system--display-emacs-build-config)
    ("ei" "Init time" emacs-init-time)
    ("ep" "Emacs PID" transient/system--display-emacs-pid)
    ("es" "Toggle server-mode" server-mode)
    ("er" "Restart server" restart-emacs-server)
    ("eu" "Uptime" emacs-uptime)
    ("ev" "Version" emacs-version)
    ]
   ["System"
    ("sp" "Proced" proced :transient nil)
    ("st" "Datetime" transient/system--display-current-datetime)
    ("sw" "World time" display-time-world :transient nil)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-S-s") #'transient/system)

;; `next-multiframe-window' & `previous-multiframe-window' renamed to
;; `next-window-any-frame' & `previous-window-any-frame' in Emacs 27
(when (version< emacs-version "27")
  (defalias 'next-window-any-frame 'next-multiframe-window)
  (defalias 'previous-window-any-frame 'previous-multiframe-window))

(defun transient/window--transpose-windows (selector)
  "Call SELECTOR and transpose buffers between current and selected windows."
  (let ((from-win (selected-window))
        (from-buf (window-buffer)))
    (funcall selector)
    (set-window-buffer from-win (window-buffer))
    (set-window-buffer (selected-window) from-buf)))

(defun transient/window--transpose-window-up ()
  "Transpose buffers between current and the window above it."
  (interactive)
  (transient/window--transpose-windows 'windmove-up))
(defun transient/window--transpose-window-down ()
  "Transpose buffers between current and the window below it."
  (interactive)
  (transient/window--transpose-windows 'windmove-down))
(defun transient/window--transpose-window-left ()
  "Transpose buffers between current and the window to its left."
  (interactive)
  (transient/window--transpose-windows 'windmove-left))
(defun transient/window--transpose-window-right ()
  "Transpose buffers between current and the window to its right."
  (interactive)
  (transient/window--transpose-windows 'windmove-right))

(defun transient/window--rotate-window-buffers (rotations)
  "Rotate buffers in the windows of the current frame ROTATIONS times.
ROTATIONS can be negative, which rotates in the opposite direction."
  (interactive "P")
  (let* (;; windows that do not contain transient buffers
         (windows (seq-filter (lambda (w)
                                (not
                                 (string= (buffer-name
                                           (window-buffer w))
                                          transient--buffer-name)))
                              (window-list)))
         (num-windows (length windows)))
    (if (not (> num-windows 1))
        (message "Only one window in the frame. Nothing to rotate.")
      (let* (;; original window order properties
             (window-props (mapcar (lambda (w)
                                     `(:buffer ,(window-buffer w)
                                       :start ,(window-start w)
                                       :point ,(window-point w)))
                                   windows))
             ;; new window order after rotation
             (window-moves (mapcar
                            (lambda (k)
                              (elt windows (mod (+ k rotations)
                                                num-windows)))
                            (number-sequence 0 (1- num-windows))))
             ;; create alist for easier looping later
             (wins-props (cl-mapcar #'cons window-moves window-props)))
        ;; iteratively assign orig window props in new window order
        (dolist (w-p wins-props)
          (let ((win (car w-p))
                (prop (cdr w-p)))
            (set-window-buffer-start-and-point
             win
             (plist-get prop :buffer)
             (plist-get prop :start)
             (plist-get prop :point))))))))

(defun transient/window--rotate-buffers-forward ()
  "Rotate buffers in current frame's windows forward."
  (interactive)
  (transient/window--rotate-window-buffers 1))
(defun transient/window--rotate-buffers-backward ()
  "Rotate buffers in current frame's windows backward."
  (interactive)
  (transient/window--rotate-window-buffers -1))

;; add transient popup for window commands, bind to "C-c C-M-w"
(transient-define-prefix transient/window ()
  "Window management commands."
  :transient-suffix 'transient--do-stay
  ["Window"
   ["Navigate"
    ("n" "Next" next-window-any-frame)
    ("p" "Previous" previous-window-any-frame)
    ("o" "Other" other-window)
    ("<up>" "↑" windmove-up)
    ("<down>" "↓" windmove-down)
    ("<left>" "←" windmove-left)
    ("<right>" "→" windmove-right)
    ]
   ["Transpose"
    ("S-<up>" "↑" transient/window--transpose-window-up)
    ("S-<down>" "↓" transient/window--transpose-window-down)
    ("S-<left>" "←" transient/window--transpose-window-left)
    ("S-<right>" "→" transient/window--transpose-window-right)
    ("[" "Rotate bwd" transient/window--rotate-buffers-backward)
    ("]" "Rotate fwd" transient/window--rotate-buffers-forward)
    ]
   ["Layout"
    ("0" "Delete window" delete-window)
    ("1" "Delete other windows" delete-other-windows)
    ("2" "Split horiz" split-window-right)
    ("3" "Split vert" split-window-below)
    ("40" "Kill buffer and window" kill-buffer-and-window)
    ("u" "Winner undo" winner-undo)
    ("r" "Winner redo" winner-redo)
    ]
   ["Resize"
    ("-" "Shrink vert" shrink-window)
    ("^" "Enlarge vert" enlarge-window)
    ("{" "Shrink horiz" shrink-window-horizontally)
    ("}" "Enlarge horiz" enlarge-window-horizontally)
    ("M" "Maximize" maximize-window)
    ("m" "Minimize" minimize-window)
    ("+" "Balance" balance-windows)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-w") #'transient/window)

;; add transient popup for workspace commands, bind to "C-c C-M-e"
(transient-define-prefix transient/workspace ()
  "Various workspace commands."
  ["Workspace"
   ["Desktop"
    ("dc" "Clear" desktop-clear)
    ("ds" "Save" desktop-save)
    ("dr" "Read" desktop-read)
    ("dR" "Revert" desktop-revert)
    ("dd" "Change Dir" desktop-change-dir)
    ]
   ]
  )
(global-set-key (kbd "C-c C-M-e") #'transient/workspace)

;; add transient popup for writing commands, bind to "C-c C-M-S-w"
(with-eval-after-load 'dictionary
  (with-eval-after-load 'synosaurus
    (with-eval-after-load 'langtool
      (with-eval-after-load 'typo
        (transient-define-prefix transient/writing ()
          "Writing commands."
          ["Writing"
           ["Typography"
            ("y" (lambda ()
                    (interactive)
                    (transient--make-description "Typo mode"
                                                 typo-mode))
             typo-mode :transient t)
            ""
            "Spelling"
            ("sm" (lambda ()
                    (interactive)
                    (transient--make-description "Flyspell mode"
                                                 flyspell-mode))
             flyspell-mode :transient t)
            ("sb" "Check buffer" flyspell-buffer :transient t)
            ("sn" "Next error" flyspell-goto-next-error :transient t)
            ("sc" "Correct word" flyspell-auto-correct-word :transient t)
            ""
            "Dictionary"
            ("ds" "Search" dictionary-search)
            ("dm" "Match words" dictionary-match-words)
            ]
           ["Thesaurus"
            ("tm" (lambda ()
                    (interactive)
                    (transient--make-description "Synosaurus mode"
                                                 synosaurus-mode))
             synosaurus-mode :transient t)
            ("tl" "Lookup" synosaurus-lookup)
            ("tr" "Replace" synosaurus-choose-and-replace)
            ("ti" "Insert" synosaurus-choose-and-insert)
            ""
            "Grammar"
            ("gs" "Start check" langtool-check)
            ("gc" "Correct buffer" langtool-correct-buffer)
            ("ge" "End check" langtool-check-done)
            ("gl" "Switch language" langtool-switch-default-language
             :transient t)
            ]
           ]
          )
        (global-set-key (kbd "C-c C-M-S-w") #'transient/writing)))))

;; add transient popup for yasnippet commands, bind to "C-c C-M-<"
(with-eval-after-load 'yasnippet
  (with-eval-after-load 'auto-yasnippet
    (defun transient/yasnippet--aya-show-current ()
     "Show the current auto-snippet `aya-current' in the minibuffer."
     (interactive)
     (message "Current auto-yasnippet:\n%s" aya-current))
   (transient-define-prefix transient/yasnippet ()
     "YASnippet commands."
     ["YASnippet"
      ["Stored snippets"
       ("SPC" "Expand" yas-expand)
       ("s" "Insert" yas-insert-snippet)
       ("n" "New" yas-new-snippet)
       ("d" "Describe" yas-describe-tables)
       ("v" "Visit file" yas-visit-snippet-file)
       ]
      ["Auto snippets"
       ("w" "Create" aya-create)
       ("y" "Expand" aya-expand)
       ("?" "Show current" transient/yasnippet--aya-show-current)
       ]
      ]
     )
   (global-set-key (kbd "C-c C-M-<") #'transient/yasnippet)))

;; Transient commands / Major mode transients

;; major-mode specific transient for csv-mode
(with-eval-after-load 'csv-mode
  (transient-define-prefix transient/csv-mode ()
    "CSV mode commands."
    ["CSV"
     ["Sort"
      ("s" "Lexicographic" csv-sort-fields)
      ("n" "Numerically" csv-sort-numeric-fields)
      ("r" "Reverse" csv-reverse-region)
      ("d" "Toggle descending" csv-toggle-descending :transient t)
      ]
     ["Edit"
      ("t" "Transpose" csv-transpose)
      ("k" "Cut" csv-kill-fields)
      ("y" "Paste" csv-yank-fields)
      ("z" "Paste as new table" csv-yank-as-new-table)
      ]
     ["Visual"
      ("A" "Align visible" csv-align-visible-fields :transient t)
      ("a" "Align" csv-align-fields :transient t)
      ("u" "Unalign" csv-unalign-fields :transient t)
      ("h" "Toggle header" csv-header-line :transient t)
      ("v" "Toggle separator" csv-toggle-invisibility :transient t)
      ]
     ]
    )
  (define-key csv-mode-map (kbd "C-c C-M-m") #'transient/csv-mode))

;; major-mode specific transient for debugger-mode
(with-eval-after-load 'debug
  (transient-define-prefix transient/debugger-mode ()
    "Debugger mode commands."
    ["Emacs debugger"
     ["Stepping"
      ("d" "Step through" debugger-step-through)
      ("c" "Continue" debugger-continue)
      ("j" "Jump" debugger-jump)
      ("q" "Exit" top-level)
      ]
     ["Breakpoints"
      ("b" "Set" debugger-frame)
      ("u" "Unset" debugger-frame-clear)
      ]
     ["Evaluate"
      ("e" "Sexp" debugger-eval-expression)
      ("R" "Sexp and record" debugger-record-expression)
      ]
     ["Other"
      ("r" "Specify return value" debugger-return-value)
      ("l" "List debug functions" debugger-list-functions)
      ("v" "Toggle locals" debugger-toggle-locals)
      ("h" "Help" describe-mode)
      ]
     ]
    )
  (define-key debugger-mode-map (kbd "C-c C-M-m") #'transient/debugger-mode))

;; major-mode specific transient for ess-mode
(with-eval-after-load 'dired
  (with-eval-after-load 'dired-filter
    (defun transient/dired-mode--dired-kill-and-next-subdir ()
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
    (transient-define-prefix transient/dired-mode/filter ()
      "Dired-Filter commands."
      ;; have suffixes not exit the transient by default
      :transient-suffix 'transient--do-stay
      ["Dired → Filter"
       ["Filter by"
          ("n" "Name" dired-filter-by-name)
          ("r" "Regexp" dired-filter-by-regexp)
          ("." "Extension" dired-filter-by-extension)
          ("h" "Hidden" dired-filter-by-dot-files)
          ("o" "Omitted" dired-filter-by-omit)
          ("g" "Garbage" dired-filter-by-garbage)
          ("e" "Predicate" dired-filter-by-predicate)
          ("f" "File" dired-filter-by-file)
          ("d" "Directory" dired-filter-by-directory)
          ("m" "Mode" dired-filter-by-mode)
          ("s" "Symlink" dired-filter-by-symlink)
          ("x" "Executable" dired-filter-by-executable)
          ("ig" "Git-ignored" dired-filter-by-git-ignored)
        ]
       ["Operators"
        ("|" "OR" dired-filter-or)
        ("!" "NOT" dired-filter-negate)
        ("*" "Decompose" dired-filter-decompose)
        ("TAB" "Transpose" dired-filter-transpose)
        ("p" "Pop" dired-filter-pop)
        ("/" "Reset" dired-filter-pop-all)
        ]
       ["Save/Load"
        ("S" "Save" dired-filter-save-filters)
        ("L" "Load" dired-filter-load-saved-filters)
        ("A" "Add" dired-filter-add-saved-filters)
        ("D" "Delete" dired-filter-delete-saved-filters)
        ]
       ]
      )
    (transient-define-prefix transient/dired-mode ()
      "Dired commands."
      ["Dired"
       ["File"
        ("RET" "Open" dired-find-file)
        ("o" "Open other" dired-find-file-other-window)
        ("F" "Open marked" dired-do-find-marked-files)
        ("z" "Open external" dired--open-file-at-pt)
        ("v" "View file" dired-view-file)
        ("C" "Copy" dired-do-copy)
        ("R" "Rename" dired-do-rename)
        ("%R" "Rename by regexp" dired-do-rename-regexp)
        ("D" "Delete" dired-do-delete)
        ("x" "Delete marked" dired-do-flagged-delete)
        ("S" "Symlink" dired-do-symlink)
        ("Y" "Symlink to" dired-do-relsymlink)
        ("c" "Compress to" dired-do-compress-to)
        ("Z" "Compress" dired-do-compress)
        ("+" "Create dir" dired-create-directory)
        ("=" "Diff" dired-diff)
        ]
       ["Search"
        ("A" "Query" dired-do-find-regexp)
        ("Q" "Query-replace" dired-do-find-regexp-and-replace)
        ""
        "Attributes"
        ("G" "chgrp" dired-do-chgrp)
        ("M" "chmod" dired-do-chmod)
        ("O" "chown" dired-do-chown)
        ("T" "Touch" dired-do-touch)
        ""
        "Mark"
        ("m" "File at pt" dired-mark :transient t)
        ("E" "By extension" dired-mark-extension :transient t)
        ("t" "Toggle marks" dired-toggle-marks :transient t)
        ("u" "Unmark" dired-unmark :transient t)
        ("U" "Unmark all" dired-unmark-all-marks :transient t)
        ]
       ["Narrow"
        ("{" "Find by name" find-name-dired)
        ("}" "Find by query" find-grep-dired)
        ("/" "Filter" transient/dired-mode/filter)
        ""
        "View"
        ("(" "Toggle details" dired-hide-details-mode :transient t)
        (")" "Toggle omit" dired-omit-mode :transient t)
        ("i" "Insert subdir" dired-maybe-insert-subdir :transient t)
        ("K" "Kill subdir" transient/dired-mode--dired-kill-and-next-subdir :transient t)
        ("s" "Sort by date" dired-sort-toggle-or-edit :transient t)
        ""
        "Other"
        ("y" "Show file type" dired-show-file-type :transient t)
        ("g" "Refresh" revert-buffer :transient t)
        ("l" "Redisplay" dired-do-redisplay :transient t)
        ("C-o" "Display other" dired-display-file)
        ("h" "Help" describe-mode)
        ]
       ]
      )
    (define-key dired-mode-map (kbd "C-c C-M-m") #'transient/dired-mode)))

;; major-mode specific transient for edebug-mode
(with-eval-after-load 'edebug
  (transient-define-prefix transient/edebug-mode ()
    "Edebug commands."
    ["Edebug"
     ["Modes"
      ("SPC" "Step" edebug-step-mode)
      ("n" "Next" edebug-next-mode)
      ("g" "Go" edebug-go-mode)
      ("G" "Go (nonstop)" edebug-Go-nonstop-mode)
      ("t" "Trace" edebug-Trace-fast-mode)
      ("c" "Continue" edebug-continue-mode)
      ("C" "Continue (fast)" edebug-Continue-fast-mode)
      ""
      "Quitting/Stopping"
      ("q" "Top level" top-level)
      ("Q" "Top level (nonstop)" edebug-top-level-nonstop)
      ("a" "Abort recursive edit" abort-recursive-edit)
      ("S" "Stop" edebug-stop)
      ]
     ["Stepping"
      ("f" "Forward sexp" edebug-forward-sexp)
      ("h" "Continue to here" edebug-goto-here)
      ("I" "Instrument callee" edebug-instrument-callee)
      ("i" "Step in" edebug-step-in)
      ("o" "Step out" edebug-step-out)
      ""
      "Breakpoints"
      ("b" "Set" edebug-set-breakpoint)
      ("u" "Unset" edebug-unset-breakpoint)
      ("B" "Next" edebug-next-breakpoint)
      ("x" "Set (cond-at-pt)" edebug-set-conditional-breakpoint)
      ("X" "Set (global cond)" edebug-set-global-break-condition)
      ]
     ["Evaluation"
      ("r" "Previous result" edebug-previous-result)
      ("e" "Sexp" edebug-eval-expression)
      ("C-e" "Last sexp" edebug-eval-last-sexp)
      ("E" "Visit eval list" edebug-visit-eval-list)
      ""
      "Views"
      ("v" "Outside" edebug-view-outside)
      ("w" "Where" edebug-where)
      ("p" "Bounce point" edebug-bounce-point)
      ("W" "Toggle save windows" edebug-toggle-save-windows)
      ""
      "Other"
      ("?" "Help" edebug-help)
      ("d" "Backtrace" edebug-backtrace)
      ]
     ]
    )
  (define-key edebug-mode-map (kbd "C-c C-M-m") #'transient/edebug-mode))

;; major-mode specific transient for ess-mode
(with-eval-after-load 'ess-mode
  (defun transient/ess-mode--new-session ()
    "Opens a new ESS session depending the current `ess-dialect'."
    (interactive)
    (cond ((string= ess-dialect "R") (R))
          ((string= ess-dialect "julia") (julia))
          (t (message "Unsupported dialect"))))
  (transient-define-prefix transient/ess-mode ()
    "Emacs Speaks Statistics commands."
    ["Emacs Speaks Statistics"
     ["Session"
      ("N" "New" transient/ess-mode--new-session)
      ("R" "Request" ess-request-a-process)
      ("s" "Switch" ess-switch-to-ESS)
      ("q" "Quit" ess-quit)
      ]
     ["Eval"
      ("l" "Line" ess-eval-line)
      ("f" "Function" ess-eval-function)
      ("r" "Region" ess-eval-region)
      ("b" "Buffer" ess-eval-buffer)
      ]
     ["Workspace"
      ("D" "Change dir" ess-change-directory)
      ("d" "R dired" ess-rdired)
      ]
     ["Help"
      ("h" "Object" ess-display-help-on-object)
      ("A" "Apropos" ess-display-help-apropos)
      ("H" "Browser" ess-display-help-in-browser)
      ]
     ]
    )
  (define-key ess-mode-map (kbd "C-c C-M-m") #'transient/ess-mode))

;; major-mode specific transient for eww-mode
(with-eval-after-load 'eww
  (defun transient/eww-mode--toggle-images ()
    "Toggle displaying of images when rendering HTML."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload)
    (message "Images are now %s" (if shr-inhibit-images "off" "on")))
  (transient-define-prefix transient/eww-mode ()
    "Emacs Web Wowser mode commands."
    ["Emacs Web Wowser"
     ["Navigation"
      ("G" "Search" eww)
      ("o" "Open file" eww-open-file)
      ("l" "Back" eww-back-url)
      ("r" "Forward" eww-forward-url)
      ("g" "Reload" eww-reload)
      ("s" "Switchb" eww-switch-to-buffer)
      ("S" "Buffers" eww-list-buffers)
      ("H" "History" eww-list-histories)
      ]
     ["Bookmarks"
      ("b" "Add" eww-add-bookmark)
      ("B" "List" eww-list-bookmarks)
      ""
      "Toggle"
      ("F" "Fonts" eww-toggle-fonts :transient t)
      ("I" "Images" transient/eww-mode--toggle-images :transient t)
      ("M-C" "Colors" eww-toggle-colors :transient t)
      ("D" "Text direction" eww-toggle-paragraph-direction :transient t)
      ]
     ["Other"
      ("d" "Downlink link" eww-download)
      ("w" "Copy url" eww-copy-page-url)
      ("R" "View readable" eww-readable)
      ("v" "View source" eww-view-source)
      ("C" "Cookies" url-cookie-list)
      ("&" "Open external" eww-browse-with-external-browser)
      ]
     ]
    )
  (define-key eww-mode-map (kbd "C-c C-M-m") #'transient/eww-mode))

(transient-define-prefix transient/ibuffer-mode/mark ()
  "Ibuffer mode mark commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer → Mark"
   [("*" "Unmark all" ibuffer-unmark-all)
    ("M" "By mode" ibuffer-mark-by-mode)
    ("m" "Modified" ibuffer-mark-modified-buffers)
    ("u" "Unsaved" ibuffer-mark-unsaved-buffers)
    ]
   [("s" "Special" ibuffer-mark-special-buffers)
    ("r" "Read-only" ibuffer-mark-read-only-buffers)
    ("/" "Dired" ibuffer-mark-dired-buffers)
    ("e" "Disassociated" ibuffer-mark-dissociated-buffers)
    ]
   [("h" "Help" ibuffer-mark-help-buffers)
    ("z" "Compressed" ibuffer-mark-compressed-file-buffers)
    ]
   ]
  )

(transient-define-prefix transient/ibuffer-mode/action ()
  "Ibuffer mode action commands."
  ["Ibuffer → Action"
   ["Properties"
    ("R" "Rename uniquely" ibuffer-do-rename-uniquely)
    ("M" "Toggle modified" ibuffer-do-toggle-modified)
    ("T" "Toggle read-only" ibuffer-do-toggle-read-only)
    ""
    "Run"
    ("E" "Eval in buffers" ibuffer-do-eval)
    ("W" "View buffers and eval" ibuffer-do-view-and-eval)
    ("F" "Command on files" ibuffer-do-shell-command-file)
    ("X" "Pipe to command" ibuffer-do-shell-command-pipe)
    ("N" "Pipe to command and replace" ibuffer-do-shell-command-pipe-replace)
    ]
   ["Search"
    ("O" "Occur" ibuffer-do-occur)
    ("U" "Replace regexp" ibuffer-do-replace-regexp)
    ("Q" "Query/Replace" ibuffer-do-query-replace)
    ("I" "Query/Replace regexp" ibuffer-do-query-replace-regexp)
    ""
    "Other"
    ("A" "View" ibuffer-do-view)
    ("H" "View (other)" ibuffer-do-view-other-frame)
    ("V" "Revert" ibuffer-do-revert)
    ("P" "Print" ibuffer-do-print)
    ]
   ]
  )

(transient-define-prefix transient/ibuffer-mode/sort ()
  "Ibuffer mode sort commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer → Sort"
   [("a" "Alphabetic" ibuffer-do-sort-by-alphabetic)
    ("f" "Filename/Process" ibuffer-do-sort-by-filename/process)
    ("m" "Mode" ibuffer-do-sort-by-major-mode)
    ]
   [("s" "Size" ibuffer-do-sort-by-size)
    ("v" "Recency" ibuffer-do-sort-by-recency)
    ("i" "Invert" ibuffer-invert-sorting)
    ]
   ]
  )

(require 'ibuffer-vc)
(transient-define-prefix transient/ibuffer-mode/filter ()
  "Ibuffer mode filter commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer → Filter"
   ["Predicates"
    ("a" "Add saved" ibuffer-add-saved-filters)
    ("c" "By content" ibuffer-filter-by-content)
    ("e" "By predicate" ibuffer-filter-by-predicate)
    ("f" "By filename" ibuffer-filter-by-filename)
    ("m" "By mode" ibuffer-filter-by-used-mode)
    ("M" "By derived mode" ibuffer-filter-by-derived-mode)
    ("n" "By name" ibuffer-filter-by-name)
    (">" "By size gt" ibuffer-filter-by-size-gt)
    ("<" "By size lt" ibuffer-filter-by-size-lt)
    ]
   ["Operators"
    ("&" "AND" ibuffer-and-filter)
    ("|" "OR" ibuffer-or-filter)]
   ["Stack"
    ("p" "Pop" ibuffer-pop-filter)
    ("\\" "Clear" ibuffer-clear-filter-groups)
    ]
   ["Presets"
    ("V" "VC groups" ibuffer-vc-set-filter-groups-by-vc-root)
    ("R" "Saved" ibuffer-switch-to-saved-filter-groups)
    ("/" "Disable" ibuffer-filter-disable)
    ]
   ]
  )

(defun transient/ibuffer-mode--activate-dwim ()
  "Toggle filter group or visit buffer under point in `ibuffer-mode'."
  (interactive)
  (condition-case nil
             (ibuffer-toggle-filter-group)
           (error (ibuffer-visit-buffer))))

;; major-mode specific transient for ibuffer-mode
(transient-define-prefix transient/ibuffer-mode ()
  "Ibuffer mode commands."
  :transient-suffix 'transient--do-stay
  ["Ibuffer"
   ["Navigation"
    ("n" "Next line" ibuffer-forward-line)
    ("p" "Previous line" ibuffer-backward-line)
    ("RET" "Open" transient/ibuffer-mode--activate-dwim :transient nil)
    ("o" "Open (other)" ibuffer-visit-buffer-other-window :transient nil)
    ]
   ["Actions"
    ("m" "Mark" ibuffer-mark-forward)
    ("u" "Unmark" ibuffer-unmark-forward)
    ("*" "→ Mark" transient/ibuffer-mode/mark)
    ("S" "Save" ibuffer-do-save)
    ("D" "Delete" ibuffer-do-delete)
    ("a" "→ Action" transient/ibuffer-mode/action)
    ]
   ["View"
    ("`" "Switch format" ibuffer-switch-format)
    ("g" "Refresh" ibuffer-update)
    ("s" "→ Sort" transient/ibuffer-mode/sort)
    ("/" "→ Filter" transient/ibuffer-mode/filter)
    ]
   ]
  )
(define-key ibuffer-mode-map (kbd "C-c C-M-m") #'transient/ibuffer-mode)

;; major-mode specific transient for markdown-mode
(with-eval-after-load 'markdown-mode
  (with-eval-after-load 'markdown-toc
    (transient-define-prefix transient/markdown-mode ()
      "Markdown mode commands."
      :transient-suffix 'transient--do-stay
      ["Markdown mode"
       ["Outline"
        ("n" "Next" markdown-outline-next)
        ("p" "Previous" markdown-outline-previous)
        ("f" "Next (same level)" markdown-outline-next-same-level)
        ("b" "Previous (same level)" markdown-outline-previous-same-level)
        ("<left>" "Promote" markdown-promote)
        ("<right>" "Demote" markdown-demote)
        ("<up>" "Move up" markdown-move-up)
        ("<down>" "Move down" markdown-move-down)
        ]
       ["Shift region"
        ("<" "Outdent" markdown-outdent-region)
        (">" "Indent" markdown-indent-region)
        ""
        "User interface"
        ("E" "Toggle math" markdown-toggle-math)
        ("F" "Toggle code font" markdown-toggle-fontify-code-blocks-natively)
        ("I" "Toggle images" markdown-toggle-inline-images)
        ("L" "Toggle show URL" markdown-toggle-url-hiding)
        ("M" "Toggle show markup" markdown-toggle-markup-hiding)
        ]
       ["Table of contents"
        ("t" "Insert/Refresh" markdown-toc-generate-or-refresh-toc :transient nil)
        ("C-t" "Delete" markdown-toc-delete-toc)
        ""
        "Other"
        ("d" "Do" markdown-do :transient nil)
        ("o" "Follow" markdown-follow-thing-at-point :transient nil)
        ("'" "Edit code block" markdown-edit-code-block :transient nil)
        ]
       ]
      )
    (define-key gfm-mode-map (kbd "C-c C-M-m") #'transient/markdown-mode)
    (define-key markdown-mode-map (kbd "C-c C-M-m") #'transient/markdown-mode)))

;; major-mode specific transient for neuron-mode
(with-eval-after-load 'neuron-mode
  (transient-define-prefix transient/neuron-mode ()
    "Neuron Zettelkasten mode commands."
    ["Neuron mode"
     ["File"
      ("o" "Follow at point" neuron-follow-thing-at-point)
      ("u" "Edit uplink" neuron-edit-uplink)
      ("r" "Open current" neuron-open-current-zettel)
      ""
      "Tags"
      ("t" "Add" neuron-add-tag)
      ("T" "Add (multiple)" neuron-add-tags)
      ]
     ["Insert"
      ("l" "Zettel link" neuron-create-and-insert-zettel-link)
      ("L" "Zettel link from region" neuron-create-zettel-from-selected-title)
      ("s" "Static link" neuron-insert-static-link)
      ""
      "Other"
      ("c" "Toggle link conn type" neuron-toggle-connection-type :transient nil)
      ("m" "Markdown major mode transient" transient/markdown-mode)
      ]
     ]
    )
  (define-key neuron-mode-map (kbd "C-c C-M-m") #'transient/neuron-mode))

;; add transient for accessing Org entry points
(with-eval-after-load 'org
  (transient-define-prefix transient/org-launcher ()
    "Launcher for Org entry points."
    ["Org launcher"
     ("a" "Agenda" org-agenda)
     ("c" "Capture" org-capture)
     ("b" "Switchb" org-switchb)
     ("l" "Store link" org-store-link)
     ]
    )
  (global-set-key (kbd "C-c C-M-o") #'transient/org-launcher))

;; major-mode specific transient for python-mode
(with-eval-after-load 'python
  (with-eval-after-load 'live-py-mode
    ;; technically also depends on reformatter but more correctly
    ;; using it to define `python-black-format-buffer-or-region' and
    ;; `python-black-format-on-save-mode'
    (transient-define-prefix transient/python-mode ()
      "Python mode commands."
      ["Python"
       ["REPL"
        ("p" "Start" run-python)
        ("s" "Send string" python-shell-send-string)
        ("x" "Send defun" python-shell-send-defun)
        ("r" "Send region" python-shell-send-region)
        ("c" "Send buffer" python-shell-send-buffer)
        ("l" "Send file" python-shell-send-file)
        ("z" "Switch to" python-shell-switch-to-shell)
        ]
       ["Formatting"
        ("TAB" "Fill paragraph" python-fill-paragraph :transient t)
        ("<" "Indent left" python-indent-shift-left :transient t)
        (">" "Indent right" python-indent-shift-right :transient t)
        ("y" "Region or buffer" python-black-format-buffer-or-region :transient t)
        ("Y" "On save" python-black-format-on-save-mode)
        ]
       ["Other"
        ("j" "Imenu" imenu)
        ("v" "Check error" python-check)
        ("f" "Symbol quick help" python-eldoc-at-point)
        ("d" "Symbol describe" python-describe-at-point)
        ("D" "Python debugger" pdb)
        ("L" "Live coding mode" live-py-mode)
        ]
       ]
      )
    (define-key python-mode-map (kbd "C-c C-M-m") #'transient/python-mode)))

;; major-mode specific transient for racket-mode
(with-eval-after-load 'racket-mode
  (transient-define-prefix transient/racket-mode ()
    "Racket mode commands."
    ["Racket"
     ["Run"
      ("rr" "Buffer in REPL" racket-run)
      ("rm" "Module in REPL" racket-run-module-at-point)
      ("rR" "File in shell" racket-racket)
      ""
      "Profiling/Logging"
      ("rp" "Profiler" racket-profile)
      ("rl" "Logger" racket-logger)
      ""
      "Testing"
      ("tt" "Run tests in REPL" racket-test)
      ("tr" "Raco test" racket-raco-test)
      ]
     ["Refactoring"
      ("Rb" "Base requires" racket-base-requires)
      ("Rt" "Tidy requires" racket-tidy-requires)
      ("RT" "Trim requires" racket-trim-requires)
      ""
      "Help"
      ("." "Visit definition" racket-xp-visit-definition)
      ("C-." "Visit module" racket-visit-module)
      ("," "Unvisit" racket-unvisit)
      ("h" "Describe" racket-xp-describe)
      ("H" "Documentation" racket-xp-documentation)
      ]
     ["Editing"
      ("a" "Align" racket-align)
      ("u" "Unalign" racket-unalign)
      ""
      "Other"
      ("S" "Recompile racket-mode" racket-mode-start-faster)
      ("f" "Find collection" racket-find-collection)
      ("x" "Explain/Explore mode" racket-xp-mode)
      ]
     ]
    )
  (define-key racket-mode-map (kbd "C-c C-M-m") #'transient/racket-mode))

;; major-mode specific transient for restclient-mode
(with-eval-after-load 'restclient
  (defun transient/restclient-mode--toggle-narrow ()
    "Toggle narrowing to the current query in a `restclient-mode' buffer."
    (interactive)
    (if (buffer-narrowed-p)
        (widen)
      (restclient-narrow-to-current)))
  (defun transient/restclient-mode--format-json-region ()
    "Format a selected region containing JSON code."
    (interactive)
    (require 'json-mode nil t)
    (if (fboundp 'json-mode-pretty-print-dwim)
        (call-interactively 'json-mode-pretty-print-dwim)
      (message "Requires the `json-mode' package be installed.")))
  (transient-define-prefix transient/restclient-mode ()
    "REST client mode `restclient-mode' commands."
    :transient-suffix 'transient--do-stay
    ["REST client"
     ["Send query"
      ("v" "And stay" restclient-http-send-current-stay-in-window)
      ("c" "And switch" restclient-http-send-current :transient nil)
      ("r" "And switch (raw results)" restclient-http-send-current-raw
       :transient nil)
      ]
     ["Movement"
      ("n" "Next query" restclient-jump-next)
      ("p" "Previous query" restclient-jump-prev)
      ("." "Mark current" restclient-mark-current)
      ]
     ["Other"
      ("u" "Copy as CURL command" restclient-copy-curl-command)
      ("N" "Narrow/Widen buffer" transient/restclient-mode--toggle-narrow)
      ("f" "Format JSON region" transient/restclient-mode--format-json-region
       :transient nil)
      ]
     ]
    )
  (define-key restclient-mode-map (kbd "C-c C-M-m") #'transient/restclient-mode)
  )

;; major-mode specific transient for smerge-mode
(with-eval-after-load 'smerge-mode
  (transient-define-prefix transient/smerge-mode ()
    "smerge-mode commands."
    ;; have suffixes not exit the transient by default
    :transient-suffix 'transient--do-stay
    ["Smerge"
     ["Move"
      ("n" "Next" smerge-next)
      ("p" "Previous" smerge-prev)]
     [
      "Keep"
      ("b" "Base" smerge-keep-base)
      ("u" "Upper" smerge-keep-upper)
      ("l" "Lower" smerge-keep-lower)
      ("a" "All" smerge-keep-all)
      ("RET" "Current" smerge-keep-current)
      ]
     ["Diff"
      ("<" "Upper/Base" smerge-diff-base-upper)
      ("=" "Upper/Lower" smerge-diff-upper-lower)
      (">" "Base/Lower" smerge-diff-base-lower)
      ("R" "Refine" smerge-refine)
      ("E" "Ediff" smerge-ediff)
      ]
     [
      "Other"
      ("C" "Combine" smerge-combine-with-next)
      ("r" "Resolve" smerge-resolve)
      ("k" "Kill current" smerge-kill-current)
      ;; emulate Vim's "ZZ" command to save and close current file
      ("ZZ" "Save and bury buffer" my-save-and-bury-buffer
       :transient nil)
      ]
     ]
    )
  (define-key smerge-mode-map (kbd "C-c C-M-m") #'transient/smerge-mode))

;; major-mode specific transient for term-mode
(with-eval-after-load 'term
  (defun transient/term-mode--toggle-char-mode-line-mode ()
    "Toggle between `term-char-mode' and `term-line-mode' in `term-mode'."
    (interactive)
    (if (term-in-line-mode)
        (progn (term-char-mode) (message "line → char"))
      (progn (term-line-mode) (message "char → line"))))
  (transient-define-prefix transient/term-mode ()
    "Term mode commands."
    ["Term"
     ("m" "Toggle between `term-char-mode' and `term-line-mode'"
      transient/term-mode--toggle-char-mode-line-mode :transient t)
     ]
    )
  (define-key term-mode-map (kbd "C-c C-M-m") #'transient/term-mode)
  (define-key term-raw-map (kbd "C-c C-M-m") #'transient/term-mode))

;; major-mode specific transient for ztreedir-mode
(with-eval-after-load 'ztree-dir
  (transient-define-prefix transient/ztreedir-mode ()
    "Ztree directory commands."
    :transient-suffix 'transient--do-stay
    ["Ztree directory"
     ["Movement"
      ("n" "Next" ztree-next-line)
      ("p" "Previous" ztree-previous-line)
      ("DEL" "Up directory" ztree-move-up-in-tree)
      ]
     ["Actions"
      ("RET" "Hard action" ztree-perform-action :transient nil)
      ("SPC" "Soft action" ztree-perform-soft-action :transient nil)
      ("x" "Expand subtree" ztree-toggle-expand-subtree)
      ("d" "Dired at point" ztree-dir-open-dired-at-point :transient nil)
      ]
     ["View"
      ("g" "Refresh" ztree-refresh-buffer)
      ("H" "Toggle show filtered files" ztree-dir-toggle-show-filtered-files)
      (">" "Narrow tree" ztree-dir-narrow-to-dir)
      ("<" "Widen tree" ztree-dir-widen-to-parent)
      ]
     ]
    )
  (define-key ztreedir-mode-map (kbd "C-c C-M-m") #'transient/ztreedir-mode))

;; major-mode specific transient for ztreediff-mode
(with-eval-after-load 'ztree-diff
  (transient-define-prefix transient/ztreediff-mode ()
    "Ztree difference commands."
    :transient-suffix 'transient--do-stay
    ["Ztree difference"
     ["Movement"
      ("n" "Next" ztree-next-line)
      ("p" "Previous" ztree-previous-line)
      ("TAB" "Jump side" ztree-jump-side)
      ("DEL" "Up directory" ztree-move-up-in-tree)
      ]
     ["Actions"
      ("RET" "Hard action" ztree-perform-action :transient nil)
      ("SPC" "Soft action" ztree-perform-soft-action :transient nil)
      ("x" "Expand subtree" ztree-toggle-expand-subtree)
      ("d" "Diff files" ztree-diff-simple-diff-files :transient nil)
      ("v" "View file" ztree-diff-view-file)
      ("C" "Copy" ztree-diff-copy)
      ("D" "Delete" ztree-diff-delete-file)
      ]
     ["View"
      ("g" "Refresh" ztree-refresh-buffer)
      ("r" "Rescan (partial)" ztree-diff-partial-rescan)
      ("R" "Rescan (full)" ztree-diff-full-rescan)
      ("h" "Toggle show equal files" ztree-diff-toggle-show-equal-files)
      ("H" "Toggle show filtered files" ztree-diff-toggle-show-filtered-files)
      ]
     ]
    )
  (define-key ztreediff-mode-map (kbd "C-c C-M-m") #'transient/ztreediff-mode))

;; Transient commands / Minor mode transients

;; add transient for Flycheck, bind to "C-c C-M-!"
(with-eval-after-load 'flycheck
  (defun transient/flycheck-mode--toggle-error-list ()
    "Toggle the Flycheck error list, showing it in a side window."
    (interactive)
    (condition-case nil (quit-windows-on "*Flycheck errors*" t)
      (error (flycheck-list-errors))))
  (transient-define-prefix transient/flycheck-mode ()
    "Flycheck minor mode commands."
    :transient-suffix 'transient--do-stay
    ["Flycheck"
     ["Error"
      ("n" "Next" flycheck-next-error)
      ("p" "Previous" flycheck-previous-error)
      ("l" "List" transient/flycheck-mode--toggle-error-list)
      ("H" "Local help at point" display-local-help)
      ("h" "Display at point" flycheck-display-error-at-point)
      ("e" "Explain at point" flycheck-explain-error-at-point)
      ("C-w" "Copy all" flycheck-copy-errors-as-kill)
      ("C" "Clear all" flycheck-clear)
      ]
     ["Checker"
      ("s" "Select" flycheck-select-checker)
      ("?" "Describe" flycheck-describe-checker :transient nil)
      ("c" "Run" flycheck-buffer)
      ("C-c" "Run via `compile'" flycheck-compile)
      ""
      "Other"
      ("v" "Verify setup" flycheck-verify-setup :transient nil)
      ("i" "Online manual" flycheck-manual :transient nil)
      ]
     ]
    )
  (define-key flycheck-mode-map (kbd "C-c C-M-!") #'transient/flycheck-mode)
  )

(provide 'init)
;;; init.el ends here
