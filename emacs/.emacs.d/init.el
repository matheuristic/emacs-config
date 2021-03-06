;;; init.el --- Emacs init file -*- lexical-binding: t; -*-

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Generated: Mon Mar 15 10:29:32 2021

;;; Commentary:

;; Emacs initialization configuration file, symlink or copy to
;; ~/.emacs.d/init.el or $XDG_CONFIG_HOME/.emacs.d/init.el

;; In Emacs 27+, the sequence of initialization is
;; 1. early-init.el
;; 2. package.el
;; 3. init.el

;;; Code:

;; Customize file and local configuration

;; store Customize settings in a separate file, custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; load local init configuration and Customize settings on startup
(add-hook 'after-init-hook
          (lambda ()
            (require 'init-local nil t) ; don't raise errors
            (load custom-file 'noerror))
          10) ; load this after regular `after-init-hook' functions

;; Custom variables and utility functions / Custom variables

(defcustom my-system-open-command "xdg-open"
  "System command to open file/URL according to preferred app by filetype.
Usually \"xdg-open\" on Linux and \"open\" on Mac."
  :type 'string
  :group 'convenience)

(defcustom my-mode-lighter-abbrev-alist '(;; Minor modes
                                          (abbrev-mode . "")
                                          (auto-revert-mode . " ⤒")
                                          (buffer-face-mode . "")
                                          (eldoc-mode . "")
                                          (paredit-mode . " ⁽⁾")
                                          (too-long-lines-mode . " ⋯")
                                          (visual-line-mode . " ⇌")
                                          ;; Major modes
                                          ;; (lisp-interaction-mode . "λ")
                                          ;; (hi-lock-mode . "")
                                          ;; (python-mode . "Py")
                                          ;; (nxhtml-mode . "nx")
                                          (emacs-lisp-mode . "ELisp"))
  "Alist for `my-abbrev-mode-line' containing mode line lighter abbreviations.

Each entry should be a cons cell (a . b) where a is the minor or
major mode symbol and b is the string to be used as the
abbreviated mode lighter in the mode line (can be an empty string).

Abbreviations for minor modes should typically be prefixed by a
space to make them easier to distinguish, but there is no need to
do so for major mode abbreviations. Use an empty string as an
abbreviation to not show a lighter for a mode.")

(defun my-mode-line-lighter-abbrev ()
  "Abbreviate mode line major and minor mode lighters.

Configure `my-mode-lighter-abbrev-alist' to determine which mode
lighters are abbreviated and what they are abbreviated to."
  (interactive)
  (dolist (abbr my-mode-lighter-abbrev-alist)
    (let* ((mode (car abbr))
           (mode-str (cdr abbr))
           (is-minor-mode (member mode minor-mode-list))
           (mode-str-old (cdr (assq mode minor-mode-alist))))
      (if is-minor-mode
          (let ((minor-mode-alist-entry (assq mode minor-mode-alist)))
            ;; if entry for minor mode exists in `minor-mode-alist'
            ;; modify its value, otherwise append a new entry
            (if minor-mode-alist-entry
                (setcdr minor-mode-alist-entry (list mode-str))
              (add-to-list 'minor-mode-alist
                           (cons mode (list mode-str))
                           t)))
        (when (eq mode major-mode)
          (setq mode-name mode-str))))))

;; rerun on major mode changes
(add-hook 'after-change-major-mode-hook #'my-mode-line-lighter-abbrev)

;; Custom variables and utility functions / Utility functions

(defun my-after-jump-context-actions (&rest _)
  "Useful context actions to perform after jumping to a new location.
This is meant for use with `advice-add' with the :after
combinator.

One useful context action example is to run `org-show-context'
after jumping to an Org buffer location to ensure the region
around the new point location is visible."
  (cond ((eq major-mode 'org-mode) (org-show-context))))

(defun my-persist-variables-to-file (varlist filename)
  "Persist variables in VARLIST to a file FILENAME."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dolist (var varlist)
        (print (list 'setq var (list 'quote (symbol-value var)))
               buf))
      (save-buffer)
      (kill-buffer))))

(defun my-frame-monitor-dpi (&optional frame)
  "Get the pixel density in dots per inch (DPI) for the screen containing FRAME.
If FRAME is nil, use the current frame.

DPI (or really points per inch, PPI) is computed with the formula
  PPI = diag_in_pixels / diag_in_inches
where
  diag_in_pixels = sqrt(width_in_pixels**2 + height_in_pixels**2)
  diag_in_inches = sqrt(width_in_mm**2 + height_in_mm**2) / inch_in_mm
  inch_in_mm = 25.4

See https://en.wikipedia.org/wiki/Pixel_density for more details."
  (let* ((attrs (frame-monitor-attributes frame))
         (geom (assoc 'geometry attrs))
         ;; diagonal in pixels
         (width-pixels (nth 3 geom))
         (height-pixels (nth 4 geom))
         (diag-pixels (sqrt (+ (* width-pixels width-pixels)
                               (* height-pixels height-pixels))))
         ;; diagonal in inches
         (scrn (assoc 'mm-size attrs))
         (width-mm (nth 1 scrn))
         (height-mm (nth 2 scrn))
         (diag-mm (sqrt (+ (* width-mm width-mm)
                           (* height-mm height-mm))))
         (diag-inches (/ diag-mm 25.4))) ; 25.4mm per inch
    ;; dpi (or ppi) = diagonal in pixels / diagonal in inches
    (/ diag-pixels diag-inches)))

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
            (pulse-delay .25))
        (pulse-momentary-highlight-region start end nil))))

(defun my-save-and-bury-buffer (&rest _)
  "Save and bury the current buffer."
  (save-buffer)
  (bury-buffer))

;; hacky workaround to install ELPA/MELPA version of a package
;; adapated from https://github.com/jwiegley/use-package/issues/319
(defun my-install-elpa-package (pkg-symb)
  "Install the ELPA-compatible repository version of package PKG-SYMB.
Useful for working around `use-package' behavior of not
installing the repository version of a package when a built-in
version is present (even if pinned to a specific repository)."
  (let ((pkg-pattern (concat package-user-dir
                             "/" (symbol-name pkg-symb) "-[0-9]*")))
    (unless (file-expand-wildcards pkg-pattern)
      (package-install (elt (cdr (assoc pkg-symb
                                        package-archive-contents))
                            0)))))

;; Package management

;; set ELPA-compatible package repositories and their priorities
(setq package-archives '(("ELPA"   . "https://elpa.gnu.org/packages/")
                         ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities '(("ELPA"  . 1)
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

;; gather use-package stats, "M-x use-package-report" to see report
(setq use-package-compute-statistics t)

;; convenience function to reinstall and reload an Emacs package
(require 'cl-macs)
(require 'seq)
(defun my-package-reinstall (pkg)
  "Prompts for an installed package PKG and reinstalls it.

All loaded features that correspond to Elisp filenames in the
package install directory (but not its subdirectories) are
unloaded, the package reinstalled, and the previously unloaded
features are reloaded."
  (interactive (list (intern (completing-read
                              "Reinstall package: "
                              (mapcar #'car package-alist)))))
  (let* ((pkg-desc (car (alist-get pkg package-alist)))
         (pkg-dir (file-name-as-directory
                   (cl-struct-slot-value 'package-desc 'dir pkg-desc)))
         (pkg-files (directory-files pkg-dir nil "\\.el$"))
         (pkg-features (mapcar
                        (lambda (fname)
                          (intern (file-name-sans-extension fname)))
                        pkg-files))
         (reload-features (seq-filter 'featurep pkg-features)))
    (dolist (feat reload-features)
      (ignore-errors ; handle when pkg is a dependency of another package
        (unload-feature feat t)))
    (package-reinstall pkg)
    (dolist (feat reload-features)
      (require feat))))

;; Environment variables

;; copy environment variables from shell
(use-package exec-path-from-shell
  :config
  (defvar my-exec-path-envs-cache-file
    (expand-file-name ".my-exec-path-envs.el" user-emacs-directory)
    "Cache file for `exec-path-from-shell-variables' env var values.")
  (defvar my-exec-path-envs nil
    "List of (NAME . VALUE) pairs corresponding to environment variable values.
Variables should match `exec-path-from-shell-variables'.")
  (defun my-exec-path-from-shell-initialize (reload)
    "Memoized version of `exec-path-from-shell-initialized' using a cache file.

The cache is assumed to stored in the file
`my-exec-path-envs-cache-file'. If the cache file
does not exist, generate one. If RELOAD is non-nil (say using
\"C-u M-x my-exec-path-from-shell-initialize\"), regenerate the
cache before processing."
    (interactive "P")
    (when (and (not reload)
               (file-exists-p my-exec-path-envs-cache-file))
      (message "Using existing exec-path-from-shell envs cache file")
      (load-file my-exec-path-envs-cache-file))
    (when (or reload
              (not my-exec-path-envs))
      (setq my-exec-path-envs (exec-path-from-shell-getenvs
                               exec-path-from-shell-variables))
      (message "Persisting exec-path-from-shell envs cache file")
      (my-persist-variables-to-file '(my-exec-path-envs)
                                    my-exec-path-envs-cache-file))
    ;; copied from exec-path-from-shell-copy-envs
    (mapc (lambda (pair)
            (exec-path-from-shell-setenv (car pair) (cdr pair)))
          my-exec-path-envs)
    (message "Initialized exec-path-from-shell environment variables"))
  (my-exec-path-from-shell-initialize nil))

;; Backend and frontend frameworks for building user interfaces

;; enable flex completion, requires Emacs 27+
(with-eval-after-load 'minibuffer
  (add-to-list 'completion-styles 'flex t))

;; use Icomplete as the completion backend
;; enable `fido-mode' to emulate ido behavior where possible
(fido-mode)

;; text completion framework
(use-package company
  :defer t
  :init
  (with-eval-after-load 'prog-mode
    (add-hook 'prog-mode-hook 'company-mode))
  (setq company-dabbrev-downcase nil
        company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t ;; use M-<num> to directly choose completion
        company-tooltip-align-annotations t)
  :config
  (add-to-list 'my-mode-lighter-abbrev-alist '(company-mode . " ℂ")))

;; edit regions in separate buffers, used by other packages like markdown-mode
(use-package edit-indirect)

;; notifications backend
(use-package alert
  :config (setq alert-default-style
                (cond
                 ;; use AppleScript for macOS notifications
                 ((eq system-type 'darwin) 'osx-notifier)
                 ;; use libnotify for Linux notifications if avilable
                 ((and (eq system-type 'gnu/linux)
                       (executable-find "notify-send"))
                  'libnotify)
                 ;; otherwise print message to minibuffer
                 (t 'message))))

;; Visual (part 1)

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

;; alternative interface for M-x
(use-package amx
  :bind ("M-X" . amx-major-mode-commands)
  :init (amx-mode))

;; recently opened files
(setq recentf-max-menu-items 10
      recentf-max-saved-items 100
      recentf-auto-cleanup 'mode) ;; clean up recent list when turning on mode
(recentf-mode 1)
;; exclude compressed files
(add-to-list 'recentf-exclude ".gz")
(add-to-list 'recentf-exclude ".xz")
(add-to-list 'recentf-exclude ".zip")
;; exclude source code files in installed packages from ELPA-compatible repos
(add-to-list 'recentf-exclude
             (concat "^" (expand-file-name "elpa/" user-emacs-directory)))
;; exclude files opened with SSH so TRAMP is not spammed with stat calls
;; exclude files opened as the superuser with su or sudo
(add-to-list 'recentf-exclude "^/\\(?:scp\\|ssh\\|su\\|sudo\\)?:")
;; exclude files from /var/folder as these are temp files
(add-to-list 'recentf-exclude "^/var/folders")
;; exclude files in `org-agenda-files'
;; these files are quickly accessible from their respective tooling
(add-hook 'after-init-hook
          (lambda ()
            (dolist (file-list (list org-agenda-files))
              (dolist (exclude-file file-list)
                (add-to-list 'recentf-exclude
                             (concat "^" exclude-file))))))
;; exclude files in conda environments
(add-hook 'after-init-hook
          (lambda ()
            (with-eval-after-load 'conda
              (add-to-list 'recentf-exclude
                           (concat "^" conda-anaconda-home)))))

;; binding for recentf
(global-set-key (kbd "C-c f") #'recentf-open-files)

;; select file to open from `recentf-list' using `completing-read'
(defun my-recentf-find-file ()
  "Use `completing-read' to find a recent file."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list)))

;; binding for `my-recentf-open-files' when in recentf dialog buffers
(define-key recentf-dialog-mode-map (kbd "f") #'my-recentf-find-file)

(save-place-mode 1)

;; save minibuffer and other history across sessions
;; don't persist kill-ring if in the habit of copy-pasting passwords
(setq history-delete-duplicates t
      history-length 100
      ;; if `desktop-save-mode' is enabled, it saves `register-alist'
      ;; and `search-ring' by default so it is unnecessary to add
      ;; those to `savehist-additional-variables'
      savehist-additional-variables '(Info-history-list
                                      ;; kill-ring
                                      kmacro-ring
                                      regexp-search-ring
                                      ;; register-alist
                                      last-kbd-macro
                                      ;; search-ring
                                      shell-command-history))

;; enable save history mode
(savehist-mode 1)

;; Buffers, windows, frames, workspaces / Buffer management

;; protect these buffers, locking them to make them unkillable
(dolist (buf '("*scratch*" "*Messages*"))
  (with-current-buffer buf
    (emacs-lock-mode 'kill)))

;; advanced buffer management with Ibuffer
(setq ibuffer-expert t ; skip extraneous confirm messages
      ibuffer-show-empty-filter-groups nil)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; configure Ibuffer filter groups
(with-eval-after-load 'ibuffer
  (defun my-ibuffer-org-agenda-files-filter ()
    "Ibuffer filter for checking if current buffer is an Org agenda file.

Specifically, the current buffer is checked to see if it is in
`org-agenda-files', is the agenda inbox file
`my-org-agenda-inbox', or is the someday inbox file
`my-org-someday-inbox'."
    (let* ((bufname (buffer-file-name))
           (fname (and bufname (file-truename bufname))) ; filename if a file buffer, nil otherwise
           (agenda-fnames (mapcar #'file-truename (append (org-agenda-files) ; agenda and inbox filenames
                                                          (list my-org-agenda-inbox
                                                                my-org-someday-inbox)))))
      (and fname
           (member fname agenda-fnames))))
  (setq ibuffer-saved-filter-groups
        ;; files are grouped by the first matching filter group in the list
        '(("default"
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
           ("Calendar" (or (name . "^\\*?[Cc]alendar.*$")
                           (name . "^diary$")))
           ("DocView" (mode . doc-view-mode))
           ("Images" (mode . image-mode))
           ("Web" (or (mode . eww-mode)
                      (mode . eww-bookmark-mode)))
           ("Shell" (or (mode . eshell-mode)
                        (mode . shell-mode)
                        (mode . term-mode)))
           ("Data" (or (mode . csv-mode)
                       (mode . json-mode)
                       (mode . nxml-mode)))
           ("Analytics" (or (mode . ess-r-mode)
                            (mode . inferior-ess-r-mode)))
           ("Programming" (derived-mode . prog-mode))
           ("Agenda" (or (mode . org-agenda-mode)
                         (predicate . (my-ibuffer-org-agenda-files-filter))))
           ("Org" (derived-mode . org-mode))
           ("Text" (derived-mode . text-mode))
           ("Fundamental" (mode . fundamental-mode))
           ("Dired" (mode . dired-mode))
           ("Magit" (derived-mode . magit-mode))
           ("Help" (or (derived-mode . apropos-mode)
                       (derived-mode . help-mode)
                       (derived-mode . Info-mode))))))
  (defun my-ibuffer-filter-groups-setup ()
    "Custom configuration to load when a new Ibuffer buffer gets created."
    ;; use "default" saved filter groups list by default
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook #'my-ibuffer-filter-groups-setup))

;; build VC project ibuffer filter groups
(use-package ibuffer-vc
  :after ibuffer
  :bind (:map ibuffer-mode-map
         ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

;; visual buffer switching using a grid of windows
(use-package buffer-expose
  :init
  (setq buffer-expose-show-current-buffer t)
  ;; set auto initialization with ace-window if it is loaded
  (with-eval-after-load 'ace-window
    (setq buffer-expose-auto-init-aw t)))

(define-minor-mode revert-without-query-mode
  "Minor mode for adding/removing current file to/from `revert-without-query'.

Enabling the minor mode adds the file to `revert-without-query'.

Disabling the minor mode removes the file from `revert-without-query'.

This minor mode has no effect when the buffer is not visiting a file."
  :init-value nil
  :lighter " 🅠"
  :keymap nil
  ;; match filename from `find-file-noselect'
  (let ((fname (abbreviate-file-name (expand-file-name
                                      (buffer-file-name)))))
    (if buffer-file-name
        (if (symbol-value revert-without-query-mode)
            (progn
              (setq revert-without-query (add-to-list 'revert-without-query fname))
              (message "Buffer revert without query ON."))
          (setq revert-without-query (remove fname revert-without-query))
          (message "Buffer revert without query OFF."))
      (message "Current buffer is NOT visiting a file."))))

;; Buffers, windows, frames, workspaces / Window management

;; traverse window config changes, C-c left/right to undo/redo
;; uncomment to not bind C-c left/right keys by default
;; (setq winner-dont-bind-my-keys t)
;; enable winner-mode at end of initialization
(add-hook 'after-init-hook #'winner-mode)

;; more convenient bindings for `other-window' and `other-frame'
(global-set-key (kbd "M-o") #'other-window)

(defun my-rotate-window-buffers (rotations)
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

(defun my-rotate-buffers-forward ()
  "Rotate buffers in current frame's windows forward."
  (interactive)
  (my-rotate-window-buffers 1))
(defun my-rotate-buffers-backward ()
  "Rotate buffers in current frame's windows backward."
  (interactive)
  (my-rotate-window-buffers -1))

;; bind "C-x 4 [" and "C-x 4 ]" to rotation of window buffers
(global-set-key (kbd "C-x 4 [") #'my-rotate-buffers-backward)
(global-set-key (kbd "C-x 4 ]") #'my-rotate-buffers-forward)

;; automatically focus on help windows when they are opened
(setq help-window-select t)

;; Buffers, windows, frames, workspaces / Frame management

;; resize frames by pixel instead of by character
(setq frame-resize-pixelwise t)

(use-package transpose-frame
  :bind (("C-x 5 [" . rotate-frame-anticlockwise)
         ("C-x 5 ]" . rotate-frame-clockwise)))

;; more convenient bindings for `other-frame'
(global-set-key (kbd "M-O") #'other-frame)

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
;; - re-use existing frames
(setq desktop-auto-save-timeout nil
      desktop-restore-in-current-display nil
      desktop-restore-reuses-frames t
      desktop-files-not-to-save (concat "\\("
                                        (mapconcat
                                         'identity
                                         '("\\`/[^/:]*:"
                                           "(ftp)\\'"
                                           "\\.log"
                                           "\\.gz")
                                         "\\|"
                                         )
                                        "\\)"))
(add-hook 'after-init-hook
          (lambda ()
            (desktop-save-mode 1)
            (desktop-read))
          50) ; load after all other `after-init-hook' functions

;; multiple window configs in a frame, most commands have a "C-x t"
;; prefix, and C-TAB and S-C-TAB to switch to next and previous tabs
;; respectively, requires Emacs 27+
(setq tab-bar-show t ; always show tab bar, set to 1 to hide bar when there's only 1 tab
      tab-bar-close-last-tab-choice 'delete-frame ; deleting the last frame deletes the frame
      tab-bar-select-tab-modifiers '(super) ; super-<num> goes to tab<num>
      tab-bar-tab-hints t ; show tab numbers
      tab-bar-tab-name-truncated-max 20 ; truncate name if exceed 20 chars
      tab-bar-new-button-show nil ; don't show new button
      tab-bar-close-button-show nil) ; don't show close button
(tab-bar-mode 1) ; enable mode

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

;; extend pcomplete with fish shell
(when (executable-find "fish")
  (use-package fish-completion
    :after eshell
    :config (add-hook 'eshell-mode-hook #'fish-completion-mode)))

(use-package eshell-z
  :after eshell)

(use-package eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

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
         ("C-c d z" . ztree-diff))
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
    :bind ("C-c x d" . docker)))

;; Dired

(require 'dired-x) ; extra features
(require 'dired-aux) ; even more extra features
(setq dired-auto-revert-buffer 'dired-directory-changed-p ; when revisiting Dired buffers, refresh if dir has changed on disk
      dired-dwim-target t ; use neighboring dired buffer as default target dir
      dired-listing-switches "-alhvFG" ; more readable file listings
      dired-omit-files (concat dired-omit-files "\\|^\\..+$") ; omit dot files in dired-omit-mode
      dired-recursive-copies 'always ; always copy recursively
      dired-recursive-deletes 'always) ; always delete recursively
;; uncomment below to automatically update Dired buffers every
;; `auto-revert-interval' seconds, at cost of some slowdown
;; (add-hook 'dired-mode-hook #'auto-revert-mode) ; auto-refresh on file change
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ; hide details initially

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

(use-package dired-filter
  :bind (:map dired-mode-map
         ("/" . dired-filter-map))
  :hook (dired-mode . dired-filter-mode)
  :init (setq-default dired-filter-stack nil))

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

;; bind `my-yank-from-kill-ring'
(global-set-key (kbd "C-c y") #'my-yank-from-kill-ring)

;; typing text replaces the active (i.e. selected) region, if any is selected
(delete-selection-mode)

;; use single spaces after sentences
(setq sentence-end-double-space nil)

;; enable transparent editing of GPG files
(require 'epa-file)
(epa-file-enable)

;; display available bindings in popup
(use-package which-key
  :bind ("C-c H w" . which-key-show-top-level)
  :init
  (setq which-key-allow-multiple-replacements t
        which-key-compute-remaps t
        ;; configure for manual activation using C-h in the middle of a key seq
        ;; see https://github.com/justbur/emacs-which-key#manual-activation
        which-key-idle-delay 10000
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t)
  (which-key-mode 1)
  :config
  ;; hide mode line lighter
  (add-to-list 'my-mode-lighter-abbrev-alist '(which-key-mode . "")))

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

;; multiple cursors
;; using `set-rectangular-region-anchor' is probably the easiest
;; see https://emacs.stackexchange.com/a/773
(use-package multiple-cursors
  :bind (("M-C" . mc/edit-lines)
         ("M-V" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
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
  ;; abbreviate mode line lighter
  (add-to-list 'my-mode-lighter-abbrev-alist '(yas-minor-mode . " ¥"))
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
  :bind (:map paredit-mode-map
         ("{" . paredit-open-curly)
         ("}" . paredit-close-curly))
  :hook ((emacs-lisp-mode . paredit-mode)
         ;; when in minibuffer via `eval-expression`
         (eval-expression-minibuffer-setup . paredit-mode)
         ;; *scratch* default mode
         (lisp-interaction-mode . paredit-mode))
  :config
  ;; non-terminal bindings, see https://www.racket-mode.com/#paredit
  (unless terminal-frame
    (define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
    (define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly))
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
  :config
  ;; hide mode line lighter
  (add-to-list 'my-mode-lighter-abbrev-alist '(undo-tree-mode . ""))
  ;; enable globally
  (global-undo-tree-mode))

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
  ;; uncomment below to enable `elmacro-mode' by default, at the cost
  ;; of some slowdown (it is better to enable it only when needed)
  ;; (elmacro-mode 1)
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

;; use built-in DWIM versions of default editing commands
;; note that comment insertion command ("M-;") is already DWIM-ified
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)

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
    :bind (("C-c N" . notmuch)
           :map notmuch-show-mode-map
           ("d" . notmuch-show--toggle-trash-tag)
           ("SPC" . notmuch-show-advance) ; don't archive by default when advancing
           ("S-SPC" . notmuch-show-rewind)
           :map notmuch-search-mode-map
           ("d" . notmuch-search--toggle-trash-tag)
           ("S-SPC" . notmuch-search-scroll-down)
           :map notmuch-tree-mode-map
           ("d" . notmuch-tree--toggle-trash-tag)
           ("S-SPC" . notmuch-tree-scroll-message-window-back))
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
    ;; unbind "<C-tab>" as it conflicts with the `tab-next' binding
    (define-key notmuch-hello-mode-map (kbd "<C-tab>") nil)
    (define-key notmuch-show-mode-map (kbd "<C-tab>") nil)
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

;; async version of `notmuch-poll' that sends a message when complete
;; buffer should be manually refreshed with "g" after polling is done
(with-eval-after-load 'notmuch
  (defun notmuch-poll-async ()
    "Like `notmuch-poll' but asynchronous."
    (interactive)
    (notmuch-start-notmuch
     "notmuch-new"
     nil
     (lambda (_proc change)
       (message (concat "notmuch-new: %s"
                        "\n"
                        "Please refresh buffer as needed.")
                change))
     "new"))
  ;; overwrite default "G" bindings in notmuch modes
  (define-key notmuch-common-keymap (kbd "G") #'notmuch-poll-async))

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
  (dolist (keymap (list notmuch-hello-mode-map ; see https://stackoverflow.com/q/27084989
                        notmuch-search-mode-map
                        notmuch-tree-mode-map))
    (define-key keymap (kbd "C-t")
      #'notmuch--toggle-search-tag-visibility)))

;; provides HTML email composition using Org-mode
;; for autogreeting, set `org-msg-greeting-fmt' to "\nHi *%s*,\n\n"
(use-package org-msg
  :after notmuch ; only load if notmuch package is also loaded
  :init
  (setq org-msg-options (concat "html-postamble:nil H:5 num:nil ^:{} "
                                "toc:nil author:nil email:nil \\n:t")
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt nil
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '(text html)
        org-msg-text-plain-alternative t
        org-msg-convert-citation t)
  :config
  ;; enable HTML email message composition
  (org-msg-mode 1)
  ;; bindings to toggle HTML email message composition
  (dolist (keymap (list notmuch-hello-mode-map
                        notmuch-search-mode-map
                        notmuch-show-mode-map
                        notmuch-tree-mode-map))
    (define-key keymap (kbd "M") #'org-msg-mode)))

(condition-case nil
    (require 'ol-notmuch)
  (error (message "ol-notmuch is not installed or requires Org 9.2.3+")))

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

;; render DocView images at full resolution and downsample using the
;; built-in image scaler in Emacs 28+ or ImageMagick in prior versions
(when (display-graphic-p)
  (setq doc-view-scale-internally t
        doc-view-resolution (floor (my-frame-monitor-dpi))
        doc-view-image-width (let* ((attrs (frame-monitor-attributes))
                                    (geom (assoc 'geometry attrs))
                                    (width-pixels (nth 3 geom)))
                               ;; default width is 40% of screen width
                               (floor (* 0.4 width-pixels)))))

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
  :after org
  :init (add-to-list 'auto-mode-alist
                     '("\\.epub\\'" . nov-mode)))

(when (executable-find "jq")
  (use-package jq-mode
    :mode "\\.jq\\'"))

;; provides a major mode for editing JSON files
(use-package json-mode
  :defer t
  :init (setq js-indent-level 2
              json-reformat:indent-width 2
              json-reformat:pretty-string? nil))

(when (executable-find "jq")
  (with-eval-after-load 'flymake-quickdef
    (flymake-quickdef-backend flymake-jq-backend
      :pre-let ((jq-exec (executable-find "jq")))
      :pre-check (unless jq-exec (error "Cannot find jq executable"))
      :write-type 'file
      :proc-form (list jq-exec "." fmqd-temp-file)
      :search-regexp
      "\\(parse error: \\)?\\(.+\\) at line \\([[:digit:]]+\\), column \\([[:digit:]]+\\).*$"
      :prep-diagnostic (let* ((msg (match-string 2))
                              (lnum (string-to-number (match-string 3)))
                              (lcol (string-to-number (match-string 4)))
                              (pos (flymake-diag-region fmqd-source lnum lcol))
                              (beg (car pos))
                              (end (cdr pos))
                              (type :error))
                         (list fmqd-source beg end type msg)))
    ;; define function for enabling the Flymake backend
    (defun flymake-jq-setup ()
      "Enable jq backend for Flymake."
      (add-hook 'flymake-diagnostic-functions
                #'flymake-jq-backend
                nil
                t))
    (with-eval-after-load 'json-mode
      ;; enable Flymake jq backend in JSON buffers
      (add-hook 'json-mode-hook #'flymake-jq-setup t)
      (add-hook 'json-mode-hook #'flymake-mode))))

(when (executable-find "jq")
  (with-eval-after-load 'json-mode
    ;; use jq with the basic operator "." (to pretty print but leave
    ;; the input unmodified otherwise) to format JSON code
    (with-eval-after-load 'reformatter
      ;; json
      ;; define `json-jq-format-{buffer|region|on-save-mode}'
      (reformatter-define json-jq-format
        :program "jq"
        :args '("--sort-keys" "." "-")
        :group 'json-mode
        :lighter " JSONFmt")
      ;; dwim function calling `json-jq-format-region' if a region is
      ;; selected, or `json-jq-format-buffer' otherwise
      (defun json-jq-format-buffer-or-region ()
        "Format the current JSON buffer or a region if selected.
Formatting a selected region only works on top-level objects."
        (interactive)
        (cond
         ((use-region-p) (json-jq-format-region (region-beginning)
                                                (region-end)))
         (t (json-jq-format-buffer))))
      ;; jsonlines
      ;; define `jsonl-jq-format-{buffer|region|on-save-mode}'
      (reformatter-define jsonl-jq-format
        :program "jq"
        :args '("--compact-output" "--sort-keys" "." "-")
        :group 'json-mode
        :lighter " JSONLFmt")
      ;; dwim function calling `jsonl-jq-format-region' if a region is
      ;; selected, or `jsonl-jq-format-buffer' otherwise
      (defun jsonl-jq-format-buffer-or-region ()
        "Format the current JSONL buffer or a region if selected.
Formatting a selected region only works on top-level objects."
        (interactive)
        (cond
         ((use-region-p) (jsonl-jq-format-region (region-beginning)
                                                 (region-end)))
         (t (jsonl-jq-format-buffer)))))))

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
    (setq markdown-command (concat "pandoc --from markdown --to html"
                                   " --standalone"
                                   " --katex"
                                   " --highlight-style=pygments"
                                   " --quiet") ; suppress warnings
          markdown-command-needs-filename t)))

(use-package markdown-toc
  :after markdown-mode)

;; provides a major mode for editing YAML files
(use-package yaml-mode
  :commands yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Org-mode

;; ;; install ELPA version of Org
;; (my-install-elpa-package 'org)

;; rebind `org-force-cycle-archived' in older Org versions to not
;; conflict with the `tab-next' default binding
(with-eval-after-load 'org
  (when (version< (org-version) "9.4")
    (define-key org-mode-map (kbd "<C-tab>") nil)
    (org-defkey org-mode-map (kbd "C-c C-<tab>") #'org-force-cycle-archived)))

;; set Org directory and inbox file
(setq org-directory (file-truename (file-name-as-directory (expand-file-name "~/org"))))
(defvar my-org-agenda-inbox (concat org-directory "agenda/inbox.org")
  "Path to Org agenda inbox.")
(defvar my-org-someday-inbox (concat org-directory "agenda/someday.org")
  "Path to Org someday inbox.")
(defvar my-org-journal-file (concat org-directory "agenda/journal.org")
  "Path to Org journal file.")
(defvar my-org-scratch-file (concat org-directory "agenda/scratch.org")
  "Path to Org scratch file.")
(defvar my-org-websnippet-file (concat org-directory "agenda/websnippets.org")
  "Path to Org websnippet file.")

;; basic Org-mode settings
(setq org-adapt-indentation nil ; don't auto-indent when promoting/demoting
      org-attach-dir-relative t ; use relative directories when setting DIR property using `org-attach-set-directory'
      ;; org-blank-before-new-entry '((heading . nil) ; don't auto-add new lines
      ;;                              (plain-list-item . nil)) ; same as above
      org-catch-invisible-edits 'show-and-error
      org-confirm-babel-evaluate nil ; don't confirm before evaluating code blocks in Org documents
      org-cycle-separator-lines 2 ; collapse single item separator lines when cycling
      org-deadline-warning-days 3 ; warn starting 3 days before deadline
      org-edit-src-content-indentation 2
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-goto-interface 'outline-path-completion
      org-hide-emphasis-markers nil
      org-hide-leading-stars nil
      org-highlight-latex-and-related '(latex script entities) ; highlight LaTeX fragments with the `org-highlight-latex-and-related' face
      org-image-actual-width (list (/ (display-pixel-width) 3)) ; auto-resize displayed images to one-third of display width
      org-link-file-path-type 'adaptive ; use relative paths for links to files in Org file dir or subdirs, absolute otherwise
      org-log-done 'time ; log time that task was marked DONE
      org-log-into-drawer t
      org-outline-path-complete-in-steps nil
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts nil ; don't render sub/superscripts in-buffer
      org-return-follows-link t
      org-src-fontify-natively nil ; don't syntax color org source blocks
      org-src-preserve-indentation t ; preserve src code block indentation on export and when switching btw org buffer and edit buffer
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window ; reuse Org file window for editing source blocks when using "C-c '"
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
;;      ---------------------
;;      |                   |
;;      |                   V
;; --> TODO.. -> NEXT... -> DONE ----->
;;     | Λ  |    |   | Λ    Λ      |
;;     V |  |    |   V |    |      |
;;     HOLD |    |   WAIT ---      |
;;      |   |    |   |             |
;;      V   V    V   V             |
;;     CANX........... -------------
;;      (note records why it was cancelled)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                          (sequence "HOLD(h@/!)" "WAIT(w@/!)" "|" "CANX(c@/!)")))

;; Org capture templates
(defun my-org-goto-end-of-org-file ()
  "Goto end of selected user file starting from `org-directory'."
  (let ((path (read-file-name
               "File: " org-directory nil nil nil
               (lambda (x) (string-suffix-p ".org" x)))))
    (find-file path)
    (goto-char (point-max))))

(setq org-capture-templates '(("t" "New Task" entry (file my-org-agenda-inbox)
                               "* TODO %i%?\n%U")
                              ("l" "Linked Task" entry (file my-org-agenda-inbox)
                               "* TODO %a%?\n%U")
                              ("s" "Someday Task" entry (file my-org-someday-inbox)
                               "* TODO %i%?\n%U")
                              ("i" "Interrupt Task" entry (function my-org-goto-end-of-org-file)
                               "* NEXT %i%?\n%U"
                               :jump-to-captured t :clock-in t :clock-resume t)
                              ("j" "Journal Entry" entry
                               (file+olp+datetree my-org-journal-file)
                               "**** %?\n%T"
                               :tree-type week :clock-in t :clock-resume t)
                              ("J" "Schedule Journal Entry" entry
                               (file+olp+datetree my-org-journal-file)
                               "**** %?\n%T"
                               :tree-type week :time-prompt t)))

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
(setq org-tag-alist '((:startgroup) ;; export
                      ("export" . ?9)
                      ("noexport" . ?0)
                      (:endgroup)
                      ;; prioritization (e.g. Eisenhower matrix)
                      ("important" . ?1)
                      ("urgent" . ?2)
                      (:newline)
                      ;; entry context, in addition to category
                      ("@work" . ?w)
                      ("@life" . ?l)
                      ("@learn" . ?e)
                      (:startgroup) ;; special meeting types
                      ("hiring" . ?h)
                      ("managing" . ?m)
                      ("vendor" . ?v)
                      ("sales" . ?s)
                      ("strategy" . ?t)
                      (:endgroup)))

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

;; have Org mode open PDF files within Emacs
(with-eval-after-load 'org
  (push '("\\.pdf\\'" . emacs) org-file-apps))

;; org-agenda settings:
;; - narrow to subtree in org-agenda-follow-mode ("F" in agenda)
;; - full-frame Agenda view
;; - use ~/ORG-DIRECTORY/*.org files as Org agenda files
;; - de-duplicate entries with both a scheduled and deadline date
;; - don't show entries with scheduled or deadline dates that are done
(setq org-agenda-follow-indirect t
      org-agenda-restore-windows-after-quit t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-skip-scheduled-delay-if-deadline nil
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-agenda-window-setup 'only-window
      org-agenda-files (seq-filter
                        (lambda (x)
                          (and
                           (not (string-suffix-p my-org-agenda-inbox x))
                           (not (string-suffix-p my-org-someday-inbox x))
                           (not (string-suffix-p my-org-scratch-file x))
                           (not (string-suffix-p my-org-websnippet-file x))))
                        (file-expand-wildcards (concat org-directory "agenda/*.org"))))

;; add separator between each day in agenda view
(setq org-agenda-format-date
      (lambda (date)
        (let* ((datestr (org-agenda-format-date-aligned date))
               (separator-width (- (window-width)
                                   (string-width datestr)
                                   1)))
          (concat "\n" datestr " " (make-string separator-width ?_)))))

;; helper functions for custom agenda commands
(defun my-org-agenda-to-deadline-prefix-str ()
  "Descriptor string for days to deadline for Org entry at point."
  (let ((deadline (org-get-deadline-time (point))))
    (when deadline
      (let ((days-left (org-time-stamp-to-now (format-time-string "%F" deadline))))
        (cond ((< days-left (- 1)) (format "%3d d. ago" (- days-left)))
              ((= days-left (- 1)) (format " Yesterday" days-left))
              ((= days-left 0)     (format "     Today" days-left))
              ((= days-left 1)     (format "  Tomorrow" days-left))
              ((> days-left 1)     (format " In %3d d." days-left)))))))

;; custom agenda commands
(setq org-agenda-custom-commands
      `(("." "Today's agenda and all TODO entries"
         ((agenda "" ((org-agenda-span 1)))
          (todo "NEXT" ((org-agenda-todo-ignore-with-date nil)
                        (org-agenda-sorting-strategy '(ts-up priority-down effort-up category-keep alpha-up))))
          (todo "WAIT" ((org-agenda-todo-ignore-with-date nil)
                        (org-agenda-sorting-strategy '(ts-up priority-down effort-up category-keep alpha-up))))
          (todo "TODO" ((org-agenda-todo-ignore-with-date nil)
                        (org-agenda-sorting-strategy '(ts-up priority-down effort-up category-keep alpha-up))))
          (todo "HOLD" ((org-agenda-todo-ignore-with-date nil)
                        (org-agenda-sorting-strategy '(ts-up priority-down effort-up category-keep alpha-up))))))
        ("u" "Undated TODO entries"
         ((todo "NEXT" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-sorting-strategy '(priority-down effort-up category-keep alpha-up))))
          (todo "WAIT" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-sorting-strategy '(priority-down effort-up category-keep alpha-up))))
          (todo "TODO" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-sorting-strategy '(priority-down effort-up category-keep alpha-up))))
          (todo "HOLD" ((org-agenda-todo-ignore-with-date t)
                        (org-agenda-sorting-strategy '(priority-down effort-up category-keep alpha-up))))))
        ("d" "Deadlines"
         ((tags-todo "DEADLINE<\"<today>\"" ; tasks that are past deadline
                     ((org-agenda-prefix-format " %(my-org-agenda-to-deadline-prefix-str) %i %-12:c%?-12t% s")
                      (org-agenda-sorting-strategy '(deadline-up priority-down scheduled-up effort-up category-keep alpha-up))))
          (tags-todo "DEADLINE>=\"<today>\"" ; tasks with upcoming deadlines
                     ((org-agenda-prefix-format " %(my-org-agenda-to-deadline-prefix-str) %i %-12:c%?-12t% s")
                      (org-agenda-sorting-strategy '(deadline-up priority-down scheduled-up effort-up category-keep alpha-up))))))
        ("i" "Inbox entries"
         ((alltodo "" ((org-agenda-files '(,my-org-agenda-inbox))
                       (org-agenda-sorting-strategy '(priority-down deadline-up scheduled-up effort-up category-keep alpha-up))))))
        ("o" "Someday entries"
         ((alltodo "" ((org-agenda-files '(,my-org-someday-inbox))
                       (org-agenda-sorting-strategy '(priority-down deadline-up scheduled-up effort-up category-keep alpha-up))))))))

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
(use-package org-download
  :after org
  :config
  ;; Mac screenshot command
  (setq org-download-screenshot-method
        (cond ((memq window-system '(mac ns)) "screencapture -i %s")
              (t "gnome-screenshot -a -f %s"))) ; default to GNOME screenshot tool
  (setq org-download-method 'attach ; use `org-attach' machinery
        org-download-timestamp "%Y%m%d%H%M%S-"))

;; presentations from Org documents
(use-package org-tree-slide
  :init (setq org-tree-slide-activate-message "Start slideshow mode"
              org-tree-slide-deactivate-message "End slideshow mode"
              org-tree-slide-fold-subtrees-skipped nil)
  :config
  ;; custom org-tree-slide profile
  (defun my-org-tree-slide-custom-profile ()
    "Set variables for custom org-tree-slide profile.

`org-tree-slide-header'            => t
`org-tree-slide-slide-in-effect'   => nil
`org-tree-slide-heading-emphasis'  => t
`org-tree-slide-cursor-init'       => t
`org-tree-slide-modeline-display'  => 'outside
`org-tree-slide-skip-done'         => nil
`org-tree-slide-skip-comments'     => t"
    (interactive)
    (setq org-tree-slide-header t
          org-tree-slide-slide-in-effect nil
          org-tree-slide-heading-emphasis t
          org-tree-slide-cursor-init t
          org-tree-slide-modeline-display 'outside
          org-tree-slide-skip-done nil
          org-tree-slide-skip-comments t)
    (message "custom profile: ON"))
  ;; use custom profile
  (call-interactively #'my-org-tree-slide-custom-profile)
  ;; unbind some default mode bindings
  (define-key org-tree-slide-mode-map (kbd "C-x s c") nil)
  ;; (define-key org-tree-slide-mode-map (kbd "C-x <") nil)
  ;; (define-key org-tree-slide-mode-map (kbd "C-x >") nil)
  ;; add mode bindings
  (define-key org-tree-slide-mode-map (kbd "C-c c") #'org-tree-slide-content)
  (define-key org-mode-map (kbd "<f8>") #'org-tree-slide-mode)
  (define-key org-mode-map (kbd "S-<f8>") #'org-tree-slide-skip-done-toggle)
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

;; load Org backend for exporting to Markdown
(with-eval-after-load 'org
  (require 'ox-md))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init (setq org-superstar-headline-bullets-list '("◉" "◇" "○" "▷")
              ;; don't prettify plain lists, which can be slow
              org-superstar-prettify-item-bullets nil))

;; start server and load org-protocol
(server-mode 1)
(require 'org-protocol)

;; add capture template for web snippets
(setq org-websnippet-capture-file my-org-websnippet-file)
(add-to-list 'org-capture-templates
             `("W" "Websnippet" entry
               (file+headline ,org-websnippet-capture-file "Unsorted")
               "* %?%:description\n:PROPERTIES:\n:URL: %:link\n:ADDED: %U\n:END:\n%:initial\n")
             t)

;; send notifications for Org agenda deadlines and scheduled tasks
(use-package org-wild-notifier
  :hook (after-init . org-wild-notifier-mode))

(when (executable-find "pikchr")
  (with-eval-after-load 'org
    (setq org-babel-pikchr-cmd (executable-find "pikchr"))
    (require 'ob-pikchr)))

;; Enable Org pre-9.2 structure expansions, e.g. ~<s~ followed by TAB
(with-eval-after-load 'org
  (require 'org-tempo nil :noerror))

;; Programming / Buffer reformatter macro

;; defines the `reformatter-define' macro that allows definition of
;; commands that run reformatters on the current buffer
(use-package reformatter)

;; Programming / Flymake syntax checker

;; basic Flymake customizations
(setq flymake-no-changes-timeout 0.5 ;; auto check buffer change wait time
      flymake-start-on-save-buffer nil) ;; don't run checks when saving

;; deferred Flymake customizations
(with-eval-after-load 'flymake
  ;; don't use legacy Flymake checker
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  ;; function for toggling Flymake diagnostics window
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
  ;; shorten Flymake mode line symbol
  (defun my-flymake-modeline-filter (ret)
    "Filter function for `flymake--mode-line-format`."
    (setf (seq-elt (car ret) 1) " FlyM")
    ret)
  (advice-add #'flymake--mode-line-format :filter-return
              #'my-flymake-modeline-filter)
  ;; convenience bindings
  (define-key flymake-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c ! l") #'my-toggle-flymake-diagnostics))

;; enable Flymake when editing Emacs Lisp buffers
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(use-package flymake-quickdef
  :demand t)

;; Programming / DevSkim and Flymake

;; Code security analysis using devskim, https://github.com/microsoft/DevSkim
;; A Flymake backend for it is defined here, and can be used by calling
;; `flymake-devskim-setup' before `flymake-mode' in a given mode's hook, e.g.
;;   (add-hook 'python-mode-hook 'flymake-devskim-setup)
;;   (add-hook 'python-mode-hook 'flymake-mode t)
;; For more info on the different severity types, see
;; https://github.com/microsoft/DevSkim/wiki/Rule-Object-Schema
(with-eval-after-load 'flymake-quickdef
  (flymake-quickdef-backend flymake-devskim-backend
    :pre-let ((devskim-exec (executable-find "devskim")))
    :pre-check (unless devskim-exec (error "Cannot find devskim executable"))
    :write-type 'file
    :proc-form (list devskim-exec
                     "analyze"
                     "-f" "text"
                     "-o" "%L:%C: %S : [%R] %N"
                     fmqd-temp-file)
    :search-regexp
    "\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\) : \\(.+\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (lcol (string-to-number (match-string 2)))
                            (severity (downcase (match-string 3)))
                            (msg (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum lcol))
                            (beg (car pos))
                            (end (cdr pos))
                            (type (cond
                                    ((string= severity "critical") :error)
                                    ((string= severity "important") :error)
                                    ((string= severity "moderate") :warning)
                                    ((string= severity "best-practice") :note)
                                    ((string= severity "manual-review") :note)
                                    (t :note))))
                       (list fmqd-source beg end type msg)))
  ;; define function for toggling the Flymake backend
  (defun flymake-devskim-toggle ()
    "Toggle devskim backend for Flymake."
    (interactive)
    (if (memq 'flymake-devskim-backend flymake-diagnostic-functions)
        (remove-hook 'flymake-diagnostic-functions #'flymake-devskim-backend t)
      (add-hook 'flymake-diagnostic-functions #'flymake-devskim-backend nil t))))

;; Programming / Conda package and environment manager

(when (executable-find "conda")
  (use-package conda
    :init (setq conda-anaconda-home (seq-some (lambda (fname)
                                                (and (file-directory-p fname) fname))
                                              (list
                                               (expand-file-name "~/miniforge3/")
                                               (expand-file-name "~/miniconda3/")
                                               (expand-file-name "~/anaconda3/"))))
    :config
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)
    ;; display current conda env in the mode line
    (define-minor-mode conda-mode-line-mode
      "Global minor mode for displaying current conda env in the mode line."
      :init-value nil
      :keymap nil
      :global t
      :lighter (:eval (concat " ℅["
                              (if conda-env-current-name
                                  (format "%s"
                                          (truncate-string-to-width
                                           conda-env-current-name
                                           15 nil nil "…"))
                                "-")
                              "]"))
      :group 'conda)
    (conda-mode-line-mode 1)))

;; Programming / lsp-mode Language Server Protocol client

;; lsp-mode Language Server Protocol client
;; auto-signature-help activation is not enabled by default, but to
;; show it activate it using `lsp-toggle-signature-auto-activate' or
;; C-S-SPC to peek, https://github.com/emacs-lsp/lsp-mode/issues/1223
(use-package lsp-mode
  :init (setq lsp-print-io nil ; disable logging packets between Emacs and LS
              lsp-print-performance nil ; disable performance logging
              lsp-enable-file-watchers nil ; disable file watchers by default, use dir local vars if needed
              lsp-eldoc-enable-hover nil ; don't have eldoc display hover info
              lsp-eldoc-render-all nil ; don't show all returned from document/onHover, only symbol info
              lsp-enable-on-type-formatting nil ; don't have the LS automatically format the document when typing
              lsp-modeline-code-actions-enable nil ; don't show code actions by default
              lsp-modeline-diagnostics-enable nil ; don't show LS diagnostics by default
              lsp-diagnostic-package :flymake ; use Flymake for syntax checking
              lsp-signature-auto-activate nil) ; don't automatically show signature
  :config
  ;; tighter mode line lighter
  (add-to-list 'my-mode-lighter-abbrev-alist
               '(lsp-mode . (:eval (concat " ʪ["
                                           (let (workspaces (lsp-workspaces))
                                             (if workspaces
                                                 (mapconcat #'lsp--workspace-print
                                                            workspaces
                                                            "][")
                                               "-"))
                                           "]"))))
  ;; enable which-key integration
  (with-eval-after-load 'which-key
    (add-hook 'lsp-mode #'lsp-enable-which-key-integration)))

;; company backend for LSP-driven completion
(use-package company-lsp
  :after lsp-mode
  :init (setq company-lsp-cache-candidates t))

;; Programming / dap-mode Debug Adaptor Protocol client

;; client for Debug Adaptor Protocol servers
(use-package dap-mode
  :after lsp-mode)

;; Programming / Bash and sh shell scripts

(with-eval-after-load 'flymake-quickdef
  (flymake-quickdef-backend flymake-shellcheck-backend
    :pre-let ((shellcheck-exec (executable-find "shellcheck")))
    :pre-check (unless shellcheck-exec (error "Cannot find shellcheck executable"))
    :write-type 'file
    :proc-form (list shellcheck-exec
                     "-f" "gcc"
                     fmqd-temp-file)
    :search-regexp
    "^.+?:\\([0-9]+\\):\\([0-9]+\\): \\(.*\\): \\(.*\\)$"
    :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                            (lcol (string-to-number (match-string 2)))
                            (severity (downcase (match-string 3)))
                            (msg (match-string 4))
                            (pos (flymake-diag-region fmqd-source lnum lcol))
                            (beg (car pos))
                            (end (cdr pos))
                            (type (cond
                                    ((string= severity "note") :note)
                                    ((string= severity "warning") :warning)
                                    (t :error))))
                       (list fmqd-source beg end type msg)))
  ;; define function for enabling the Flymake backend
  (defun flymake-shellcheck-setup ()
    "Enable shellcheck backend for Flymake."
    (add-hook 'flymake-diagnostic-functions #'flymake-shellcheck-backend nil t))
  (with-eval-after-load 'sh-script
    (add-hook 'sh-mode-hook #'flymake-shellcheck-setup)
    (add-hook 'sh-mode-hook #'flymake-mode)))

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
  :init (setq nrepl-log-messages t))

;; linting, requires clj-kondo be installed on the system
;; see https://github.com/borkdude/clj-kondo for install instructions
(when (executable-find "clj-kondo")
  ;; Flymake config, adapted from https://github.com/turbo-cafe/flymake-kondor
  (with-eval-after-load 'flymake-quickdef
    (flymake-quickdef-backend flymake-clj-kondo-backend
      :pre-let ((clj-kondo-exec (executable-find "clj-kondo")))
      :pre-check (unless clj-kondo-exec (error "Cannot find clj-kondo executable"))
      :write-type 'pipe
      :proc-form (list clj-kondo-exec "--lint" "-")
      :search-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]]+\\): \\(.+\\)$"
      :prep-diagnostic (let* ((lnum (string-to-number (match-string 1)))
                              (lcol (string-to-number (match-string 2)))
                              (severity (match-string 3))
                              (msg (match-string 4))
                              (pos (flymake-diag-region fmqd-source lnum lcol))
                              (beg (car pos))
                              (end (cdr pos))
                              (type (cond
                                     ((string= severity "error") :error)
                                     ((string= severity "warning") :warning)
                                     ((string= severity "info") :note)
                                     (t :note))))
                         (list fmqd-source beg end type msg)))
    (defun flymake-clj-kondo-setup ()
      "Enable clj-kondo backend for Flymake."
      (add-hook 'flymake-diagnostic-functions #'flymake-clj-kondo-backend nil t))
    ;; enable Flymake with clj-kondo backend when editing Clojure
    (with-eval-after-load 'clojure-mode
      (add-hook 'clojure-mode-hook 'flymake-clj-kondo-setup)
      (add-hook 'clojure-mode-hook 'flymake-mode t))))

;; Programming / Dyalog APL

;; support for editing Dyalog files and integration with Dyalog RIDE
(use-package dyalog-mode
  :config
  ;; support for entering APL glyphs using Dyalog RIDE bindings
  (use-package dyalog-apl-input
    :ensure nil) ; in site-lisp directory
  ;; setup Dyalog APL buffer-specific editing environment
  (defun dyalog-mode--setup ()
    "Setup code to run when entering a `dyalog-mode' buffer."
    ;; set font
    (let ((apl-font-name "APL385 Unicode"))
      (when (find-font (font-spec :name apl-font-name))
        (setq-local buffer-face-mode-face `(:family ,apl-font-name))
        (buffer-face-mode)))
    ;; set input method
    (set-input-method "dyalog-apl-prefix"))
  ;; run setup code when entering Dyalog APL buffers
  (add-hook 'dyalog-mode-hook #'dyalog-mode--setup)
  (add-hook 'dyalog-session-mode-hook #'dyalog-mode--setup)
  (add-hook 'dyalog-debugger-mode-hook #'dyalog-mode--setup))

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

;; Programming / fish shell scripts

(use-package fish-mode
  :init (setq fish-enable-auto-indent t
              fish-indent-offset 4))

;; Programming / J

;; load support for editing J scripts
(use-package j-mode
  :ensure nil ; in site-lisp directory
  :init (setq j-help-browser-function #'eww-browse-url)
  :config
  (with-eval-after-load 'flymake-quickdef
    (flymake-quickdef-backend flymake-j-console-debug-lint-backend
      :pre-let ((j-console-exec (executable-find j-console-cmd)))
      :pre-check (unless j-console-exec
                   (error "Cannot find J executable"))
      :write-type 'file
      :proc-form (list j-console-exec
                       "-js"
                       "require 'debug/lint'"
                       (concat "echo lint '" fmqd-temp-file "'")
                       "exit''")
      :search-regexp
      "│\\([[:digit:]]+\\)│\\(.+\\) *│$"
      :prep-diagnostic
      (let* ((lnum (1+ (string-to-number (match-string 1)))) ; lint output linenums are 0-indexed, but Emacs linenums are 1-indexed
             (msg (match-string 2))
             (pos (flymake-diag-region fmqd-source lnum))
             (beg (car pos))
             (end (cdr pos))
             (type :error))
        (list fmqd-source beg end type msg)))
    (defun flymake-j-console-debug-lint-setup ()
      "Enable jconsole debug/lint backend for Flymake."
      (add-hook 'flymake-diagnostic-functions #'flymake-j-console-debug-lint-backend nil t))
    ;; enable Flymake with j-console backend when editing J files
    (add-hook 'j-mode-hook #'flymake-j-console-debug-lint-setup)
    (add-hook 'j-mode-hook #'flymake-mode t)))

;; Programming / Python

;; install ELPA version of python.el
(my-install-elpa-package 'python)

;; use the py script for initializing the Python REPL if available
(when (executable-find "py")
  (setq python-shell-interpreter "py"
               python-shell-interpreter-args ""
               python-shell-prompt-detect-failure-warning nil)
  (with-eval-after-load 'python
    (add-to-list 'python-shell-completion-native-disabled-interpreters "py")))

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
(use-package ess
  :mode ("\\.R$" . R-mode)
  :commands (R-mode ess-switch-to-ESS)
  :init (setq ess-eval-visibly 'nowait
              ess-default-style 'RStudio
              ;; use Flymake only when buffer has an inferior process
              ess-use-flymake 'process))

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
(with-eval-after-load 'ess-r-mode
  (define-key ess-r-mode-map (kbd "M--") #'my-insert-R-assignment-operator)
  (define-key ess-r-mode-map (kbd "C-S-m") #'my-insert-R-forward-pipe-operator)
  (define-key inferior-ess-r-mode-map (kbd "M--") #'my-insert-R-assignment-operator)
  (define-key inferior-ess-r-mode-map (kbd "C-S-m") #'my-insert-R-forward-pipe-operator))

;; view data in ESS-R
(use-package ess-view-data
  :after ess-r-mode
  :bind (:map ess-r-mode-map
         ("C-c v" . ess-view-data-print))
  :init
  ;; set update print backend to knitr::kable() due to csv-mode
  ;; header-line errors when using the default print backend
  (setq ess-view-data-current-update-print-backend 'kable))

;; insert column or variable names or values in ESS-R, useful when
;; working with tidyverse
(use-package ess-r-insert-obj
  :after ess-r-mode
  :bind (:map ess-r-mode-map
         ("C-c i f" . ess-r-insert-obj-dt-name)
         ("C-c i c" . ess-r-insert-obj-col-name)
         ("C-c i C" . ess-r-insert-obj-col-name-all)
         ("C-c i v" . ess-r-insert-obj-value)
         ("C-c i V" . ess-r-insert-obj-value-all)))

(use-package poly-R)

(with-eval-after-load 'reformatter
  (with-eval-after-load 'ess-r-mode
    ;; define `ess-r-styler-format-buffer', `ess-r-styler-format-region'
    ;; and `ess-r-styler-format-on-save-mode'
    (reformatter-define ess-r-styler-format
      :program "Rscript"
      :args `("--vanilla"
              "-e"
              ,(mapconcat
                'identity
                '("options(styler.colored_print.vertical=FALSE)"
                  "con <- file(\"stdin\")"
                  "out <- styler::style_text(readLines(con))"
                  "close(con)"
                  "out")
                "; ")
              "-")
      :group 'ess-R
      :lighter 'RStylFmt)
    ;; dwim function that calls `ess-r-styler-format-region' if a region
    ;; is selected, or `ess-r-styler-format-buffer' otherwise
    (defun ess-r-styler-format-buffer-or-region ()
      "Format the current R buffer or a region if selected using styler.
Formatting a selected region only works on top-level objects."
      (interactive)
      (cond
       ((use-region-p) (ess-r-styler-format-region (region-beginning)
                                                   (region-end)))
       (t (ess-r-styler-format-buffer))))))

;; Programming / Racket

(use-package racket-mode
  :defer t
  :config
  (defun racket-mode--maybe-enable-racket-xp-mode ()
    "Enables `racket-xp-mode' if the \"racket\" executable is in system path.
This is useful for only enabling `racket-xp-mode' when the active
environment has Racket installed."
    (when (executable-find "racket")
      (racket-xp-mode 1)))
  (add-hook 'racket-mode-hook #'racket-mode--maybe-enable-racket-xp-mode))

;; Project interaction

;; project interaction library
(use-package projectile
  :demand t
  :init (setq projectile-completion-system 'default
              projectile-create-missing-test-files t ; create test file if none is found when toggling
              projectile-mode-line-prefix " ℙ"
              projectile-switch-project-action 'projectile-commander
              projectile-use-git-grep t) ; use git grep to skip backup, object, and untracked files when in a Git project
  :config
  ;; don't show project type in minor mode lighter
  (defun my-projectile-mode-line ()
    "Report project name in the modeline."
    (format "%s[%s]" projectile-mode-line-prefix (or (projectile-project-name) "-")))
  (setq projectile-mode-line-function 'my-projectile-mode-line)
  ;; enable mode globally
  (projectile-mode))

;; Org TODOs for projectile projects
;; in `org-projectile-per-project-filepath' at the project's root directory
(use-package org-projectile
  :after (org projectile)
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org"))

;; binding for calling Magit
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :config (add-hook 'magit-process-find-password-functions
                    #'magit-process-password-auth-source))

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
  :bind ("C-x G" . git-timemachine))

(use-package browse-at-remote)

;; per-project file trees
(use-package treemacs
  :demand t
  :bind ("C-c d t" . treemacs)
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
  :init (setq ebib-preload-bib-files '("main.bib")
              ebib-bib-search-dirs '("~/bib/"))
  :config
  ;; friendlier key bindings in `ebib-multiline-edit-mode' buffers
  (define-key ebib-multiline-mode-map (kbd "C-c C-c")
    #'ebib-quit-multiline-buffer-and-save)
  (define-key ebib-multiline-mode-map (kbd "C-c C-s")
    #'ebib-save-from-multiline-buffer)
  (define-key ebib-multiline-mode-map (kbd "C-c C-q")
    #'ebib-cancel-multiline-buffer)
  ;; custom user interface setup for `ebib-multiline-mode' buffers
  (defun my-ebib-multiline-mode--setup ()
    "Custom user interface setup for `ebib-multiline-mode' buffers."
    ;; show mode bindings in header
    (setq-local header-line-format
                "Edit, then exit with ‘C-c C-c’, save with ‘C-c C-s’ or abort with ‘C-c C-q’"))
  (add-hook 'ebib-multiline-mode-hook #'my-ebib-multiline-mode--setup)
  ;; wrapper function for inserting citations differently by major-mode
  (require 'org-ebib)
  (defun my-ebib-insert-citation ()
    "Wrapper function for inserting an ebib citation differently by major-mode."
    (interactive)
    (cond ((eq major-mode 'org-mode)
           (call-interactively #'org-ebib-insert-link))
          (t
           (call-interactively #'ebib-insert-citation)))))

;; browse and import bibliographic references
(use-package biblio
  :config
  ;; generate and pop up a BibTeX entry from DOI in a special buffer
  (defun my-biblio--get-bibtex-from-doi (doi)
    "Retrieves BibTeX entry matching DOI into the \"*Biblio output*\" buffer."
    (interactive "MDOI: ")
    (let ((buf (get-buffer-create "*Biblio output*")))
      (with-current-buffer buf
        (erase-buffer)
        (doi-insert-bibtex doi)
        (pop-to-buffer (current-buffer))
        (current-buffer)))))

;; Search

;; "C-c C-p" in grep bufs allow writing with changes pushed to files
(use-package wgrep
  :config (setq wgrep-auto-save-buffer nil
                wgrep-too-many-file-length 10))

(when (executable-find "rg")
  (use-package rg
    :bind ("<f6>" . rg-menu)))

;; jump to definition using ag or rg and applying heuristics
(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive nil
        dumb-jump-default-project "./"
        dumb-jump-prefer-searcher 'rg)
  :config
  ;; remove the default etags backend from the list of backends for
  ;; `xref-find-definitions' and add dumb-jump to the end of that list
  ;; so it is used as a fallback when no better finders are available
  (setq xref-backend-functions (remq 'etags--xref-backend
                                     xref-backend-functions))
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t))

;; jump to visible text using char-based decision tree
(use-package avy
  :config
  ;; bind over `goto-line' since it can be invoked by entering numbers
  ;; for `avy-goto-line' input instead characters in the decision tree
  (global-set-key [remap goto-line] #'avy-goto-line)
  ;; jump to location of any text that is visible in the current frame
  ;; bind over "M-g M-g" (use "M-g g" for `goto-line' instead)
  (global-set-key (kbd "M-g M-g") #'avy-goto-char-timer))

;; display, select and jump to links in various major modes
(use-package ace-link
  :config
  ;; bind "o" to calling ace-link in compilation-mode, Custom-mode,
  ;; eww-mode, help-mode, Info-mode and woman-mode
  (ace-link-setup-default)
  ;; bind "C-c M-o" to jump to link in Org mode
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c M-o") #'ace-link-org))
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c M-o") #'ace-link-org-agenda)))

(setq imenu-auto-rescan t)

;; menu list of major definitions across several buffers
(use-package imenu-anywhere
  :defer t
  :after imenu
  :bind ("C-c I" . imenu-anywhere))

;; show imenu as a list in a side buffer
(use-package imenu-list
  :defer t
  :after imenu
  :bind ("C-c I" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  ;; pulse target after selecting
  (add-hook 'imenu-list-after-jump-hook
            (lambda () (pulse-momentary-highlight-one-line (point))))
  ;; close imenu list after going to entry
  (advice-add 'imenu-list-goto-entry :after 'imenu-list-quit-window))

;; fold characters when searching with Isearch
(setq search-default-mode #'char-fold-to-regexp)

;; Visual (part 2)

;; resize window margins for nicer writing environment
(use-package olivetti)

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

;; display line numbers by default when editing code
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;; show trailing whitespace when editing code
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; show point location column number in mode line
(setq column-number-mode t)

;; show matching parentheses with no delay
(setq show-paren-delay 0)
(show-paren-mode 1)

;; pulse modified region
(use-package goggles
  :init (setq goggles-pulse-delay 0.07)
  :config
  (goggles-mode) ; enable mode
  (add-to-list 'my-mode-lighter-abbrev-alist '(goggles-mode . ""))) ; hide mode line lighter

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

(require 'censor)

;; add visual indentation guides
;; don't enable this in any mode by default as it can cause slowdown
;; when editing large files, as well as issues with copy-pasting
(use-package highlight-indent-guides
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-character ?\x2502))

;; pulse line when point is cycled btw top/middle/bottom of window
(advice-add 'move-to-window-line-top-bottom :after #'my-pulse-line)

;; pulse line after changing focused window using `other-window'
(advice-add 'other-window :after #'my-pulse-line)

;; also pulse line when Emacs regains focus, covers focus on new frame
(add-hook 'focus-in-hook #'my-pulse-line)

;; as well as pulsing the line when popping the mark ring
(advice-add 'pop-to-mark-command :after #'my-pulse-line)
(advice-add 'pop-global-mark :after #'my-pulse-line)

(require 'too-long-lines-mode)
(too-long-lines-mode 1)

(use-package hl-todo
  :bind (:map hl-todo-mode-map
         ("C-c t n" . hl-todo-next)
         ("C-c t p" . hl-todo-prev)
         ("C-c t o" . hl-todo-occur)
         ("C-c t i" . hl-todo-insert))
  :hook (prog-mode . hl-todo-mode))

;; Web

;; built-in Emacs text web browser
(use-package eww
  :ensure nil ;; built-in
  :commands (eww eww-follow-link)
  :init (setq eww-search-prefix "https://duckduckgo.com/lite?q=")
  :config
  ;; don't render images in HTML pages by default
  (setq-default shr-inhibit-images t)
  ;; toggle for enabling and disabling images
  (defun eww--toggle-images ()
    "Toggle displaying of images when rendering HTML."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload)
    (message "Images are now %s" (if shr-inhibit-images "off" "on")))
  (define-key eww-mode-map "I" #'eww--toggle-images))

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
;; use an archive date string format that is safe for Windows systems
(setq org-readitlater-archive-date-format-string
      "%Y-%m-%d-%a-%H-%M-%S-%Z") ; 2016-08-18-Thu-20-50-02-PDT
;; load custom package in the lisp subfolder of the user emacs directory
(require 'org-readitlater)
;; uncomment if not accessing org-readitlater commands via a transient
;; (define-key org-mode-map (kbd "C-c O") org-readitlater-keymap)

;; add capture template for org-readitlater
(with-eval-after-load 'org
  (defvar org-readitlater-capture-file (concat org-directory "readitlater/readitlater.org")
    "Path to Org file containing org-readitlater entries.")
  (add-to-list 'org-capture-templates
               `("a" "Archive Webpage" entry
                 (file+headline ,org-readitlater-capture-file "Unsorted")
                 "* %?%:description\n:PROPERTIES:\n:URL: %:link\n:READITLATER_BACKEND_OPTIONS: --isolate --no-css --no-fonts --no-frames --no-images --no-js\n:ADDED: %U\n:END:\n%:initial\n")
               t)
  ;; auto-download page after capturing with org-readitlater template
  (defun do-org-readitlater-dl-hook ()
    (when (equal (buffer-name)
                 (concat "CAPTURE-"
                         (file-name-nondirectory org-readitlater-capture-file)))
      (org-readitlater-archive)))
  (add-hook 'org-capture-before-finalize-hook #'do-org-readitlater-dl-hook))

;; Writing

;; advise flyspell jump functions to perform context actions after
;; they are run
(with-eval-after-load 'flyspell
  (advice-add 'flyspell-goto-next-error :after #'my-after-jump-context-actions))

;; unbind some Flyspell default bindings that conflict with other more
;; useful bindings
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)) ; `iedit-mode' binding

;; provides word lookups from a dictionary server
;; `dictionary-server' can be set to "localhost" to use a local
;; dictionary server like dictd or GNU Dico that implements RFC 2229
(use-package dictionary
  :init (setq dictionary-server "dict.org"
              dictionary-default-dictionary "*"))

;; grammar checking functions using LanguageTool
;; n-gram data, if any, should be in ~/languagetool/ngram-data/<lang>
(use-package langtool
  :init
  (setq langtool-default-language "en-US"
        langtool-user-arguments (let ((ngram-data-dir
                                       (file-name-as-directory
                                        (expand-file-name
                                         "~/languagetool/ngram-data/"))))
                                  (if (file-directory-p ngram-data-dir)
                                      `("--languagemodel" ,ngram-data-dir)
                                    nil))
        langtool-language-tool-jar (expand-file-name
                                    "~/jars/languagetool-commandline.jar")))

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

;; bindings for scrolling line-at-a-time like "C-e" and "C-y" in Vim
(global-set-key (kbd "C-S-e") #'scroll-up-line)
(global-set-key (kbd "C-S-y") #'scroll-down-line)

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

;; enable auth-source integration with pass
(when (executable-find "pass")
  (use-package auth-source-pass
    :demand t
    :config
    (defcustom auth-source-pass-filename-list '("~/.password-store")
      "Directory filenames of different pass repositories on the system."
      :group 'auth-source
      :type '(repeat string))
    (defun auth-source-pass-cycle-active-store ()
      "Sets `auth-source-pass-filename' by cycling through `auth-source-pass-filename-list'."
      (interactive)
      (let* ((cur-pos (seq-position auth-source-pass-filename-list
                                    auth-source-pass-filename))
             (new-pos (if cur-pos ; non-nil
                          (mod (1+ cur-pos)
                               (length auth-source-pass-filename-list))
                        0)) ; default to first entry
             (new-dir (elt auth-source-pass-filename-list
                           new-pos)))
        (setq auth-source-pass-filename new-dir)
        (message (concat "auth-source-pass-filename set to: "
                         auth-source-pass-filename))))
    (defun my-auth-source-pass-enable ()
      "Enable auth-source and pass integration."
      (interactive)
      (auth-source-pass-enable)
      (message "auth-source-password-store enabled"))
    (defun my-auth-source-pass-disable ()
      "Disable auth-source and pass integration."
      (interactive)
      ;; To add password-store to the list of sources, evaluate the following:
      (setq auth-sources (delete 'password-store auth-sources))
      ;; clear the cache (required after each change to #'auth-source-pass-search)
      (auth-source-forget-all-cached)
      (message "auth-source-password-store disabled"))))

;; emacs integration with pass password-store
(when (executable-find "pass")
  (use-package password-store))

;; extend `tabulated-list-mode' with more functionality, adapted from
;; https://emacsnotes.wordpress.com/2019/04/16/how-i-shortlist-add-ons-for-my-emacs-introducing-tablist/
(use-package tablist
  :config
  ;; enable `tablist-minor-mode' automatically
  (add-hook 'tabulated-list-mode-hook 'tablist-minor-mode)
  ;; tablist-minor-mode shadows "U" binding in package-menu-mode-map
  ;; so add an extra binding for package-menu-mark-upgrades
  (define-key package-menu-mode-map (kbd "C-c u") #'package-menu-mark-upgrades)
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

;; add remote user paths to the TRAMP remote search paths
(with-eval-after-load 'tramp-sh
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; repeating "C-SPC" after popping mark with "C-u C-SPC" pops it again
(setq set-mark-command-repeat-pop t)

(defun my-clear-register (register)
  "Clear the contents in register REGISTER."
  (interactive (list (register-read-with-preview "Clear register: ")))
  (set-register register nil)
  (message "Cleared register %c" register))

;; global binding for clearing a register's contents
(global-set-key (kbd "C-x r DEL") #'my-clear-register)

;; convert regexp to rx notation
(use-package xr)

;; alternative binding for opening the menu bar
(global-set-key (kbd "C-c e m") #'menu-bar-open)

;; OS-specific / macOS

;; on macOS, use Option keys as Meta and file polling for auto-revert
(when (eq system-type 'darwin)
  (setq auto-revert-use-notify nil ;; macOS does not support file notifications
        mac-option-modifier 'meta ;; use Option key as Meta
        mac-right-option-modifier 'left ;; right Option uses left's mapping
        mac-command-modifier 'super)) ;; keep Super key as is

(when (eq window-system 'ns)
  (setq mac-option-modifier 'meta ; use Option key as Meta
        mac-right-option-modifier 'left ; right Option uses left's mapping
        mac-command-modifier 'super) ; keep Super key as is
  ;; unbind "s-w" and "s-q" to avoid unintended frame and Emacs closure
  (global-unset-key (kbd "s-w"))
  (global-unset-key (kbd "s-q")))

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

;; increment DocView DPI resolution for Mac Retina screens
;; when using emacs-mac port
(when (eq window-system 'mac)
  (setq doc-view-resolution 300))

;; manage launchd jobs on macOS systems
(when (eq system-type 'darwin)
  (use-package launchctl
    :mode ("\\.plist\\'" . nxml-mode)
    :bind ("C-c x l" . launchctl)))

;; as of Emacs 27.1, `tab-bar-mode' does not show visual tabs in macOS
;; so in macOS show tab number and name (if renamed) in the mode line
(when (eq system-type 'darwin)
  (add-to-list 'my-mode-lighter-abbrev-alist
               '(tab-bar-mode . (:eval
                                 (concat
                                  " ↹["
                                  ;; tab num, and tab name if explicitly renamed
                                  (let* ((current-tab (tab-bar--current-tab))
                                         (is-explicit-name (alist-get 'explicit-name current-tab)))
                                    (concat (number-to-string (1+ (tab-bar--current-tab-index)))
                                            (when is-explicit-name
                                              (concat ":" (alist-get 'name current-tab)))))
                                  "]")))))

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

;; add transient popup for bibliography commands
(require 'ebib)
(require 'biblio)
(transient-define-prefix transient/bibliography ()
  "Various bibliography commands."
  ["Bibliography"
   ["Biblio"
    ("bl" "Search" biblio-lookup)
    ("bd" "Show BibTeX from DOI" my-biblio--get-bibtex-from-doi)
    ("bi" "Insert BibTeX from DOI" doi-insert-bibtex)
    ("bo" "Show open access from DOI" dissemin-lookup)
    ]
   ["Ebib"
    ("eb" "Open" ebib)
    ("ei" "Import" ebib-import)
    ("ec" "Cite" my-ebib-insert-citation)
    ]
   ]
  )
(global-set-key (kbd "C-c B") #'transient/bibliography)

;; add transient popup for bookmark commands
(transient-define-prefix transient/bookmarks ()
  "Various bookmark commands."
  ["Bookmarks"
   ["Navigate"
    ("j" "Jump" bookmark-jump)
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
(global-set-key (kbd "C-c e B") #'transient/bookmarks)

(require 'buffer-expose)

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

(defun transient/buffer--print-minor-modes ()
  "Print enabled minor modes for the current buffer."
  (interactive)
  (let* ((maybe-active-minor-modes (mapcar #'car minor-mode-alist))
         (active-minor-modes (seq-filter (lambda (mode)
                                           (and (boundp mode)
                                                (symbol-value mode)))
                                         maybe-active-minor-modes))
         ;; sort alphabetically
         (active-minor-modes-sorted (sort (mapcar #'symbol-name
                                                  active-minor-modes)
                                          'string<))
         ;; five minor modes to a line
         (active-minor-mode-tuples (seq-partition active-minor-modes-sorted 5))
         (active-minor-mode-tuples-strs (mapcar
                                         (lambda (tup)
                                           (mapconcat #'identity tup " "))
                                         active-minor-mode-tuples))
         (msg-str (mapconcat 'identity active-minor-mode-tuples-strs "\n")))
    (message msg-str)))

(defun transient/buffer--clone-indirect-buffer-other-window ()
  "Create an indirect buffer of the current one in another window.
This wraps `clone-indirect-buffer-other-window' but provides a
name for the cloned indirect buffer ending with \"-INDIRECT\"."
  (interactive)
  (let ((bufname (generate-new-buffer-name
                  (concat (buffer-name)
                          "-INDIRECT"))))
    (clone-indirect-buffer-other-window bufname t)))

;; add transient for buffer management commands
(transient-define-prefix transient/buffer ()
  "Buffer management commands."
  ["Buffer"
   ["Select"
    ("b" "Switch" switch-to-buffer)
    ("n" "Next" next-buffer :transient t)
    ("p" "Previous" previous-buffer :transient t)
    ("z" "Open external" transient/buffer--open-containing-dir-externally)
    ;; commands below are autoloaded, so there should be no need
    ;; to make sure (require 'browse-at-remote) is run prior
    ("gb" "Git browse" browse-at-remote)
    ("gw" "Git copy URL" browse-at-remote-kill)
    ]
   ["Hygiene"
    ("cr" "Whitespace report" whitespace-report)
    ("cw" "Whitespace cleanup" whitespace-cleanup)
    ("ci" "Indent" transient/buffer--indent-region-or-buffer)
    ("ct" "Untabify" transient/buffer--untabify-region-or-buffer)
    ("ca" "All hygiene ops" transient/buffer--apply-all-hygiene-ops-region-or-buffer)
    ]
   ["File operations"
    ("r" "Revert" revert-buffer)
    ("B" "Bury" bury-buffer)
    ("U" "Unbury" unbury-buffer)
    ("s" "Save" save-buffer)
    ("S" "Save all" save-some-buffers)
    ("k" "Kill" kill-this-buffer)
    ("K" "Kill matching" kill-matching-buffers)
    ("o" "Kill others" transient/buffer--kill-other-buffers)
    ("T" "TRAMP cleanup" transient/buffer--tramp-cleanup-buffers)
    ("I" "Make indirect" transient/buffer--clone-indirect-buffer-other-window)
    ]
   ["Expose"
    ("ee" "All" buffer-expose)
    ("em" "Current mode" buffer-expose-current-mode)
    ("eM" "Major mode" buffer-expose-major-mode)
    ("ed" "Dired" buffer-expose-dired-buffers)
    ("e!" "Non-special" buffer-expose-no-stars)
    ("e*" "Special" buffer-expose-stars)
    ]
   ]
  [
   ["Other"
    ("M" "Minor modes" transient/buffer--print-minor-modes)
    ("R" (lambda ()
           (transient--make-description
            "Autorevert"
            (and (boundp 'auto-revert-mode) auto-revert-mode)))
     auto-revert-mode :transient t)
    ("M-r" (lambda ()
             (transient--make-description
              "Revert without query"
              revert-without-query-mode))
     revert-without-query-mode :transient t)
    ]
   ]
  )
(global-set-key (kbd "C-c e b") #'transient/buffer)

;; add transient popup for conda commands
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
  (global-set-key (kbd "C-c C") #'transient/conda))

(require 'debug)

(defun transient/debugger--list-variables ()
  "Print variables configured to invoke the debugger to the minibuffer."
  (interactive)
  (prin1 (debug--variable-list)))

;; add transient popup for debugger commands
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
(global-set-key (kbd "C-c e d") #'transient/debugger)

;; add transient popup for Ediff commands
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
(global-set-key (kbd "C-c d e") #'transient/ediff)

;; add transient popup for various editing commands
(transient-define-prefix transient/edit ()
  "Editing commands."
  ["Edit"
   ["Completion"
    ("/" "Dyn. abbrev" dabbrev-expand :transient t) ; built-in
    ("TAB" "Company" company-complete) ; autoloaded from company-mode.el
    ]
   ["Line"
    ("O" "New line above" my-open-line-above :transient t)
    ("o" "New line below" my-open-line-below :transient t)
    ("J" "Join lines" my-join-next-line :transient t)
    ]
   ["Multi-cursor" ; functions autoloaded from multiple-cursors.el
    ("C" "Edit lines" mc/edit-lines)
    ("V" "Rect select" set-rectangular-region-anchor)
    ("<" "Previous" mc/mark-previous-like-this :transient t)
    (">" "Next" mc/mark-next-like-this :transient t)
    ]
   ["Other"
    (";" "Iedit" iedit-mode) ; autoloaded from iedit.el
    ("=" "Expand region" er/expand-region) ; autoloaded from expand-region.el
    ]
   ]
  )
(global-set-key (kbd "C-c e e") #'transient/edit)

(defun transient/frame--previous-frame ()
  "Select previous frame."
  (interactive)
  (other-frame -1))

;; add transient popup for frame commands
(transient-define-prefix transient/frame ()
  "Frame management commands."
  ["Frame"
   ["Select"
    ("n" "Next" other-frame)
    ("p" "Previous" transient/frame--previous-frame)
    ("s" "By name" select-frame-by-name)
    ]
   ["Layout"
    ("0" "Delete frame" delete-frame)
    ("1" "Delete other frames" delete-other-frames)
    ("2" "Create new frame" make-frame-command)
    ]
   ["Resize"
    ("M" "Toggle maximized" toggle-frame-maximized :transient t)
    ("f" "Toggle fullscreen" toggle-frame-fullscreen :transient t)
    ]
   ]
  )
(global-set-key (kbd "C-c e f") #'transient/frame)

;; add transient popup for help commands
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
    ("dc" "Char" describe-char)
    ("df" "Function" describe-function)
    ("dF" "Face" describe-face)
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
    ("lf" "List faces" list-faces-display)
    ("ve" "View messages" view-echo-area-messages)
    ("vl" "View lossage" view-lossage)
    ("w" "Where is" where-is)
    ]
   ]
  )
(global-set-key (kbd "C-c H h") #'transient/help)

;; add transient for keyboard macros
(with-eval-after-load 'elmacro
  (transient-define-prefix transient/keyboard-macros ()
    "Keyboard macro commands. Tries to adhere to \"C-x C-k\" bindings."
    ["Keyboard Macros"
     ["Actions"
      ("C-s" "Start" kmacro-start-macro)           ; also "C-x ("
      ("C-k" "Call last" kmacro-end-or-call-macro) ; also "C-x )"
      ("C-r" "Call last on region" apply-macro-to-region-lines)
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
  (global-set-key (kbd "C-c k") #'transient/keyboard-macros))

(defun transient/marks-and-markers--xref-pop-marker-stack-all ()
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

(defun transient/marks-and-markers--push-mark ()
  "Push location of point into the mark ring."
  (interactive)
  (push-mark))
(defun transient/marks-and-markers--pop-mark ()
  "Pop the top location the mark ring and jump to it."
  (interactive)
  (set-mark-command t))
(defun transient/marks-and-markers--push-marker ()
  "Push location of point onto the marker stack."
  (interactive)
  (xref-push-marker-stack))
(defun transient/marks-and-markers--clear-marker-stack ()
  "Clear the marker stack."
  (interactive)
  (xref-clear-marker-stack)
  (message "Cleared `xref--marker-ring'"))

(transient-define-prefix transient/marks-and-markers ()
  "Commands for manipulating and managing marks and markers."
  ["Marks/Markers"
   ["Mark"
    ("SPC" "Push" transient/marks-and-markers--push-mark)
    ("RET" "Pop" transient/marks-and-markers--pop-mark :transient t)
    ("DEL" "Pop global" pop-global-mark :transient t)
    ]
   ["Marker"
    ("." "Push" transient/marks-and-markers--push-marker)
    ("," "Pop" xref-pop-marker-stack :transient t)
    ("<" "Pop all" transient/marks-and-markers--xref-pop-marker-stack-all)
    ("c" "Clear stack" transient/marks-and-markers--clear-marker-stack)
    ]
   ]
  )
(global-set-key (kbd "C-c M") #'transient/marks-and-markers)

;; add transient for accessing Org entry points
(with-eval-after-load 'org
  ;; file launchers
  (defun transient/org-launcher--find-my-org-agenda-inbox ()
    "Open a file buffer for `my-org-agenda-inbox'."
    (interactive)
    (find-file my-org-agenda-inbox))
  (defun transient/org-launcher--find-my-org-someday-inbox ()
    "Open a file buffer for `my-org-someday-inbox'."
    (interactive)
    (find-file my-org-someday-inbox))
  (defun transient/org-launcher--find-my-org-journal-file ()
    "Open a file buffer for `my-org-journal-file'."
    (interactive)
    (find-file my-org-journal-file))
  (defun transient/org-launcher--find-org-readitlater-capture-file ()
    "Open a file buffer for `org-readitlater-capture-file'."
    (interactive)
    (find-file org-readitlater-capture-file))
  (defun transient/org-launcher--find-org-websnippet-capture-file ()
    "Open a file buffer for `org-websnippet-capture-file'."
    (interactive)
    (find-file org-websnippet-capture-file))
  (defun transient/org-launcher--find-my-org-scratch-file ()
    "Open a file buffer for `my-org-scratch-file'."
    (interactive)
    (find-file my-org-scratch-file))

  (transient-define-prefix transient/org-launcher ()
    "Launcher for Org entry points."
    ["Org launcher"
     ["Main"
      ("a" "Agenda" org-agenda)
      ("c" "Capture" org-capture)
      ("b" "Switchb" org-switchb)
      ("l" "Store link" org-store-link)
      ]
     ["Find-file"
      ("fi" "Inbox" transient/org-launcher--find-my-org-agenda-inbox)
      ("fs" "Someday" transient/org-launcher--find-my-org-someday-inbox)
      ("fj" "Journal" transient/org-launcher--find-my-org-journal-file)
      ("fr" "Read-it-later" transient/org-launcher--find-org-readitlater-capture-file)
      ("fw" "Websnippets" transient/org-launcher--find-org-websnippet-capture-file)
      ("fx" "Scratch" transient/org-launcher--find-my-org-scratch-file)
      ]
     ]
    )
  (global-set-key (kbd "C-c o") #'transient/org-launcher))

;; add transient popup for Emacs package management
(transient-define-prefix transient/package ()
  "Emacs package management commands."
  ["Package management"
   ("l" "List packages" list-packages)
   ("i" "Install package" package-install)
   ("k" "Delete package" package-delete)
   ("r" "Reinstall package" my-package-reinstall)
   ("R" "Use-package report" use-package-report) ; requires use-package-compute-statistics set to non-nil before use-package declarations
   ]
  )
(global-set-key (kbd "C-c e P") #'transient/package)

;; add transient for password-store commands
(with-eval-after-load 'auth-source-pass
  (with-eval-after-load 'password-store
    (defun transient/password-store--toggle-auth-source-pass-store ()
      "Toggle auto-source and password-store integration."
      (interactive)
      (if (member 'password-store auth-sources)
          (my-auth-source-pass-disable)
        (my-auth-source-pass-enable)))
    (transient-define-prefix transient/password-store ()
      "Various password-store commands."
      [:description (lambda ()
                      (concat "Password store ["
                              (password-store-dir)
                              "]"))
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
        ("<tab>" "Cycle store" auth-source-pass-cycle-active-store :transient t)
        ("A" (lambda ()
               (transient--make-description
                "Auth-source integration"
                (member 'password-store auth-sources)))
         transient/password-store--toggle-auth-source-pass-store
         :transient t)
        ("C" "Clear" password-store-clear)
        ("I" "Init store" password-store-init)
        ("v" "Version" password-store-version)
        ]
       ]
      )
    (global-set-key (kbd "C-c P") #'transient/password-store)))

;; add transient for Emacs profiler
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
(global-set-key (kbd "C-c e p") #'transient/profiler)

;; add transient popup for projectile
(with-eval-after-load 'projectile
  (require 'org-projectile)
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
      ]
     ["Buffer"
      ("b" "Switchb" projectile-switch-to-buffer)
      ("<left>" "Previous" projectile-previous-project-buffer :transient t)
      ("<right>" "Next" projectile-next-project-buffer :transient t)
      ("I" "Ibuffer" projectile-ibuffer)
      ("S" "Save" projectile-save-project-buffers)
      ("k" "Kill" projectile-kill-buffers)
      ]
     ["Search"
      ("o" "Occur" projectile-multi-occur)
      ("s" "Grep" projectile-grep)
      ("r" "Replace" projectile-replace)
      ]
     ["Tags"
      ("j" "Find tag" projectile-find-tag)
      ("R" "Regen tags" projectile-regenerate-tags)
      ]
     ]
    [
     ["File"
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
      ]
     ["Other"
      ("m" "Commander" projectile-commander)
      ("p" "Switch project" projectile-switch-project)
      ("K" "Add project TODO" org-projectile-project-todo-completing-read)
      ("T" "Goto project TODOs" org-projectile-goto-location-for-project)
      ]
     ]
    )
  (define-key projectile-mode-map (kbd "C-c p") #'transient/projectile))

;; add transient popup for register commands
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
   [("DEL" "Clear" my-clear-register)
    ("i" "Insert" insert-register)
    ("l" "List" list-registers)
    ("v" "View" view-register)
    ]
   ]
  )
(global-set-key (kbd "C-c R") #'transient/registers)

;; add transient popup for search tools
(defun transient/search--rg-menu-or-rgrep ()
  "Dispatch to `rg-menu' if command available, else `rgrep'."
  (interactive)
  (if (and (executable-find "rg")
           (fboundp 'rg-menu))
      (call-interactively #'rg-menu)
    (call-interactively #'rgrep)))
(transient-define-prefix transient/search ()
  "Search commands."
  ["Search"
   ["Grep"
    ("gr" "Recursive" transient/search--rg-menu-or-rgrep)
    ("gz" "Recursive (*.gz)" rzgrep)
    ("gg" "With user args" grep)
    ("gf" "Via find" grep-find)
    ]
   ["Occur in buffers"
    ("oo" "Current" occur)
    ("ob" "Matching" multi-occur-in-matching-buffers)
    ("om" "All" multi-occur)
    ]
   ["Query/Replace"
    ("rs" "String" query-replace)
    ("rr" "Regexp" query-replace-regexp)
    ]
   ["Other"
    ("." "Find definition" xref-find-definitions)
    ("w" "EWW web search" eww)
    ]
   ]
  )
(global-set-key (kbd "C-c S") #'transient/search)

;; add transient popup for Emacs server management
(transient-define-prefix transient/server ()
  "Emacs server-related commands."
  ;; suffix actions don't exit the transient popup by default
  :transient-suffix 'transient--do-stay
  [:description (lambda ()
                  (transient--make-description
                   "Emacs server-mode"
                   server-mode))
   ("s" "Toggle server-mode" server-mode)
   ("r" "Restart server" restart-emacs-server)
   ("e" "Next server editing buffer" server-edit)
   ("k" "Stop server and delete connection file" server-force-delete)
   ]
  )
(global-set-key (kbd "C-c e s") #'transient/server)

;; add transient popup for shell tools
(transient-define-prefix transient/shell ()
  "Various shell tools."
  ["Shell tools"
   ["Shell"
    ("e" "Eshell" my-eshell-with-name)
    ("a" "ANSI Term" ansi-term)
    ]
   ["Tmux"
    ("ts" "Send" tmux-send)
    ("tr" "Resend" tmux-resend)
    ]
   ]
  )
(global-set-key (kbd "C-c T") #'transient/shell)

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

(defun transient/system--regenerate-my-exec-path-envs-cache ()
  "Regenerate `my-exec-path-envs-cache-file' and reload env vars."
  (interactive)
  (my-exec-path-from-shell-initialize t))

;; add transient popup for system process management and info, and
;; Emacs build and runtime info
(transient-define-prefix transient/system ()
  "System process managment, general info and Emacs runtime commands."
  ["System, process and Emacs runtime"
   ["Emacs"
    ("eb" "Build config" transient/system--display-emacs-build-config)
    ("ei" "Init time" emacs-init-time)
    ("ep" "Emacs PID" transient/system--display-emacs-pid)
    ("eu" "Uptime" emacs-uptime)
    ("ev" "Version" emacs-version)
    ("eV" "Regen exec-path-envs cache" transient/system--regenerate-my-exec-path-envs-cache)
    ]
   ["System"
    ("sp" "Proced" proced)
    ("st" "Datetime" transient/system--display-current-datetime :transient t)
    ("sw" "World time" display-time-world)
    ]
   ]
  )
(global-set-key (kbd "C-c e S") #'transient/system)

;; make sure functions used by visual transient are loaded
(require 'follow)
(require 'hilit-chg)
(require 'hl-line)
(require 'display-line-numbers)
(require 'face-remap)
(require 'whitespace)

(require 'censor)
(require 'olivetti)
(require 'highlight-indent-guides)
(require 'hl-todo)
(require 'prism)
(require 'too-long-lines-mode)

(defvar-local transient/visual--face-remap-cookies '()
  "Alist storing cookies for `face-remap-add-relative' calls.")

(defun transient/visual--toggle-lighten-face (face)
  "Toggle brightness of FACE color for emphasis or emphasis."
  (let ((face-remap-cookie-old (alist-get
                                face
                                transient/visual--face-remap-cookies)))
    (if face-remap-cookie-old
        (progn
          (face-remap-remove-relative face-remap-cookie-old)
          (setq transient/visual--face-remap-cookies
                (assq-delete-all
                 face
                 transient/visual--face-remap-cookies)))
      (let* ((light-color (color-lighten-name
                           (face-attribute face :foreground)
                           50)) ;; lighten color by 50 percent
             (face-remap-cookie-new (face-remap-add-relative
                                     face
                                     :foreground light-color)))
        (push `(,face . ,face-remap-cookie-new)
              transient/visual--face-remap-cookies)))))

(defun transient/visual--toggle-lighten-font-lock-comment-face ()
  "Toggle brightness of `font-lock-comment-face'."
  (interactive)
  (transient/visual--toggle-lighten-face
   'font-lock-comment-face))

(defun transient/visual--toggle-lighten-font-lock-comment-delimiter-face ()
  "Toggle brightness of `font-lock-comment-delimiter-face'."
  (interactive)
  (transient/visual--toggle-lighten-face
   'font-lock-comment-delimiter-face))

(defun transient/visual--toggle-lighten-font-lock-doc-face ()
  "Toggle brightness of `font-lock-doc-face'."
  (interactive)
  (transient/visual--toggle-lighten-face
   'font-lock-doc-face))

(defun transient/visual--display-toggle-trailing-whitespace ()
  "Toggle the display of trailing whitespace."
  (interactive)
  ;; `show-trailing-whitespace' is buffer-local by default
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "show-trailing-whitespace: %s"
           (if show-trailing-whitespace "yes" "no")))

(defun transient/visual--toggle-ligatures ()
  "Toggle ligatures.
Currently only works for Emacs Mac port."
  (interactive)
  (cond ((fboundp 'mac-auto-operator-composition-mode)
         (mac-auto-operator-composition-mode))
        (t (message "Not implemented for this Emacs build."))))

(defun transient/visual--text-scale-reset ()
  "Resets buffer `text-scale-mode-amount' to zero."
  (interactive)
  (text-scale-set 0))

(defun transient/visual--my-pulse-line ()
  "Pulse the point's current line using `my-pulse-line'."
  (interactive)
  (my-pulse-line))

;; add transient popup for visual commands
(transient-define-prefix transient/visual ()
  "Visual commands and toggles."
  :transient-suffix 'transient--do-stay
  ["Visual"
   ["Display"
    ("H" (lambda ()
           (transient--make-description
            "Highlight changes"
            highlight-changes-mode))
     highlight-changes-mode)
    ("i" (lambda ()
           (transient--make-description
            "Indent guides"
            highlight-indent-guides-mode))
     highlight-indent-guides-mode)
    ("l" (lambda ()
           (transient--make-description
            "Line numbers"
            display-line-numbers-mode))
     display-line-numbers-mode)
    ("t" (lambda ()
           (transient--make-description
            "Truncate lines"
            truncate-lines))
     toggle-truncate-lines)
    ("w" (lambda ()
           (transient--make-description
            "Trailing whitespace"
            show-trailing-whitespace))
     transient/visual--display-toggle-trailing-whitespace)
    ("W" (lambda ()
           (transient--make-description
            "Whitespace"
            whitespace-mode))
     whitespace-mode)
    ("L" (lambda ()
           (transient--make-description
            "Hide long lines"
            too-long-lines-mode))
     too-long-lines-mode)
    ("$" (lambda ()
           (concat "Selective display ["
                   (if selective-display
                       (number-to-string selective-display)
                     " ")
                   "]"))
     set-selective-display)
    ("P" (lambda ()
           (transient--make-description
            "Prettify symbols"
            prettify-symbols-mode))
     prettify-symbols-mode)
    ("B" (lambda ()
           (transient--make-description
            "Buffer-specific face"
            buffer-face-mode))
     buffer-face-mode)
    ("C-l" "Ligatures" transient/visual--toggle-ligatures)
    ]
   ["Cursor"
    ("b" (lambda ()
           (transient--make-description
            "Blink"
            blink-cursor-mode))
     blink-cursor-mode)
    ("h" (lambda ()
           (transient--make-description
            "Highlight line"
            hl-line-mode))
     hl-line-mode)
    ("p" (lambda ()
           (transient--make-description
            "Show paren"
            show-paren-mode))
     show-paren-mode)
    ("T" (lambda ()
           (transient--make-description
            "Transient mark"
            transient-mark-mode))
     transient-mark-mode)
    ]
   ["Color"
    ("cf" (lambda ()
            (transient--make-description
             "Font locking"
             font-lock-mode))
     font-lock-mode)
    ("cc" (lambda ()
            (transient--make-description
             "Comments"
             (null (assq 'font-lock-comment-face
                         transient/visual--face-remap-cookies))))
     transient/visual--toggle-lighten-font-lock-comment-face)
    ("cC" (lambda ()
            (transient--make-description
             "Comment delims"
             (null (assq 'font-lock-comment-delimiter-face
                         transient/visual--face-remap-cookies))))
     transient/visual--toggle-lighten-font-lock-comment-delimiter-face)
    ("cd" (lambda ()
            (transient--make-description
             "Docstrings"
             (null (assq 'font-lock-doc-face
                         transient/visual--face-remap-cookies))))
     transient/visual--toggle-lighten-font-lock-doc-face)
    ("cp" (lambda ()
            (transient--make-description
             "Prism"
             prism-mode))
     prism-mode)
    ("cP" (lambda ()
            (transient--make-description
             "Prism whitespace"
             prism-whitespace-mode))
     prism-whitespace-mode)
    ("cT" (lambda ()
             (transient--make-description
              "Highlight TODOs"
              hl-todo-mode))
     hl-todo-mode)
    ]
   ]
  [
   [:description (lambda ()
                   (transient--make-description
                    "Narrow"
                    (buffer-narrowed-p)))
    ("nd" "Defun" narrow-to-defun)
    ("nr" "Region" narrow-to-region)
    ("np" "Page" narrow-to-page)
    ("nw" "Widen" widen)
    ]
   [:description (lambda ()
                   (concat "Zoom ["
                           (if text-scale-mode
                               (number-to-string text-scale-mode-amount)
                             " ")
                           "]"))
    ("+" "Increase" text-scale-increase)
    ("-" "Decrease" text-scale-decrease)
    ("0" "Reset" transient/visual--text-scale-reset)
    ]
   ["Layout"
    ("m" (lambda ()
           (transient--make-description
            "Menu bar"
            menu-bar-mode))
     menu-bar-mode)
    ("s" (lambda ()
           (transient--make-description
            "Vscroll bar"
            (frame-parameter nil 'vertical-scroll-bars)))
     toggle-scroll-bar)
    ("S" (lambda ()
           (transient--make-description
            "Hscroll bar"
            (frame-parameter nil 'horizontal-scroll-bars)))
     toggle-horizontal-scroll-bar)
    ("o" (lambda ()
           (transient--make-description
            "Olivetti"
            olivetti-mode))
     olivetti-mode)
    ]
   ["Other"
    ("C" (lambda ()
           (transient--make-description
            "Scroll lock"
            (and (boundp 'scroll-lock-mode)
                 scroll-lock-mode)))
     scroll-lock-mode)
    ("v" (lambda ()
           (transient--make-description
            "Visual line"
            visual-line-mode))
     visual-line-mode)
    ("F" (lambda ()
           (transient--make-description
            "Follow"
            follow-mode))
     follow-mode)
    ("x" (lambda ()
           (transient--make-description
            "Censor"
            censor-mode))
     censor-mode)
    ("X" (lambda ()
           (transient--make-description
            "Censor (global)"
            global-censor-mode))
     global-censor-mode)
    ("SPC" "Pulse line" transient/visual--my-pulse-line)
    ]
   ]
  )
(global-set-key (kbd "C-c l v") #'transient/visual)

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

;; add transient popup for window commands
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
    ("[" "Rotate bwd" my-rotate-buffers-backward)
    ("]" "Rotate fwd" my-rotate-buffers-forward)
    ]
   ["Layout"
    ("0" "Delete window" delete-window)
    ("1" "Delete other windows" delete-other-windows)
    ("2" "Split horiz" split-window-right)
    ("3" "Split vert" split-window-below)
    ("4" "Kill buffer and window" kill-buffer-and-window)
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
(global-set-key (kbd "C-c e w") #'transient/window)

;; add transient popup for workspace commands
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
   ["Tabs"
    ("<backtab>" "Previous" tab-bar-switch-to-prev-tab :transient t)
    ("<tab>" "Next" tab-bar-switch-to-next-tab :transient t)
    ("1" "Only" tab-bar-close-other-tabs :transient t)
    ("2" "New" tab-bar-new-tab :transient t)
    ("0" "Close" tab-bar-close-tab :transient t)
    ("u" "Undo close" tab-bar-undo-close-tab :transient t)
    ("r" "Rename" tab-bar-rename-tab :transient t)
    ("m" "Move" tab-bar-move-tab :transient t)
    ("M" "Move (frame)" tab-bar-move-tab-to-frame :transient t)
    ("<return>" "Switch" tab-bar-switch-to-tab)
    ]
   ]
  )
(global-set-key (kbd "C-c e W") #'transient/workspace)

;; add transient popup for writing commands
(require 'dictionary)
(require 'langtool)
(require 'typo)

(defun transient/writing--ispell-dwim ()
  "Dispatch to different Ispell spelling correction commands by major mode.

If the major mode derives from `prog-mode', call interactively
`ispell-comments-and-strings'.

If the major mode derives from `message-mode', call interactively
`ispell-message'.

Otherwise call interactively `ispell'.

Note that `ispell-comments-and-strings' and `ispell-message' do
not support restricting to a region."
  (interactive)
  (let ((fun (cond
              ((derived-mode-p 'prog-mode) #'ispell-comments-and-strings)
              ((derived-mode-p 'message-mode) #'ispell-message)
              (t #'ispell))))
    (call-interactively fun)))

(transient-define-prefix transient/writing ()
  "Writing commands."
  ["Writing"
   ["Spelling"
    ("F" (lambda ()
           (interactive)
           (transient--make-description "Flyspell mode"
                                        flyspell-mode))
     flyspell-mode :transient t)
    ("P" "Flyspell prog mode" flyspell-prog-mode :transient t)
    ("B" "Check buffer" flyspell-buffer :transient t)
    ("n" "Next error" flyspell-goto-next-error :transient t)
    ("c" "Correct word" ispell-word :transient t)
    ("C" "Correct buffer/region" transient/writing--ispell-dwim :transient t)
    ("D" "Change dictionary" ispell-change-dictionary :transient t)
    ]
   ["LanguageTool"
    ("gs" "Start check" langtool-check)
    ("gc" "Correct buffer" langtool-correct-buffer)
    ("ge" "End check" langtool-check-done)
    ("gl" "Switch language" langtool-switch-default-language
     :transient t)
    ]
   ["Dictionary"
    ("ds" "Search" dictionary-search)
    ("dm" "Match words" dictionary-match-words)
    ]
   ]
  [
   ["Typography"
    ("y" (lambda ()
           (interactive)
           (transient--make-description "Typography mode"
                                        typo-mode))
     typo-mode :transient t)]
   ]
  )

(global-set-key (kbd "C-c l w") #'transient/writing)

;; add transient popup for yasnippet commands
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
   (global-set-key (kbd "C-c Y") #'transient/yasnippet)))

;; Transient commands / Major mode transients

;; major-mode specific transient for clojure-mode
(with-eval-after-load 'clojure-mode
  (with-eval-after-load 'cider
    (transient-define-prefix transient/clojure-mode/eval ()
      "`clojure-mode' CIDER evaluation commands."
      ["CIDER → Run"
       ["Eval"
        ("r" "Region" cider-eval-region)
        ("n" "Namespace form" cider-eval-ns-form)
        ("e" "Last sexp" cider-eval-last-sexp)
        ("P" "Last sexp (pprint)" cider-pprint-eval-last-sexp)
        ("w" "Last sexp replace" cider-eval-last-sexp-and-replace)
        ("E" "Last sexp to REPL" cider-eval-last-sexp-to-repl)
        ("d" "Defun at point" cider-eval-defun-at-point)
        ("f" "Defun at point (pprint)" cider-pprint-eval-defun-at-point)
        (":" "Minibuffer input" cider-read-and-eval)
        ]
       ["Load"
        ("k" "Buffer" cider-load-buffer)
        ("l" "File" cider-load-file)
        ("p" "All proj ns" cider-load-all-project-ns)
        ]
       ["Other"
        ("i" "Inspect" cider-inspect)
        ("m" "Macroexpand (single level)" cider-macroexpand-1)
        ("M" "Macroexpand (all levels)" cider-macroexpand-all)
        ]
       ]
      )

    (transient-define-prefix transient/clojure-mode/test ()
      "`clojure-mode' CIDER testing commands."
      ["CIDER → Test"
       ("t" "Run" cider-test-run-test)
       ("l" "Run loaded" cider-test-run-loaded-tests)
       ("p" "Run project" cider-test-run-project-tests)
       ("n" "Run namespace" cider-test-run-ns-tests)
       ("r" "Rerun failed" cider-test-rerun-failed-tests)
       ("s" "Show report" cider-test-show-report)
       ]
      )

    (transient-define-prefix transient/clojure-mode/help ()
      "`clojure-mode' CIDER help/documentation commands."
      ["CIDER → Help"
       ("d" "CIDER docs" cider-doc)
       ("c" "Clojure docs" cider-clojuredocs)
       ("C" "Clojure docs (web)" cider-clojuredocs-web)
       ("j" "Java docs (web)" cider-javadoc)
       ("a" "Search symbols" cider-apropos)
       ("s" "Select symbols" cider-apropos-select)
       ("A" "Search docs" cider-apropos-documentation)
       ("S" "Select docs" cider-apropos-documentation-select)
       ]
      )

    (defun transient/clojure-mode/debug--eval-defun-at-point ()
      "Debug version of `cider-eval-defun-at-point'."
      (interactive)
      (cider-eval-defun-at-point t))

    (transient-define-prefix transient/clojure-mode/debug ()
      "`clojure-mode' CIDER debug/documentation commands."
      ["CIDER → Debug"
       ("x" "Eval at point" transient/clojure-mode/debug--eval-defun-at-point)
       ("v" "Toggle trace variable" cider-toggle-trace-var)
       ("n" "Toggle trace namespace" cider-toggle-trace-ns)
       ]
      )

    (defun transient/clojure-mode/repl--clear-output-all ()
      "Clear all output in CIDER REPL buffer."
      (interactive)
      (cider-find-and-clear-repl-output t))

    (transient-define-prefix transient/clojure-mode/repl ()
      "`clojure-mode' CIDER REPL commands."
      ["CIDER → REPL"
       ["Input"
        ("z" "Switch to buffer" cider-switch-to-repl-buffer)
        ("n" "Set namespace" cider-repl-set-ns)
        ("p" "Insert last sexp" cider-insert-last-sexp-in-repl)
        ("x" "Refresh" cider-refresh)
        ]
       ["Output"
        ("o" "Clear" cider-find-and-clear-repl-output)
        ("O" "Clear all" transient/clojure-mode/repl--clear-output-all)
        ]
       [
        "Other"
        ("d" "Display conn info" cider-display-connection-info)
        ("b" "Interrupt" cider-interrupt)
        ("Q" "Quit CIDER" cider-quit)
        ]
       ]
      )

    (transient-define-prefix transient/clojure-mode ()
      "`clojure-mode' CIDER commands."
      ["CIDER"
       ["Session"
        ("jc" "Jack-in (Clojure)" cider-jack-in-clj)
        ("js" "Jack-in (ClojureScript)" cider-jack-in-cljs)
        ("jb" "Jack-in (Both)" cider-jack-in-clj&cljs)
        ]
       ["Submenus"
        ("r" "→ REPL" transient/clojure-mode/repl)
        ("e" "→ Run" transient/clojure-mode/eval)
        ("t" "→ Test" transient/clojure-mode/test)
        ("d" "→ Debug" transient/clojure-mode/debug)
        ("h" "→ Help" transient/clojure-mode/help)
        ]
       ]
      )

    (define-key clojure-mode-map (kbd "C-c m") #'transient/clojure-mode)))

;; major-mode specific transient for csv-mode
(with-eval-after-load 'csv-mode
  (transient-define-prefix transient/csv-mode ()
    "`csv-mode' commands."
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
  (define-key csv-mode-map (kbd "C-c m") #'transient/csv-mode))

;; major-mode specific transient for debugger-mode
(with-eval-after-load 'debug
  (transient-define-prefix transient/debugger-mode ()
    "`debugger-mode' commands."
    ["Emacs debugger"
     ["Movement"
      ("n" "Next line" next-line :transient t)
      ("p" "Previous line" previous-line :transient t)
      ]
     ["Breakpoints"
      ("b" "Set" debugger-frame)
      ("u" "Unset" debugger-frame-clear)
      ]
     ["Evaluate"
      ("e" "Sexp" debugger-eval-expression)
      ("R" "Sexp and record" debugger-record-expression)
      ]
     ]
    [
     [
      "Stepping"
      ("d" "Step through" debugger-step-through)
      ("c" "Continue" debugger-continue)
      ("j" "Jump" debugger-jump)
      ("q" "Exit" top-level)
      ]
     ["Other"
      ("RET" "Follow at point" backtrace-help-follow-symbol)
      ("r" "Specify return value" debugger-return-value)
      ("l" "List debug functions" debugger-list-functions)
      ("v" "Toggle locals" backtrace-toggle-locals)
      ("h" "Help" describe-mode)
      ]
     ]
    )
  (define-key debugger-mode-map (kbd "C-c m") #'transient/debugger-mode))

;; major-mode specific transient for dired-mode
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
      "`dired-mode' Dired-Filter commands."
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
      "`dired-mode' commands."
      ["Dired"
       ["File open"
        ("RET" "Open" dired-find-file)
        ("o" "Open other" dired-find-file-other-window)
        ("F" "Open marked" dired-do-find-marked-files)
        ("z" "Open external" dired--open-file-at-pt)
        ("v" "View file" dired-view-file)
        ("+" "Create dir" dired-create-directory)
        ("=" "Diff" dired-diff)
        ]
       ["File operations"
        ("C" "Copy" dired-do-copy)
        ("D" "Delete" dired-do-delete)
        ("x" "Delete marked" dired-do-flagged-delete)
        ("S" "Symlink" dired-do-symlink)
        ("Y" "Symlink to" dired-do-relsymlink)
        ("c" "Compress to" dired-do-compress-to)
        ("Z" "Compress" dired-do-compress)
        ]
       ["File modification"
        ("R" "Rename" dired-do-rename)
        ("%R" "Rename by regexp" dired-do-rename-regexp)
        ("G" "chgrp" dired-do-chgrp)
        ("M" "chmod" dired-do-chmod)
        ("O" "chown" dired-do-chown)
        ("T" "Touch" dired-do-touch)
        ]
       ["Mark"
        ("m" "File at pt" dired-mark :transient t)
        ("E" "By extension" dired-mark-extension :transient t)
        ("t" "Toggle marks" dired-toggle-marks :transient t)
        ("u" "Unmark" dired-unmark :transient t)
        ("U" "Unmark all" dired-unmark-all-marks :transient t)
        ]
       ]
      [
       ["Search/Filter"
        ("A" "Query" dired-do-find-regexp)
        ("Q" "Query-replace" dired-do-find-regexp-and-replace)
        ("{" "Find by name" find-name-dired)
        ("}" "Find by query" find-grep-dired)
        ("/" "Filter" transient/dired-mode/filter)
        ]
       ["View"
        ("(" "Toggle details" dired-hide-details-mode :transient t)
        (")" "Toggle omit" dired-omit-mode :transient t)
        ("i" "Insert subdir" dired-maybe-insert-subdir :transient t)
        ("K" "Kill subdir" transient/dired-mode--dired-kill-and-next-subdir :transient t)
        ("s" "Sort by date" dired-sort-toggle-or-edit :transient t)
        ]
       ["Other"
        ("y" "Show file type" dired-show-file-type :transient t)
        ("g" "Refresh" revert-buffer :transient t)
        ("l" "Redisplay" dired-do-redisplay :transient t)
        ("C-o" "Display other" dired-display-file)
        ("h" "Help" describe-mode)
        ]
       ]
      )
    (define-key dired-mode-map (kbd "C-c m") #'transient/dired-mode)))

;; major-mode specific transient for edebug-mode
(with-eval-after-load 'edebug
  (transient-define-prefix transient/edebug-mode ()
    "`edebug-mode' commands."
    ["Edebug"
     ["Modes"
      ("SPC" "Step" edebug-step-mode)
      ("n" "Next" edebug-next-mode)
      ("g" "Go" edebug-go-mode)
      ("G" "Go (nonstop)" edebug-Go-nonstop-mode)
      ("t" "Trace" edebug-Trace-fast-mode)
      ("c" "Continue" edebug-continue-mode)
      ("C" "Continue (fast)" edebug-Continue-fast-mode)
      ]
     ["Stepping"
      ("f" "Forward sexp" edebug-forward-sexp)
      ("h" "Continue to here" edebug-goto-here)
      ("I" "Instrument callee" edebug-instrument-callee)
      ("i" "Step in" edebug-step-in)
      ("o" "Step out" edebug-step-out)
      ]
     ["Breakpoints"
      ("b" "Set" edebug-set-breakpoint)
      ("u" "Unset" edebug-unset-breakpoint)
      ("B" "Next" edebug-next-breakpoint)
      ("x" "Set (cond-at-pt)" edebug-set-conditional-breakpoint)
      ("X" "Set (global cond)" edebug-set-global-break-condition)
      ]
     ]
    [
     ["Evaluation"
      ("r" "Previous result" edebug-previous-result)
      ("e" "Sexp" edebug-eval-expression)
      ("C-e" "Last sexp" edebug-eval-last-sexp)
      ("E" "Visit eval list" edebug-visit-eval-list)
      ]
     ["Views"
      ("v" "Outside" edebug-view-outside)
      ("w" "Where" edebug-where)
      ("p" "Bounce point" edebug-bounce-point)
      ("W" "Toggle save windows" edebug-toggle-save-windows)
      ]
     ["Quitting/Stopping"
      ("q" "Top level" top-level)
      ("Q" "Top level (nonstop)" edebug-top-level-nonstop)
      ("a" "Abort recursive edit" abort-recursive-edit)
      ("S" "Stop" edebug-stop)
      ]
     ]
    [
     ["Other"
      ("d" "Backtrace" edebug-pop-to-backtrace)
      ("=" "Frequency count" edebug-temp-display-freq-count)
      ("?" "Help" edebug-help)
      ]
     ]
    )
  (define-key edebug-mode-map (kbd "C-c m") #'transient/edebug-mode))

;; major-mode specific transient for ess-r-mode
(with-eval-after-load 'ess-r-mode
  (require 'ess-view-data)
  (require 'ess-r-insert-obj)
  (transient-define-prefix transient/ess-r-mode ()
    "`ess-r-mode' commands."
    ["Emacs Speaks Statistics"
     ["Session"
      ("N" "New" R)
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
      ("v" "View data" ess-view-data-print)
      ]
     ["Insert"
      ("if" "Dataframe name" ess-r-insert-obj-dt-name)
      ("ic" "Column name" ess-r-insert-obj-col-name)
      ("iC" "Column name (all)" ess-r-insert-obj-col-name-all)
      ("iv" "Column value" ess-r-insert-obj-value)
      ("iV" "Column value (all)" ess-r-insert-obj-value-all)
      ]
     ["Help"
      ("h" "Object" ess-display-help-on-object)
      ("A" "Apropos" ess-display-help-apropos)
      ("H" "Browser" ess-display-help-in-browser)
      ]
     ]
    [
     ["Format"
      ("y" "Region or buffer" ess-r-styler-format-buffer-or-region)
      ("Y" (lambda ()
             (interactive)
             (transient--make-description
              "Buffer on save"
              ess-r-styler-format-on-save-mode))
       ess-r-styler-format-on-save-mode :transient t)
      ]
     ]
    )
  (define-key ess-r-mode-map (kbd "C-c m") #'transient/ess-r-mode))

;; major-mode specific transient for eww-mode
(with-eval-after-load 'eww
  (transient-define-prefix transient/eww-mode ()
    "`eww-mode' commands."
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
      ]
     ["Toggle"
      ("F" "Fonts" eww-toggle-fonts :transient t)
      ("I" "Images" eww--toggle-images :transient t)
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
  (define-key eww-mode-map (kbd "C-c m") #'transient/eww-mode))

(transient-define-prefix transient/ibuffer-mode/mark ()
  "`ibuffer-mode' mark commands."
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
  "`ibuffer-mode' action commands."
  ["Ibuffer → Action"
   ["Run"
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
    ]
   ["Properties"
    ("R" "Rename uniquely" ibuffer-do-rename-uniquely)
    ("M" "Toggle modified" ibuffer-do-toggle-modified)
    ("T" "Toggle read-only" ibuffer-do-toggle-read-only)
    ]
   ]
  [
   ["Other"
    ("A" "View" ibuffer-do-view)
    ("H" "View (other)" ibuffer-do-view-other-frame)
    ("V" "Revert" ibuffer-do-revert)
    ("P" "Print" ibuffer-do-print)
    ]
   ]
  )

(transient-define-prefix transient/ibuffer-mode/sort ()
  "`ibuffer-mode' sort commands."
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
  "`ibuffer-mode' filter commands."
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
  "`ibuffer-mode' commands."
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
(define-key ibuffer-mode-map (kbd "C-c m") #'transient/ibuffer-mode)

;; major-mode specific transient for json-mode
(with-eval-after-load 'json-mode
  (transient-define-prefix transient/json-mode ()
    "`json-mode' commands."
    :transient-suffix 'transient--do-stay
    ["JSON mode"
     ("p" "Copy path" json-mode-show-path)
     ("b" "Toggle bool value" json-toggle-boolean)
     ("i" "Increment num" json-increment-number-at-point)
     ("d" "Decrement num" json-decrement-number-at-point)
     ("k" "Nullify sexp" json-nullify-sexp)
     ("B" "Beautify" json-mode-beautify)
     ("R" "Reformat (jq)" json-jq-format-buffer-or-region)
     ]
    )
  (define-key json-mode-map (kbd "C-c m") #'transient/json-mode))

;; major-mode specific transient for launchctl-mode
(when (eq system-type 'darwin)
  (with-eval-after-load 'launchctl
    (transient-define-prefix transient/launchctl-mode ()
      "`launchctl-mode' commands."
      ["launchctl mode"
       ["Config"
        ("n" "New" launchctl-new)
        ("e" "Edit" launchctl-edit)
        ("v" "View" launchctl-view)
        ]
       ["Service"
        ("l" "Load" launchctl-load)
        ("u" "Unload" launchctl-unload)
        ("r" "Reload" launchctl-reload)
        ("s" "Start" launchctl-start)
        ("o" "Stop" launchctl-stop)
        ("m" "Remove" launchctl-remove)
        ("d" "Disable" launchctl-disable)
        ("p" "Enable" launchctl-enable)
        ("i" "Info" launchctl-info)
        ]
       ["Other"
        ("g" "Refresh" launchctl-refresh)
        ("t" "Sort" tabulated-list-sort)
        ("*" "Filter" launchctl-filter)
        ("$" "Setenv" launchctl-setenv)
        ("#" "Unsetenv" launchctl-unsetenv)
        ("h" "Help" launchctl-help)
        ]
       ]
      )
    (define-key launchctl-mode-map (kbd "C-c m") #'transient/launchctl-mode)))

;; major-mode specific transient for markdown-mode
(with-eval-after-load 'markdown-mode
  (with-eval-after-load 'markdown-toc
    (transient-define-prefix transient/markdown-mode ()
      "`markdown-mode' commands."
      :transient-suffix 'transient--do-stay
      ["Markdown mode (other bindings: 'C-c C-c', 'C-c C-s', 'C-c C-x')"
       ["Navigate"
        ("n" "Next" markdown-outline-next)
        ("p" "Previous" markdown-outline-previous)
        ("f" "Next (same level)" markdown-outline-next-same-level)
        ("b" "Previous (same level)" markdown-outline-previous-same-level)
        ]
       ["Move outline"
        ("<left>" "Promote" markdown-promote)
        ("<right>" "Demote" markdown-demote)
        ("<up>" "Move up" markdown-move-up)
        ("<down>" "Move down" markdown-move-down)
        ]
       ["Shift region"
        ("<" "Outdent" markdown-outdent-region)
        (">" "Indent" markdown-indent-region)
        ]
       ]
      [
       ["User interface"
        ("E" "Toggle math" markdown-toggle-math)
        ("F" "Toggle code font" markdown-toggle-fontify-code-blocks-natively)
        ("I" "Toggle images" markdown-toggle-inline-images)
        ("L" "Toggle show URL" markdown-toggle-url-hiding)
        ("M" "Toggle show markup" markdown-toggle-markup-hiding)
        ]
       ["Table of contents"
        ("t" "Insert/Refresh" markdown-toc-generate-or-refresh-toc :transient nil)
        ("C-t" "Delete" markdown-toc-delete-toc)
        ]
       ["Other"
        ("d" "Do" markdown-do :transient nil)
        ("o" "Follow" markdown-follow-thing-at-point :transient nil)
        ("N" "Cleanup list nums" markdown-cleanup-list-numbers :transient nil)
        ("'" "Edit code block" markdown-edit-code-block :transient nil)
        ]
       ]
      )
    (define-key gfm-mode-map (kbd "C-c m") #'transient/markdown-mode)
    (define-key markdown-mode-map (kbd "C-c m") #'transient/markdown-mode)))

;; major-mode specific transient for org-agenda-mode
(with-eval-after-load 'org-agenda
  (defun transient/org-agenda-mode--hide-done ()
    "Hide items with DONE state in `org-agenda-mode' buffer."
    (interactive)
    (setq org-agenda-skip-scheduled-if-done
          (not org-agenda-skip-scheduled-if-done))
    (org-agenda-redo-all t))

  (transient-define-prefix transient/org-agenda-mode ()
    "`org-agenda-mode' commands."
    :transient-suffix 'transient--do-stay
    ["Org agenda"
     ["Agenda view"
      ("d" "Day" org-agenda-day-view)
      ("w" "Week" org-agenda-week-view)
      ("f" "Later" org-agenda-later)
      ("b" "Earlier" org-agenda-earlier)
      ]
     ["Navigate"
      ("n" "Next line" org-agenda-next-line)
      ("p" "Prev line" org-agenda-previous-line)
      ("N" "Next item" org-agenda-next-item)
      ("P" "Prev item" org-agenda-previous-item)
      ]
     ["Visit"
      ("SPC" "Show" org-agenda-show-and-scroll-up :transient nil)
      ("TAB" "Goto" org-agenda-goto :transient nil)
      ("RET" "Switch to" org-agenda-switch-to :transient nil)
      ("C-c C-o" "Link" org-agenda-open-link :transient nil)
      ]
     ["Other"
      ("r" "Redisplay" org-agenda-redo)
      ("j" "Goto date" org-agenda-goto-date)
      ("." "Goto today" org-agenda-goto-today)
      ("(" (lambda ()
             (transient--make-description
              "Hide DONE"
              org-agenda-skip-scheduled-if-done))
       transient/org-agenda-mode--hide-done)
      ]
     ]
    [
     ["Filter"
      ("<" "By category" org-agenda-filter-by-category)
      ("_" "By effort" org-agenda-filter-by-effort)
      ("=" "By regexp" org-agenda-filter-by-regexp)
      ("\\" "By tag" org-agenda-filter-by-tag)
      ("^" "By top headline" org-agenda-filter-by-top-headline)
      ("|" "Remove all" org-agenda-filter-remove-all)
      ]
     ["Clock"
      ("I" "In" org-agenda-clock-in)
      ("O" "Out" org-agenda-clock-out)
      ("X" "Cancel" org-agenda-clock-cancel)
      ("J" "Current task" org-agenda-clock-goto)
      ("R" (lambda ()
             (transient--make-description
              "Clocktable"
              org-agenda-clockreport-mode))
       org-agenda-clockreport-mode)
      ]
     ["Modify"
      ("t" "Status" org-agenda-todo)
      (":" "Tags" org-agenda-set-tags)
      ("," "Priority" org-agenda-priority)
      ("z" "Add note" org-agenda-add-note)
      ("C-c C-x p" "Property" org-agenda-set-property)
      ]
     ]
    [
     ["Date"
      (">" "Prompt" org-agenda-date-prompt)
      ("C-c C-s" "Schedule" org-agenda-schedule)
      ("C-c C-d" "Deadline" org-agenda-deadline)
      ]
     ["Node ops"
      ("$" "Archive" org-agenda-archive)
      ("C-c C-w" "Refile" org-agenda-refile)
      ("C-k" "Kill" org-agenda-kill)
      ]
     ]
    )

  (define-key org-agenda-mode-map (kbd "C-c m") #'transient/org-agenda-mode))

;; major-mode specific transient for org-mode
(with-eval-after-load 'org
  (require 'org-download)
  (require 'org-readitlater)
  (require 'org-tree-slide)
  (defun transient/org-mode--toggle-display-image-width ()
    "Toggle resizing of inline images in `org-mode' to one-third screen width."
    (interactive)
    (if org-image-actual-width
        (setq org-image-actual-width nil)
      (setq org-image-actual-width (list (/ (display-pixel-width) 3))))
    (org-redisplay-inline-images))

  (defun transient/org-mode--next-heading-dwim (n)
    "Go to N-th next occur highlight or visible heading otherwise."
    (interactive "p")
    (if org-occur-highlights
        (next-error n)
      (org-next-visible-heading n)))

  (defun transient/org-mode--previous-heading-dwim (n)
    "Go to N-th previous occur highlight or visible heading otherwise."
    (interactive "p")
    (if org-occur-highlights
        (previous-error n)
      (org-previous-visible-heading n)))

  (transient-define-prefix transient/org-mode/readitlater ()
    "org-readitlater commands."
    ["Org → Read-it-later"
     ("a" "Archive" org-readitlater-archive)
     ("r" "Dry run" org-board-archive-dry-run)
     ("n" "New entry" org-readitlater-new)
     ("k" "Delete archives" org-readitlater-delete-all)
     ("o" "Open archive" org-readitlater-open)
     ("d" "Diff" org-readitlater-diff)
     ("3" "Diff3" org-readitlater-diff3)
     ("c" "Cancel in-progress archive" org-readitlater-cancel)
     ("x" "Run post-archive functions" org-readitlater-run-after-archive-function)
     ("O" "Open archive directory" org-attach-reveal-in-emacs)
     ]
    )

  (transient-define-prefix transient/org-mode ()
    "`org-mode' commands."
    ["Org"
     ["Toggle"
      ("i" (lambda ()
             (transient--make-description
              "Images"
              org-inline-image-overlays))
       org-toggle-inline-images :transient t)
      ("I" (lambda ()
             (transient--make-description
              "Indent"
              org-indent-mode))
       org-indent-mode :transient t)
      ("P" (lambda ()
             (transient--make-description
              "Prettify entities"
              org-pretty-entities))
       org-toggle-pretty-entities :transient t)
      ("M-l" (lambda ()
               (transient--make-description
                "Link display"
                (not org-link-descriptive)))
       org-toggle-link-display :transient t)
      ("M-i" (lambda ()
               (transient--make-description
                "Image resize"
                org-image-actual-width))
       transient/org-mode--toggle-display-image-width :transient t)
      ]
     ["Search"
      ("g" "Goto" org-goto)
      ("o" "Occur" org-occur :transient t)
      ("/" "Create sparse tree" org-sparse-tree :transient t)
      ("c" "Clear search results" org-remove-occur-highlights :transient t)
      ("n" "Next (sparse) node" transient/org-mode--next-heading-dwim :transient t)
      ("p" "Previous (sparse) node" transient/org-mode--previous-heading-dwim :transient t)
      ]
     ["Modify"
      ("t" "Todo state" org-todo)
      (":" "Tags" org-set-tags-command)
      ("," "Priority" org-priority)
      ("D" "Insert drawer" org-insert-drawer)
      ("P" "Set property" org-set-property)
      ("N" "Add note" org-add-note)
      ]
     ]
    [
     ["Node ops"
      ("a" "Archive" org-archive-subtree-default)
      ("r" "Refile" org-refile)
      ("s" "Sort" org-sort)
      ]
     ["Text ops"
      ("F" "Add footnote" org-footnote-action)
      ("<" "Insert structure" org-insert-structure-template)
      ("'" "Edit special" org-edit-special)
      ("e" "Emphasize" org-emphasize)
      ]
     [:description (lambda ()
                     (transient--make-description
                      "Narrow"
                      (buffer-narrowed-p)))
      ("M-s" "Subtree" org-narrow-to-subtree)
      ("M-b" "Block" org-narrow-to-block)
      ("M-w" "Widen" widen)
      ]
     ["Other"
      ("<tab>" "Cycle node" org-cycle :transient t)
      ("<S-tab>" "Cycle global" org-global-cycle :transient t)
      ("C-p" (lambda ()
               (transient--make-description
                "Slideshow mode"
                org-tree-slide-mode))
       org-tree-slide-mode)
      ("ds" "Download screenshot" org-download-screenshot)
      ("dy" "Download yank" org-download-yank)
      ("R" "→ Read-it-later" transient/org-mode/readitlater)
      ]
     ]
    )
  (define-key org-mode-map (kbd "C-c m") #'transient/org-mode))

;; major-mode specific transient for org-msg-edit-mode
(with-eval-after-load 'org-msg
  (transient-define-prefix transient/org-msg-edit-mode ()
    "`org-msg-edit-mode' commands."
    ["OrgMsg"
     ["Jump to"
      ("f" "From" message-goto-from)
      ("t" "To" message-goto-to)
      ("c" "Cc" message-goto-cc)
      ("B" "Bcc" message-goto-bcc)
      ("F" "Fcc" message-goto-fcc)
      ("S" "Subject" message-goto-subject)
      ("b" "Body" org-msg-goto-body)
      ]
     ["Action"
      ("C-a" "Manage attachments" org-msg-attach)
      ("C-e" "Preview" org-msg-preview)
      ("C-c" "Send" org-ctrl-c-ctrl-c)
      ("C-k" "Kill" org-msg-edit-kill-buffer)
      ]
     ]
    )
  (define-key org-msg-edit-mode-map (kbd "C-c m") #'transient/org-msg-edit-mode))

;; major-mode specific transient for python-mode
(with-eval-after-load 'python
  ;; technically also depends on reformatter but more correctly
  ;; using it to define `python-black-format-buffer-or-region' and
  ;; `python-black-format-on-save-mode'
  (transient-define-prefix transient/python-mode ()
    "`python-mode' commands."
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
     ["Format"
      ("TAB" "Fill paragraph" python-fill-paragraph :transient t)
      ("<" "Indent left" python-indent-shift-left :transient t)
      (">" "Indent right" python-indent-shift-right :transient t)
      ("y" "Region or buffer" python-black-format-buffer-or-region)
      ("Y" (lambda ()
             (interactive)
             (transient--make-description
              "Buffer on save"
              python-black-format-on-save-mode))
       python-black-format-on-save-mode :transient t)
      ]
     ["Other"
      ("j" "Imenu" imenu)
      ("v" "Check error" python-check)
      ("f" "Symbol quick help" python-eldoc-at-point)
      ("d" "Symbol describe" python-describe-at-point)
      ("D" "Python debugger" pdb)
      ]
     ]
    )
  (define-key python-mode-map (kbd "C-c m") #'transient/python-mode))

;; major-mode specific transient for racket-mode
(with-eval-after-load 'racket-mode
  (defun transient/racket-mode--visit-definition ()
    "Visits definition of identifier at point in `racket-mode' buffers.
Uses `racket-xp-visit-definition' if `racket-xp-mode' is enabled,
and `racket-repl-visit-definition' otherwise."
    (interactive)
    (if racket-xp-mode
        (racket-xp-visit-definition)
      (racket-repl-visit-definition)))

  (defun transient/racket-mode--describe ()
    "Describe identifier at point in `racket-mode' buffers.
Uses `racket-xp-describe' if `racket-xp-mode' is enabled, and
`racket-repl-describe' otherwise."
    (interactive)
    (if racket-xp-mode
        (racket-xp-describe)
      (racket-repl-describe)))

  (defun transient/racket-mode--documentation ()
    "Show documentation for identifier at point in `racket-mode' buffers.
Documentation is opened in an external browser.
Uses `racket-xp-documentation' if `racket-xp-mode' is enabled,
and `racket-repl-documentation' otherwise."
    (interactive)
    (if racket-xp-mode
        (racket-xp-documentation)
      (racket-repl-documentation)))

  (transient-define-prefix transient/racket-mode ()
    "`racket-mode' commands."
    ["Racket"
     ["Run"
      ("rr" "Buffer in REPL" racket-run)
      ("rm" "Module in REPL" racket-run-module-at-point)
      ("rR" "File in shell" racket-racket)
      ]
     ["Profiling/Logging"
      ("rp" "Profiler" racket-profile)
      ("rl" "Logger" racket-logger)
      ]
     ["Refactoring"
      ("Rb" "Base requires" racket-base-requires)
      ("Rt" "Tidy requires" racket-tidy-requires)
      ("RT" "Trim requires" racket-trim-requires)
      ]
     ["Editing"
      ("a" "Align" racket-align)
      ("u" "Unalign" racket-unalign)
      ]
     ]
    [
     ["Testing"
      ("tt" "Run tests in REPL" racket-test)
      ("tr" "Raco test" racket-raco-test)
      ]
     ["Help"
      ("." "Visit definition" transient/racket-mode--visit-definition)
      ("C-." "Visit module" racket-visit-module)
      ("," "Unvisit" racket-unvisit)
      ("h" "Describe" transient/racket-mode--describe)
      ("H" "Documentation" transient/racket-mode--documentation)
      ]
     ["Other"
      ("f" "Find collection" racket-find-collection)
      ("p" (lambda ()
             (transient--make-description
              "Paredit mode"
              paredit-mode))
       paredit-mode :transient t)
      ("x" (lambda ()
             (transient--make-description
              "Explain/Explore mode"
              racket-xp-mode))
       racket-xp-mode :transient t)
      ("s" "Recompile racket-mode" racket-mode-start-faster)
      ]
     ]
    )

  (define-key racket-mode-map (kbd "C-c m") #'transient/racket-mode))

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
    "`restclient-mode' commands."
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
  (define-key restclient-mode-map (kbd "C-c m") #'transient/restclient-mode))

;; major-mode specific transient for smerge-mode
(with-eval-after-load 'smerge-mode
  (transient-define-prefix transient/smerge-mode ()
    "`smerge-mode' commands."
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
  (define-key smerge-mode-map (kbd "C-c m") #'transient/smerge-mode))

;; major-mode specific transient for term-mode
(with-eval-after-load 'term
  (defun transient/term-mode--toggle-char-mode-line-mode ()
    "Toggle between `term-char-mode' and `term-line-mode' in `term-mode'."
    (interactive)
    (if (term-in-line-mode)
        (progn (term-char-mode) (message "line → char"))
      (progn (term-line-mode) (message "char → line"))))
  (transient-define-prefix transient/term-mode ()
    "`term-mode' commands."
    ["Term"
     ("m" "Toggle between `term-char-mode' and `term-line-mode'"
      transient/term-mode--toggle-char-mode-line-mode :transient t)
     ]
    )
  (define-key term-mode-map (kbd "C-c m") #'transient/term-mode)
  (define-key term-raw-map (kbd "C-c m") #'transient/term-mode))

;; major-mode specific transient for ztreedir-mode
(with-eval-after-load 'ztree-dir
  (transient-define-prefix transient/ztreedir-mode ()
    "`ztreedir-mode' commands."
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
  (define-key ztreedir-mode-map (kbd "C-c m") #'transient/ztreedir-mode))

;; major-mode specific transient for ztreediff-mode
(with-eval-after-load 'ztree-diff
  (transient-define-prefix transient/ztreediff-mode ()
    "`ztreediff-mode' commands."
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
  (define-key ztreediff-mode-map (kbd "C-c m") #'transient/ztreediff-mode))

;; Transient commands / Minor mode transients

;; add transient for Flymake
(with-eval-after-load 'flymake
  (with-eval-after-load 'flymake-quickdef
    (transient-define-prefix transient/flymake-mode ()
      "`flymake-mode' commands."
      :transient-suffix 'transient--do-stay
      [:description (lambda ()
                      (transient--make-description
                       "Flymake"
                       flymake-mode))
       ["Error"
        ("n" "Next" flymake-goto-next-error)
        ("p" "Previous" flymake-goto-prev-error)
        ("l" "List" my-toggle-flymake-diagnostics)
        ("." "Describe" display-local-help)
        ]
       ["Check"
        ("c" "Start" flymake-start)
        ("k" "Stop" flymake-proc-stop-all-syntax-checks)
        ]
       ["Other"
        ("m" "Toggle mode" flymake-mode)
        ("r" "Reporting backends" flymake-reporting-backends)
        ("d" "Disabled backends" flymake-disabled-backends)
        ("l" "Log" flymake-switch-to-log-buffer)
        ("c" "Compile (no check)" flymake-proc-compile)
        ("D" (lambda ()
               (transient--make-description
                "Devskim"
                (memq 'flymake-devskim-backend flymake-diagnostic-functions)))
         flymake-devskim-toggle)
        ]
       ]
      ))
  (global-set-key (kbd "C-c F") #'transient/flymake-mode))

;; add transient for lsp-mode
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'dap-mode
    (with-eval-after-load 'avy
      (defun transient/lsp-mode--install-server ()
        "Install or reinstall `lsp-mode' server."
        (interactive)
        (lsp-install-server t))
      (transient-define-prefix transient/lsp-mode ()
        "`lsp-mode' session commands."
        ["Language server"
         ["Session"
          ("ss" "Start" lsp)
          ("sr" "Restart" lsp-workspace-restart)
          ("sQ" "Shutdown" lsp-workspace-shutdown) ; should normally be "sq", but using "sQ" to work around transient bug with `transient-bind-q-to-quit'
          ("sd" "Describe" lsp-describe-session)
          ("sD" "Disconnect" lsp-disconnect)
          ]
         ["Toggle"
          ("Tl" (lambda ()
                  (transient--make-description
                   "Lens mode"
                   lsp-lens-mode))
           lsp-lens-mode :transient t)
          ("TL" (lambda ()
                  (transient--make-description
                   "Trace I/O"
                   lsp-print-io))
           lsp-toggle-trace-io :transient t)
          ("Th" (lambda ()
                  (transient--make-description
                   "Symbol highlight"
                   lsp-enable-symbol-highlighting))
           lsp-toggle-symbol-highlight :transient t)
          ("Tb" (lambda ()
                  (transient--make-description
                   "Header breadcrumb"
                   lsp-headerline-breadcrumb-mode))
           lsp-headerline-breadcrumb-mode :transient t)
          ("Ta" (lambda ()
                  (transient--make-description
                   "Modeline code actions"
                   lsp-modeline-code-actions-mode))
           lsp-modeline-code-actions-mode :transient t)
          ("TD" (lambda ()
                  (transient--make-description
                   "Modeline diagnostics"
                   lsp-modeline-diagnostics-mode))
           lsp-modeline-diagnostics-mode :transient t)
          ("Ts" (lambda ()
                  (transient--make-description
                   "Signature auto-activate"
                   lsp-signature-auto-activate))
           lsp-toggle-signature-auto-activate :transient t)
          ("Tf" (lambda ()
                  (transient--make-description
                   "On type formatting"
                   lsp-enable-on-type-formatting))
           lsp-toggle-on-type-formatting :transient t)
          ("TT" (lambda ()
                  (transient--make-description
                   "Treemacs sync"
                   lsp-treemacs-sync-mode))
           lsp-treemacs-sync-mode :transient t)
          ]
         ["Goto"
          ("gg" "Definition" lsp-find-definition)
          ("gr" "References" lsp-find-references)
          ("gi" "Implementation" lsp-find-implementation)
          ("gt" "Type definition" lsp-find-type-definition)
          ("gd" "Declaration" lsp-find-declaration)
          ("gh" "Call hierarchy" lsp-treemacs-call-hierarchy)
          ("ga" "Apropos" xref-find-apropos)
          ("ge" "Errors list" lsp-treemacs-errors-list)
          ]
         ]
        [
         ["Folders"
          ("Fa" "Add" lsp-workspace-folders-add)
          ("Fr" "Remove" lsp-workspace-folders-remove)
          ("Fb" "Blacklist remove" lsp-workspace-blacklist-remove)
          ]
         ["Actions"
          ("aa" "Execute code action" lsp-execute-code-action)
          ("al" "Click lens with Avy" lsp-avy-lens)
          ("ah" "Highlight relevant" lsp-document-highlight)
          ]
         ["Other"
          ("I" "Install server" transient/lsp-mode--install-server)
          ("dd" "DAP debug" dap-debug)
          ("de" "DAP edit template" dap-debug-edit-template)
          ]
         ]
        [
         ["Refactoring"
          ("rr" "Rename" lsp-rename)
          ("ro" "Organize imports" lsp-organize-imports)
          ]
         ["Format"
          ("==" "Buffer" lsp-format-buffer)
          ("=r" "Region" lsp-format-region)
          ]
         ["Help"
          ("hh" "Describe" lsp-describe-thing-at-point)
          ("hs" "Signature" lsp-signature-activate)
          ]
         ]
        )
      (global-set-key (kbd "C-c L") #'transient/lsp-mode))))

(provide 'init)
;;; init.el ends here
