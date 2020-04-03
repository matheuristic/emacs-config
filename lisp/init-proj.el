;;; init-proj.el --- Emacs config project interaction layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Project interaction tooling

;;; Code:

(require 'init-org)
(require 'init-ui-hydra)

;; project interaction library
(use-package projectile
  :delight projectile-mode '(:eval (concat " [" (projectile-project-name) "]"))
  :bind (:map projectile-mode-map
         ("C-c C-M-p" . projectile-command-map) ;; prefix binding for projectile commands
         ("C-c C-M-S-p" . my-hydra/projectile/body))
  :init
  (setq projectile-create-missing-test-files t ;; create a test file if none is found when toggling
        projectile-switch-project-action 'projectile-commander
        projectile-use-git-grep t) ;; use git grep to skip backup, object, and untracked files when in a Git project
  (projectile-mode) ;; enable mode globally
  (defhydra my-hydra/projectile (:color teal :hint nil)
    "
Projectile: %(projectile-project-root)

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
    ("p" projectile-switch-project "switch project")
    ("q" nil "quit" :exit t)))

;; Org TODOs for projectile projects
;; use `org-capture' to capture and store TODOs for the current project
;; in `org-projectile-per-project-filepath' at the project's root directory
(use-package org-projectile
  :after (org projectile)
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")
  (push (org-projectile-project-todo-entry) org-capture-templates))

;; lightweight alternative to emerge/ediff
(use-package smerge-mode
  :ensure nil
  :hook (find-file . (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "^<<<<<<< " nil t)
                           (smerge-mode 1)))))
  :bind (:map smerge-mode-map
         ("C-c C-M-m" . my-hydra/smerge/body))
  :config (defhydra my-hydra/smerge (:color pink :hint nil :post (smerge-auto-leave))
            "
Smerge

Move   _n_   : next          _p_ : prev

Keep   _b_   : base          _u_   : upper         _l_   : lower
       _a_   : all           _RET_ : current

Diff   _<_   : upper/base    _=_   : upper/lower   _>_   : base/lower
       _R_   : refine        _E_   : ediff

Other  _C_   : combine       _r_   : resolve       _k_   : kill current

"
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
            ("k" smerge-kill-current)
            ("q" nil "quit" :exit t)))

(when (executable-find "git")
  ;; Git porcelain
  (use-package magit
    :commands magit-status
    :bind (("C-c C-M-g s" . magit-status)
           :map ibuffer-mode-map
           ("G" . my-ibuffer-magit-status-at-pt))
    :config
    (setq auto-revert-check-vc-info t)
    ;; "G" in Ibuffer calls `magit-status' for file at point using `ibuffer-vc'
    ;; adapted from https://www.manueluberti.eu/emacs/2019/08/06/ibuffer-magit/
    (defun my-ibuffer-magit-status-at-pt ()
      "Call `magit-status' for the buffer at point while in Ibuffer."
      (interactive)
      (condition-case nil
          (progn
            (require 'ibuffer-vc)
            (let ((buf (ibuffer-current-buffer t)))
              (magit-status (cdr (ibuffer-vc-root buf)))))
        (message "requires the `ibuffer-vc' package be installed."))))

  ;; Browse historic versions of Git-controlled files
  (use-package git-timemachine
    :commands git-timemachine
    :bind ("C-c C-M-g t" . git-timemachine))

  ;; "I" in Magit opens an interface to manage git identity
  (use-package git-identity
    :after magit
    :bind (:map magit-status-mode-map
           ("I" . git-identity-info))
    :config (git-identity-magit-mode 1)))

(provide 'init-proj)

;;; init-proj.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
