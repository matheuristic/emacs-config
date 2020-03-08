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

;; Org TODOs for projectile projects which are reflected in the Org agenda
;; use `org-capture' to store TODOs for the current project
(use-package org-projectile
  :after (org projectile)
  :config
  (setq org-projectile-projects-file "~/org/projects.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

(provide 'init-proj)

;;; init-proj.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
