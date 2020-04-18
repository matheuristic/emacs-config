;;; init-org.el --- Emacs configuration Org-mode layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Org mode, https://orgmode.org/

;;; Code:

(require 'init-ui-hydra)

(defvar org-directory (file-name-as-directory (file-truename "~/org")))

(defvar my-org-agenda-inbox (concat org-directory "inbox.org"))
(defvar org-agenda-files (cons my-org-agenda-inbox
                               (mapcar #'(lambda (x) (concat org-directory x))
                                       '("gtd.org"
                                         "tickler.org"))))
(defvar org-capture-templates '(("t" "Todo" entry (file my-org-agenda-inbox)
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
(defvar org-refile-targets '((nil . (:maxlevel . 9)) ;; current buffer
                             (org-agenda-files . (:maxlevel . 3)))) ;; files for agenda display
(defvar org-refile-use-outline-path 'file) ;; allows refile to top level
 ;; tags (note that tags within the same group are mutually exclusive)
(defvar org-tag-alist '((:startgroup) ;; difficulty
                        ("easy" . ?1)
                        ("medium" . ?2)
                        ("hard" . ?3)
                        (:endgroup)
                        (:startgroup) ;; location
                        ("@home" . ?H)
                        ("@work" . ?W)
                        ("@traveling" . ?V)
                        ("@phone" . ?P)
                        ("@email" . ?M)
                        ("@errands" . ?E)
                        (:endgroup)
                        (:startgroup) ;; time-sensitivity
                        ("someday" . ?s)
                        ("urgent" . ?u)
                        (:endgroup)
                        (:startgroup) ;; export
                        ("export" . ?e)
                        ("noexport" . ?x)
                        (:endgroup)
                        ;; ungrouped
                        ("note" . ?n)))

(defvar org-journal-dir (concat org-directory "journal/")) ;; default journal directory

;; Org-mode
;;
;; see http://doc.norang.ca/org-mode.html for an example config
;;
;; using "C-c '" to edit Org source blocks in a side buffer
;; automatically escapes the Org markup on return
(use-package org
  :hook (org-mode . visual-line-mode)
  :bind (("C-c C-M-o" . my-hydra/org-global/body)
         :map org-agenda-mode-map
         ("C-c C-M-m" . my-hydra/org-agenda/body)
         :map org-mode-map
         ("C-c C-M-m" . my-hydra/org-mode/body))
  :init (defhydra my-hydra/org-global (:color teal)
          "Org"
          ("a" org-agenda "agenda")
          ("c" org-capture "capture")
          ("b" org-switchb "switch buffer")
          ("l" org-store-link "store link")
          ("q" nil "quit"))
  :config
  (require 'org-agenda)
  (setq org-adapt-indentation nil ;; don't auto-indent node text when promoting or demoting nodes
        org-agenda-follow-indirect t ;; narrow to subtree in org-agenda-follow-mode ("F" in agenda)
        org-agenda-restore-windows-after-quit t
        org-agenda-start-on-weekday nil
        org-agenda-window-setup 'only-window ;; full-frame Agenda view
        org-catch-invisible-edits 'error
        org-confirm-babel-evaluate nil ;; don't confirm before evaluating code blocks in Org documents
        org-edit-src-content-indentation 2
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers nil
        org-hide-leading-stars t
        org-highlight-latex-and-related '(latex script entities) ;; highlight LaTeX fragments with the `org-highlight-latex-and-related' face
        org-log-into-drawer t
        org-outline-path-complete-in-steps nil
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil ;; don't render sub/superscripts in-buffer
        org-refile-allow-creating-parent-nodes 'confirm
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-preserve-indentation t ;; preserve src code block indentation on export and when switching btw org buffer and edit buffer
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window ;; reuse Org file window for editing source blocks when using "C-c '"
        org-startup-folded t
        org-startup-indented nil
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
        org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANX(c@/!)"))
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-use-fast-todo-selection t
        org-use-speed-commands nil)
  ;; scale up LaTeX fragment preview images on OS X
  (if (and (display-graphic-p)
           (eq system-type 'darwin)
           (executable-find "dvipng"))
      (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))
  ;; add custom agenda commands that only show undated tasks in list view
  (dolist (my-custom-cmd
           '(("N" "Three-day agenda and undated TODO entries"
               ((agenda "" ((org-agenda-span 3)))
                (alltodo "" ((org-agenda-todo-ignore-with-date t)
                             (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up category-keep alpha-up))))))
             ("u" "Undated TODO entries"
              (alltodo "" ((org-agenda-todo-ignore-with-date t)
                           (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up category-keep alpha-up)))))))
    (add-to-list 'org-agenda-custom-commands my-custom-cmd))
  (defhydra my-hydra/org-agenda (:color amaranth :hint nil)
    "
Org agenda

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
                   (org-agenda-redo-all t))))
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/org-mode (:color amaranth :columns 3)
    "Org-mode"
    ("M-s" org-narrow-to-subtree "narrow-subtree")
    ("M-b" org-narrow-to-block "narrow-block")
    ("M-w" widen "widen")
    ("i" org-toggle-inline-images "toggle-images")
    ("I" org-indent-mode "toggle-indent")
    ("P" org-toggle-pretty-entities "toggle-prettify")
    ("<tab>" org-cycle "cycle")
    ("<S-tab>" org-global-cycle "global-cycle")
    ("/" org-sparse-tree "sparse-tree")
    ("c" org-remove-occur-highlights "occur-clear")
    ("p" (lambda (n) (interactive "p") (if org-occur-highlights (previous-error n) (org-previous-visible-heading n))) "previous")
    ("n" (lambda (n) (interactive "p") (if org-occur-highlights (next-error n) (org-next-visible-heading n))) "next")
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
    ("e" my-hydra/org-mode-emphasize/body "→ Emphasize" :exit t)
    ("x" my-hydra/org-mode-extras/body "→ Extras" :exit t)
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/org-mode-emphasize (:color teal :columns 4)
    "Org-mode → Emphasize"
    ("b" (org-emphasize ?*) "bold")
    ("i" (org-emphasize ?/) "italic")
    ("u" (org-emphasize ?_) "underline")
    ("s" (org-emphasize ?+) "strike-through")
    ("c" (org-emphasize ?~) "code")
    ("v" (org-emphasize ?=) "verbatim")
    ("q" my-hydra/org-mode/body "←"))
  ;; use defhydra+ to add more hydra entries
  (defhydra my-hydra/org-mode-extras (:color teal :columns 4)
    "Org-mode → Extras"
    ("q" my-hydra/org-mode/body "←"))
  ;; preview LaTeX fragments scaled to font size, requires dvipng from TexLive
  (when (and (display-graphic-p)
             (executable-find "dvipng"))
    (defvar my-org-latex-scale-base (plist-get org-format-latex-options :scale)
      "Base LaTeX fragment scale.")
    (defun my-org-display-latex-fragments ()
      "Previews LaTeX fragments in the buffer scaled to match font size."
      (interactive)
      (let* ((curr-text-scale (condition-case nil text-scale-mode-amount (error 0)))
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
    (add-hook 'org-mode-hook (lambda (&optional arg) (my-org-display-latex-fragments)))
    ;; ... and regenerate after changing font size
    (advice-add 'text-scale-mode :after (lambda (&optional arg) (my-org-display-latex-fragments))))
  ;; graphical mode Org UI configuration
  (when (display-graphic-p)
    (require 'org-mouse) ;; Org-mode mouse support
    ;; use variable pitch fonts in Org mode ...
    (add-hook 'org-mode-hook #'variable-pitch-mode)
    (add-hook 'org-mode-hook (lambda () (setq line-spacing 0.1)))
    (with-eval-after-load 'init-ui-font
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
  ;; maximize org-capture buffer
  (defun my-org-capture-setup (&rest args)
    "Save window configuration prior to `org-capture'."
    (set-frame-parameter nil 'my-org-capture-prior-config (current-window-configuration)))
  (defun my-org-capture-teardown ()
    "Restore window configuration prior to `org-capture'."
    (let ((prior-window-configuration (frame-parameter nil 'my-org-capture-prior-config)))
      (when prior-window-configuration
        (set-window-configuration prior-window-configuration))))
  (advice-add 'org-capture :before 'my-org-capture-setup)
  (add-hook 'org-capture-mode-hook 'delete-other-windows)
  (add-hook 'org-capture-after-finalize-hook 'my-org-capture-teardown)
  ;; display the outline path at point using which-func
  (with-eval-after-load 'which-func
    (add-to-list 'which-func-modes 'org-mode)
    (defun my-org-which-function-string-shortener (str &optional maxlen)
      "Shortens STR if it is longer than MAXLEN chars."
      (let* ((len (length str))
             (maxlen (or maxlen 40)) ;; default maxlen of 40
             (num-left-chars (/ maxlen 2))
             (num-right-chars (- maxlen num-left-chars 3)))
        (if (> len maxlen)
            (concat (substring str 0 num-left-chars)
                    "..."
                    (substring str (- len num-right-chars) len))
          str)))
    (defun my-org-which-function ()
      "Returns current outline path."
      (if (eq major-mode 'org-mode)
        (condition-case nil
            (mapconcat #'my-org-which-function-string-shortener
                       (org-get-outline-path t)
                       " > ")
          (error nil))))
    (add-to-list 'which-func-functions #'my-org-which-function)
    ;; Org-specific which-func header
    (defun my-org-narrow-to-subtree-toggle ()
      "Toggle org-narrow-to-subtree."
      (interactive)
      (if (buffer-narrowed-p)
          (widen)
        (org-narrow-to-subtree)))
    (defvar my-which-func-header-keymap-org
      (let ((map (make-sparse-keymap)))
        (define-key map [header-line mouse-1] 'my-org-narrow-to-subtree-toggle)
        ;; work around mouse-1 mapping to mouse-2 when cursor is on org bullet
        (define-key map [header-line mouse-2] 'my-org-narrow-to-subtree-toggle)
        (define-key map [header-line mouse-3] 'outline-up-heading)
        (define-key map [header-line wheel-up] 'org-backward-heading-same-level)
        (define-key map [header-line wheel-down] 'org-forward-heading-same-level)
        map)
      "Keymap for header line which-func.")
    (defvar my-which-func-header-keymap-help-text-org
      "mouse-1 : toggle rest visibility\n\
mouse-3 : go up one heading\n\
wheel-u : next same-level heading\n\
wheel-d : prev same-level heading"
      "Help text for `my-which-fun-header-keymap-org'.")
    (defvar my-which-func-header-format-org
            `(:propertize which-func-current
                          local-map ,my-which-func-header-keymap-org
                          face which-func
                          mouse-face mode-line-highlight
                          help-echo my-which-func-header-keymap-help-text-org))
    ;; add Org-mode which-func header to lookup assoc list, see init-ui.el
    (add-to-list 'my-which-func-header-formats `(org-mode . ,my-which-func-header-format-org))))

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
          org-download-timestamp "%Y%m%d%H%M%S-")
    (defhydra+ my-hydra/org-mode-extras ()
      ("d" my-hydra/org-mode-extras/download/body "→ Download" :exit t))
    (defhydra my-hydra/org-mode-extras/download (:color teal)
      "Org-mode → Extras → Download"
      ("s" org-download-screenshot "screenshot")
      ("y" org-download-yank "yank")
      ("q" my-hydra/org-mode-extras/body "←"))))

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
              "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
        org-capture-templates)
  (setq org-journal-date-prefix "#+TITLE: Daily Journal "
        org-journal-file-format "%Y%m%d.org"
        org-journal-file-type 'daily))

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
  (advice-add 'org-present-read-only :after (lambda () (my-org-present-extra-mode 1)))
  (advice-add 'org-present-read-write :after (lambda () (my-org-present-extra-mode 0)))
  (defhydra+ my-hydra/org-mode-extras ()
          ("p" (lambda ()
                   (interactive)
                   (let ((in-present-mode (condition-case nil
                                              org-present-mode
                                            (error nil))))
                     (if in-present-mode (org-present-quit) (org-present))))
           "org-present" :exit t)))

;; export to reveal.js presentations, https://gitlab.com/oer/org-re-reveal
(use-package org-re-reveal
  :after org
  :init (setq org-re-reveal-note-key-char nil
              org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/"))

;; export to Markdown
(use-package ox-md
  :ensure nil ;; built-in to Org
  :after org)

;; export to Tufte-LaTeX, requires https://github.com/tsdye/tufte-org-mode
(use-package org-tufte-latex
  :ensure nil ;; local package
  :after org
  :config (defhydra+ my-hydra/org-mode-extras ()
            ("T" org-tufte-latex-minor-mode "org-tufte-latex" :exit t)))

(provide 'init-org)

;;; init-org.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
