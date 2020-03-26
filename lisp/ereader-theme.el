;;; ereader-theme.el --- eReader color theme

;; Modified work Copyright (C) 2020 matheuristic
;; Original work Copyright (C) 2013-2019 Marian Schubert

;; Author: matheuristic
;; URL: https://github.com/matheuristic/emacs-config
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimalistic color theme mostly emulating eReader devices
;; forked from eink-theme.el at https://github.com/maio/eink-emacs

;;; Code:

(deftheme ereader
  "Theme emulating an eReader device, with some splashes of color.")

(let ((fg "#111111")
      (fg-table "#222291")
      (bg "#fffff8")
      (bg-light "#ddddd8")
      (fg-light "#ddddd8")
      (bg-highlight "#fff1aa")
      (bg-highlight-2 "light cyan")
      (bg-highlight-3 "light green"))

  (custom-theme-set-faces
   'ereader

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground "white smoke"))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground "dark gray"))))
   `(font-lock-builtin-face ((t (:background ,bg :foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg :weight semi-bold))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground "#606060"))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(highlight ((t (:background "#efecaf"))))
   `(hl-line ((t (:background "#fedcba"))))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background "#eeeee8" :foreground ,fg))))
   `(line-number ((t (:foreground "#cacaca" :weight light))))
   `(link ((t (:foreground ,fg :underline t))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(mode-line ((t (:background ,bg-light :foreground ,fg))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg-light :foreground "#909090"))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(query-replace ((t (:strike-through t))))
   `(region ((t (:background "#eeeee8" :foreground ,fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg))))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg :background "#ffffe0"))))
   `(org-block-begin-line ((t (:foreground "#555555" :background "#e2e1d5" :height 0.8))))
   `(org-block-end-line ((t (:foreground "#555555" :background "#e2e1d5" :height 0.8))))
   `(org-date ((t (:foreground ,fg :underline t))))
   `(org-document-info ((t (:foreground "midnight blue" :slant italic :height 1.3))))
   `(org-document-title ((t (:foreground "midnight blue" :weight bold :height 1.8))))
   `(org-done ((t (:foreground ,fg-light))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-indent ((t (:inherit org-hide))))
   ;; use :overline to give headings more top margin
   `(org-level-1 ((t (:foreground ,fg :weight semi-bold :height 1.3 :overline ,bg))))
   `(org-level-2 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-3 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-4 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-5 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-level-6 ((t (:foreground ,fg :weight semi-bold :height 1.1 :overline ,bg))))
   `(org-link ((t (:foreground "royal blue" :underline t))))
   `(org-meta-line ((t (:inherit org-document-info-keyword))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-table ((t (:foreground ,fg-table))))
   `(org-tag ((t (:inherit shadow :weight bold :height 0.8))))
   `(org-todo ((t (:foreground ,fg))))
   `(org-verbatim ((t (:foreground ,fg :weight semi-bold))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   
   ;; powerline
   `(powerline-active1 ((t (:background "gray22" :foreground ,bg :inherit mode-line))))
   `(powerline-active2 ((t (:background "gray40" :foreground ,bg :inherit mode-line))))

   ;; magit
   `(magit-header ((t (:weight semi-bold))))
   `(magit-item-mark ((t (:background ,bg-highlight))))
   `(magit-item-highlight ((t (:weight bold))))
   `(magit-section-heading ((t (:weight semi-bold))))
   `(magit-section-highlight ((t (:weight semi-bold))))
   `(magit-diff-context-highlight ((t (:foreground ,fg))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))

   ;; diff
   `(diff-added ((t (:background "#e9ffe9"))))
   `(diff-removed ((t (:background "#ffecec"))))
   `(diff-refine-added ((t (:background "#a4f4a3"))))
   `(diff-refine-removed ((t (:background "#f9cbca"))))
   `(magit-diff-added-highlight ((t (:weight demibold :background "#e9ffe9"))))
   `(magit-diff-added ((t (:background "#e9ffe9"))))
   `(magit-diff-removed-highlight ((t (:weight demibold :background "#ffecec"))))
   `(magit-diff-removed ((t (:background "#ffecec"))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-author-face ((t (:inherit default))))
   `(git-timemachine-minibuffer-detail-face ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:inherit default))))
   `(dired-subtree-depth-1-face ((t (:inherit default))))
   `(dired-subtree-depth-2-face ((t (:inherit default))))
   `(dired-subtree-depth-3-face ((t (:inherit default))))
   `(dired-subtree-depth-4-face ((t (:inherit default))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background "gray90" :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:background ,bg-highlight))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   '(parenface-paren-face ((t (:foreground "gray70"))))
   '(parenface-curly-face ((t (:foreground "gray70"))))
   '(parenface-bracket-face ((t (:foreground "gray70"))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground "gray70"))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground "black" :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background "red" :foreground "black" :weight bold))))
   `(show-paren-match ((t (:foreground "black" :weight bold))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black" :weight bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:inherit default))))
   `(rpm-spec-package-face ((t (:inherit default))))
   `(rpm-spec-macro-face ((t (:inherit default))))
   `(rpm-spec-doc-face ((t (:inherit default))))
   `(rpm-spec-var-face ((t (:inherit default))))
   `(rpm-spec-ghost-face ((t (:inherit default))))
   `(rpm-spec-section-face ((t (:inherit default :weight bold))))

   ;; linum / nlinum-relative
   `(nlinum-relative-current-face ((t (:inherit normal :weight bold))))
   `(linum ((t (:inherit normal :weight bold))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:inherit normal :weight bold :foreground ,fg))))

   ;; mmm-mode
   `(mmm-default-submode-face ((t (:inherit normal :background "#ffffef"))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background "#eeeee8" :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))
  
   ;; evil-quickscope
   `(evil-quickscope-first-face ((t (:foreground ,fg :background "#eeeee8"))))
   `(evil-quickscope-second-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((t (:foreground ,fg :background "#eeeee8"))))
   `(evil-snipe-matches-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil
   `(evil-ex-lazy-highlight ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-matches ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-replacement ((t (:background ,bg-highlight :underline nil :foreground ,fg))))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ereader)

(provide 'ereader-theme)
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; ereader-theme.el ends here
