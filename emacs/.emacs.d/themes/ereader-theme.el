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

;; Minimalistic color theme emulating eReader devices with a dash of color,
;; forked and modified from eink-theme.el at https://github.com/maio/eink-emacs

;; Emacs 27 introduced a new face attribute ':extend' to control face extension
;; at EOL until the edge of the window. By default, this attribute is non-nil
;; only for the `region' and `hl-line' faces. Make sure to set this attribute
;; to t for any face that needs to be extended beyond the EOL.

;;; Code:

(deftheme ereader
  "Theme emulating an eReader device, with some splashes of color.")

(let ((fg "#121212")
      (fg-table "#262626")
      (bg "#ffffff")
      (bg-light "#d0d0d0")
      (fg-light "#b2b2b2")
      (bg-highlight "#ffffaf")
      (bg-highlight-2 "#d7ffff")
      (bg-highlight-3 "#87ff87")
      (fg-dim "#949494")
      (bg-region "#eeeeee")
      (bg-search "#ffaf5f"))

  (custom-theme-set-faces
   'ereader

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground ,bg))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground "#a8a8a8"))))
   `(font-lock-builtin-face ((t (:foreground ,fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg :weight semi-bold))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground "#626262"))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :underline nil))))
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(header-line ((t (:background ,bg-light))))
   `(highlight ((t (:background "#ffff87"))))
   `(hl-line ((t (:background "#ffd7af"))))
   `(ido-first-match ((t (:foreground ,fg))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:background ,bg-search :foreground ,fg))))
   `(line-number ((t (:foreground "#c6c6c6" :weight light))))
   `(link ((t (:foreground ,fg :underline t))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(mode-line ((t (:background ,bg :foreground ,fg :overline ,fg))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,fg-dim :overline nil))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(query-replace ((t (:background ,bg-search :strike-through t))))
   `(region ((t (:background ,bg-region :foreground ,fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(tab-bar ((t (:background ,bg-light :foreground ,fg))))
   `(tab-bar-tab ((t (:background ,bg-light :foreground "#00005f" :underline t :weight extra-bold))))
   `(tab-bar-tab-inactive ((t (:background ,bg-light :foreground ,fg))))
   `(vertical-border ((t (:foreground ,bg-light))))
   `(whitespace-line ((t (:background ,bg-highlight-2 :foreground ,fg))))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight light))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:foreground ,fg :background "#ffffd7" :extend t))))
   `(org-block-begin-line ((t (:foreground "#585858" :background ,bg-light :extend t))))
   `(org-block-end-line ((t (:foreground "#585858" :background ,bg-light :extend t))))
   `(org-date ((t (:foreground ,fg :underline t))))
   `(org-document-info ((t (:foreground "#00005f" :slant italic))))
   `(org-document-title ((t (:foreground "#00005f" :weight bold))))
   `(org-done ((t (:foreground ,fg-light))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-indent ((t (:inherit org-hide))))
   ;; use :overline to give headings more top margin
   `(org-level-1 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-2 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-3 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-4 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-5 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-level-6 ((t (:foreground ,fg :weight semi-bold :overline ,bg))))
   `(org-link ((t (:foreground "#5f5fd7" :underline t))))
   `(org-meta-line ((t (:inherit org-document-info-keyword))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-table ((t (:foreground ,fg-table))))
   `(org-tag ((t (:inherit shadow :weight bold))))
   `(org-todo ((t (:foreground ,fg))))
   `(org-verbatim ((t (:foreground ,fg :weight semi-bold))))
   `(org-verse ((t (:inherit org-block :slant italic))))

   ;; org-super-agenda
   `(org-super-agenda-header ((t (:foreground ,fg :weight semi-bold))))

   ;; powerline
   `(powerline-active1 ((t (:background "#3a3a3a" :foreground ,bg :inherit mode-line))))
   `(powerline-active2 ((t (:background "#626262" :foreground ,bg :inherit mode-line))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background ,fg))))
   `(doom-modeline-bar-inactive ((t (:background ,fg-dim))))

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
   `(diff-added ((t (:background "#d7ffd7"))))
   `(diff-removed ((t (:background "#ffafaf"))))
   `(diff-refine-added ((t (:background "#afffaf"))))
   `(diff-refine-removed ((t (:background "#ffd7d7"))))
   `(magit-diff-added-highlight ((t (:weight demibold :background "#d7ffd7"))))
   `(magit-diff-added ((t (:background "#d7ffd7"))))
   `(magit-diff-removed-highlight ((t (:weight demibold :background "#ffafaf"))))
   `(magit-diff-removed ((t (:background "#ffafaf"))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-author-face ((t (:inherit default))))
   `(git-timemachine-minibuffer-detail-face ((t (:weight bold))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   ;; `(flycheck-error ((t (:inherit error))))
   ;; `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:inherit default))))
   `(dired-subtree-depth-1-face ((t (:inherit default))))
   `(dired-subtree-depth-2-face ((t (:inherit default))))
   `(dired-subtree-depth-3-face ((t (:inherit default))))
   `(dired-subtree-depth-4-face ((t (:inherit default))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background "#e4e4e4" :weight bold))))
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
   '(parenface-paren-face ((t (:foreground ,fg-light))))
   '(parenface-curly-face ((t (:foreground ,fg-light))))
   '(parenface-bracket-face ((t (:foreground ,fg-light))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground ,fg-light))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground ,fg :weight bold))))
   `(sp-show-pair-mismatch-face ((t (:background "#ff5f5f" :foreground ,fg :weight bold))))
   `(show-paren-match ((t (:foreground ,fg :weight bold))))
   `(show-paren-mismatch ((t (:background "#ff5f5f" :foreground ,fg :weight bold))))

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
   `(nlinum-relative-current-face ((t (:inherit shadow :weight bold))))
   `(linum ((t (:inherit shadow :weight bold))))

   ;; web-mode
   `(web-mode-current-element-highlight-face ((t (:inherit normal :weight bold :foreground ,fg))))

   ;; mmm-mode
   ;; `(mmm-default-submode-face ((t (:inherit normal :background "#ffffef"))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background ,bg-region :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))

   ;; markdown-mode
   `(markdown-code-face ((t (:inherit fixed-pitch :foreground ,fg :background "#ffffd7" :extend t))))

   ;; ace-window
   `(aw-background-face ((t (:foreground ,fg-dim))))
   `(aw-leading-char-face ((t (:foreground "#ff5f5f" :weight bold :height 4.0))))
   `(aw-minibuffer-leading-char-face ((t (:foreground "#ff5f5f" :weight bold))))

   ;; vterm
   `(vterm-color-default ((t (:inherit default :foreground ,fg :background ,bg :extend t))))
   `(vterm-color-black ((t (:inherit term-color-black))))
   `(vterm-color-red ((t (:inherit term-color-red))))
   `(vterm-color-green ((t (:inherit term-color-green))))
   `(vterm-color-yellow ((t (:inherit term-color-yellow))))
   `(vterm-color-blue ((t (:inherit term-color-blue))))
   `(vterm-color-magenta ((t (:inherit term-color-magenta))))
   `(vterm-color-cyan ((t (:inherit term-color-cyan))))
   `(vterm-color-white ((t (:inherit term-color-white))))

   ;; nswbuff
   `(nswbuff-default-face ((t (:foreground "#ff5f5f"))))
   `(nswbuff-current-buffer-face ((t (:foreground "#ff5f5f" :weight bold :underline t))))
   `(nswbuff-separator-face ((t (:inherit shadow))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background "#e4e4e4"))))
   `(highlight-indent-guides-even-face ((t (:background "#c6c6c6"))))
   `(highlight-indent-guides-character-face ((t (:foreground "#c6c6c6" :weight ultra-light))))
   `(highlight-indent-guides-top-odd-face ((t (:background "#808080"))))
   `(highlight-indent-guides-top-even-face ((t (:background "#626262"))))
   `(highlight-indent-guides-top-character-face ((t (:foreground "#626262" :weight ultra-light))))
   `(highlight-indent-guides-stack-odd-face ((t (:background ,fg-light))))
   `(highlight-indent-guides-stack-even-face ((t (:background ,fg-dim))))
   `(highlight-indent-guides-stack-character-face ((t (:foreground ,fg-dim :weight ultra-light))))

   ;; volatile-highlights
   `(vhl/default-face ((t (:background ,bg-highlight-3))))

   ;; evil-quickscope
   `(evil-quickscope-first-face ((t (:foreground ,fg :background ,bg-region))))
   `(evil-quickscope-second-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil-snipe
   `(evil-snipe-first-match-face ((t (:foreground ,fg :background ,bg-search))))
   `(evil-snipe-matches-face ((t (:foreground ,fg :background ,bg-highlight-3))))

   ;; evil
   `(evil-ex-lazy-highlight ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-matches ((t (:background ,bg-highlight-2))))
   `(evil-ex-substitute-replacement ((t (:background ,bg-highlight :underline nil :foreground ,fg))))))

;; (custom-theme-set-variables
;;  'ereader
;;  `(default-frame-alist (add-to-list 'default-frame-alist '(internal-border-width . ,(if (eq system-type 'darwin) 12 6)))))

(custom-theme-set-variables
 'ereader
 '(highlight-indent-guides-auto-enabled nil)
 '(tab-bar-separator " ⋮ "))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
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
