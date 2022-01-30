
;;; j-mode.el --- Major mode for editing J programs

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL: http://github.com/zellio/j-mode
;; Version: 1.1.1
;; Keywords: J, Languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock and basic REPL integration for the
;; [J programming language](http://www.jsoftware.com)

;;; Installation

;; The only method of installation is to check out the project, add it to the
;; load path, and load normally. This may change one day.
;;
;; Put this in your emacs config
;;   (add-to-list 'load-path "/path/to/j-mode/")
;;   (load "j-mode")
;;
;; Add for detection of j source files if the auto-load fails
;;   (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

;; Required eval depth for older systems
(setq max-lisp-eval-depth (max 500 max-lisp-eval-depth))

(require 'j-font-lock)
(require 'j-console)
(require 'j-help)

(defconst j-mode-version "1.1.1"
  "`j-mode' version")

(defgroup j-mode nil
  "A mode for J"
  :group 'languages
  :prefix "j-")

(defcustom j-mode-hook nil
  "Hook called by `j-mode'"
  :type 'hook
  :group 'j)

(defvar j-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") 'j-console)
    (define-key map (kbd "C-c C-c") 'j-console-execute-buffer)
    (define-key map (kbd "C-c C-r") 'j-console-execute-region)
    (define-key map (kbd "C-c C-l") 'j-console-execute-line)
    (define-key map (kbd "C-c h")   'j-help-lookup-symbol)
    (define-key map (kbd "C-c C-h") 'j-help-lookup-symbol-at-point)
    map)
  "Keymap for J major mode")

(defvar j-mode-menu nil "Drop-down menu for j-mode interaction")
(easy-menu-define j-mode-menu j-mode-map "J Mode menu"
  '("J"
    ["Start J Console" j-console t]
    ["Execute Buffer" j-console-execute-buffer t]
    ["Execute Region" j-console-execute-region t]
    ["Execute Line" j-console-execute-line t]
    "---"
    ["J Symbol Look-up" j-help-lookup-symbol t]
    ["J Symbol Dynamic Look-up" j-help-lookup-symbol-at-point t]
    ["Help on J-mode" describe-mode t]))

(define-derived-mode j-mode prog-mode "J"
  "Major mode for editing J files.

\\{j-mode-map}"
  :syntax-table j-syntax-table
  (setq ; one day: font-lock-multiline t
        font-lock-defaults j-font-locks
	prettify-symbols-alist j->apl)
  ;; (pretty-add-keywords nil j->apl)
  ;; (use-local-map j-mode-keymap)
  (set (make-local-variable 'comment-start) "NB. ")
  (set (make-local-variable 'comment-start-skip) "NB.+\\s-*")
  ;; Don't show matching parens as some parens are J primitives
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
  ;; https://endlessparentheses.com/locally-configure-or-disable-show-paren-mode.html
  (setq-local blink-matching-paren nil
              show-paren-mode nil))

(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

(provide 'j-mode)

;;; j-mode.el ends here
