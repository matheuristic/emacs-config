;;; acme-mode.el --- Minor mode for approximating Acme mouse behavior -*- lexical-binding: t; -*-

;; Additional copyright (c) 2022 matheuristic
;; Original copyright (c) 2009 Alex Kritikos

;; Author: matheuristic
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: acme, mouse
;; URL: https://github.com/matheuristic/emacs-config

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for replicating Acme mouse behavior.
;;
;; Adapted from https://github.com/akrito/acme-mouse/blob/master/acme-mouse.el
;; with additional functionality for executing text in an external
;; shell and other Acme functionality.
;;
;; TODO:
;; * Trackpad mouse chords. E.g., Acme is able to do left-click then
;;   press Option in macOS to do a 1-2 chord. How to do this in Emacs?
;; * Plumbing. Use wand for this? Need to have a catchall case to do
;;   `acme-mode--search'. Or just implement from scratch.

;;; Code:

;; Button values
(defvar acme-mode--lbutton 1)
(defvar acme-mode--mbutton 2)
(defvar acme-mode--rbutton 4)
(defvar acme-mode--allbuttons (+ acme-mode--lbutton
                                 acme-mode--mbutton
                                 acme-mode--rbutton))
(defvar acme-mode--nobuttons 0)

(defvar acme-mode--state 'noselect
  "State of `amce-mode'.

Possible states:
* 'noselect (neutral state with no mouse buttons pressed)
* 'textselect (1 pressed but no further buttons)
* 'textselect-cut (1-2 chord last run but some buttons still pressed)
* 'textselect-paste (1-3 chord last run but some buttons still pressed)
* 'textselect2 (2 pressed but no further buttons)
* 'textselect3 (3 pressed but no further buttons)
* 'donothing (2-3 chord or 3-2 chord pressed, to cancel execute or look)
")

(defvar acme-mode--buttons acme-mode--nobuttons
  "Current buttons pressed stored as bits of an integer.

Examples:
  (eql acme-mode-state-down acme-mode--nobuttons)
    => t if no buttons pressed, nil if any pressed
  (eql acme-mode-state-down acme-mode--lbutton)
    => t if _only_ left button pressed, nil otherwise
  (> (logand acme-mode-state-down acme-mode--rbutton) 0)
    => t if right button pressed, 0 if not")

(defvar acme-mode--region-start nil
  "Start of last cut region.")

(defvar acme-mode--region-end nil
  "End of last cut region.")

(defvar acme-mode--seltext nil
  "Selected text.")

(defvar acme-mode--argtext nil
  "Argument text.")

;; Convenience functions
(defun acme-mode--down-p (&rest buttons)
  "Check `acme-mode-buttons' if exactly the given BUTTONS are pressed."
  (let ((checkval (apply '+ buttons)))
    (eql acme-mode--buttons checkval)))
(defun acme-mode--button-down (button)
  "Update `acme-mode--buttons' on BUTTON press."
  (setq acme-mode--buttons
        (logior acme-mode--buttons button)))
(defun acme-mode--button-up (button)
  "Update `acme-mode--buttons' on BUTTON release."
  (setq acme-mode--buttons
        (logand acme-mode--buttons
                (- acme-mode--allbuttons button))))
(defun acme-mode--maybe-reset-state ()
  (when (acme-mode--down-p acme-mode--nobuttons)
    (setq acme-mode--state 'noselect)))

;; Button 1 down-press
(defun acme-mode--down-mouse-1 (event)
  "Handler for down-mouse-1 event."
  (interactive "e")
  (acme-mode--button-down acme-mode--lbutton)
  (cond ((eq acme-mode--state 'noselect)
         (setq acme-mode--state 'textselect)
         (mouse-set-mark event)
         (mouse-drag-region event))))

;; Button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--mouse-1 (event)
  "Handler for left-click EVENT."
  (interactive "e")
  (acme-mode--button-up acme-mode--lbutton)
  (cond ((eq acme-mode--state 'textselect)
         (setq deactivate-mark nil)
         (mouse-set-point event)
         (setq transient-mark-mode (cons 'only t))))
  (acme-mode--maybe-reset-state))

;; Double button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--double-mouse-1 (event)
  "Handler for double left-click EVENT."
  (interactive "e")
  (acme-mode--button-up acme-mode--lbutton)
  (cond ((eq acme-mode--state 'textselect)
         (setq deactivate-mark nil)
         (mouse-set-point event)
         (acme-mode--select-region)
         (setq transient-mark-mode (cons 'only t))))
  (acme-mode--maybe-reset-state))

;; Button 1 release, mouse pointer moved since down-mouse-1
(defun acme-mode--drag-mouse-1 (event)
  "Handler for left-click drag EVENT."
  (interactive "e")
  (acme-mode--button-up acme-mode--lbutton)
  (cond ((eq acme-mode--state 'textselect)
         (setq deactivate-mark nil)
         (mouse-set-region event)
         (setq transient-mark-mode (cons 'only t))))
  (acme-mode--maybe-reset-state))

;; Button 2 down-press
(defun acme-mode--down-mouse-2 (event)
  "Handler for middle-click EVENT."
  (interactive "e")
  (acme-mode--button-down acme-mode--mbutton)
  (cond ((eq acme-mode--state 'noselect)
         (setq acme-mode--state 'textselect2))
        ((eq acme-mode--state 'textselect)
         (setq acme-mode--state 'textselect-cut)
         (mouse-set-point event)
         (acme-mode--select-region)
         (kill-region (mark) (point)))
        ((eq acme-mode--state 'textselect-paste)
         (setq acme-mode--state 'textselect-cut)
         (undo))
        ((eq acme-mode--state 'textselect3)
         (setq acme-mode--state 'donothing))))

;; Button 2 release, no mouse movement since down-mouse-2
(defun acme-mode--mouse-2 (event)
  (interactive "e")
  (acme-mode--button-up acme-mode--mbutton)
  (cond ((eq acme-mode--state 'textselect2)
         (acme-mode--execute event)))
  (acme-mode--maybe-reset-state))

;; Button 3 down-press
(defun acme-mode--down-mouse-3 (event arg)
  (interactive "e\nP")
  (acme-mode--button-down acme-mode--rbutton)
  (cond ((eq acme-mode--state 'noselect)
         (setq acme-mode--state 'textselect3))
        ((eq acme-mode--state 'textselect)
         (setq acme-mode--state 'textselect-paste)
         (mouse-set-point event)
         (acme-mode--select-region)
         (delete-region (mark) (point))
         (yank arg)
         (setq deactivate-mark nil)
         (activate-mark))
        ((eq acme-mode--state 'textselect-cut)
         (setq acme-mode--state 'textselect-paste)
         (undo)
         (set-mark acme-mode--region-start)
         (goto-char acme-mode--region-end))
        ((eq acme-mode--state 'textselect2)
         (setq acme-mode--state 'donothing))))

;; Button 3 release
(defun acme-mode--mouse-3 (event)
  (interactive "e")
  (acme-mode--button-up acme-mode--rbutton)
  (cond ((eq acme-mode--state 'textselect3)
         (acme-mode--search event)))
  (acme-mode--maybe-reset-state))

;; same as mouse-3, no drag-mouse-3 select and look currently, so just
;; perform a normal drag-mouse-1 select then mouse-3 instead
(defun acme-mode--drag-mouse-3 (event)
  (interactive "e")
  (acme-mode--mouse-3 event)
  (acme-mode--maybe-reset-state))

(defun acme-mode--select-region ()
  (let ((range (mouse-start-end (mark)
                                (point)
                                mouse-selection-click-count)))
    (setq acme-mode--region-start (nth 0 range))
    (setq acme-mode--region-end (nth 1 range))
    (set-mark acme-mode--region-start)
    (goto-char acme-mode--region-end)))

;; Search - modified from Dan McCarthy's acme-search.el

(defun acme-mode--header-line-active-p ()
  (not (null header-line-format)))

(defun acme-mode--move-mouse-to-point ()
  "Move mouse pointer to point in the current window."
  (let* ((coords (posn-col-row (posn-at-point)))
	 (window-coords (window-inside-edges))
	 (x (+ (car coords) (car window-coords) -1)) ;the fringe is 0
	 (y (+ (cdr coords) (cadr window-coords)
	       (if (acme-mode--header-line-active-p)
		   -1
		 0))))
    (set-mouse-position (selected-frame) x y)))

(defun acme-mode--highlight-search (sym)
  "Set the region to current search result for SYM.

Assumes point is at the end of the result."
  (set-mark (point))
  (search-backward sym nil t)
  (exchange-point-and-mark))

(defun acme-mode--search (posn)
  "Search forward for selected text or symbol at POSN.

The mouse is warped to the search result if one exists."
  (let ((sym (if (region-active-p)
                 (buffer-substring (mark) (point))
               (mouse-set-point posn)
               (thing-at-point 'symbol))))
    (if (search-forward sym nil t)
        (acme-mode--highlight-search sym)
      (let ((saved-point (point)))
        (message "Wrapped search")
        (goto-char (point-min))
        (if (search-forward sym nil t)
            (acme-mode--highlight-search sym)
          (goto-char saved-point))))
    ;; Redisplay screen if search went past the bottom of window
    (unless (posn-at-point)
      (universal-argument)
      (recenter))
    (acme-mode--move-mouse-to-point)))

(defun acme-mode--execute (posn)
  "Run selected text or symbol at POSN in an external shell.

The selected command is run asynchronously."
  (let ((shell-command-dont-erase-buffer t) ; append to instead of overwriting output buffer
        (sym (if (region-active-p)
                 (buffer-substring (mark) (point))
               (mouse-set-point posn)
               (thing-at-point 'symbol))))
    (async-shell-command sym)))

;; See https://emacs.stackexchange.com/questions/64964/difference-between-mouse-1-and-down-mouse-1
(defvar acme-mode-map
  (let ((map (make-sparse-keymap)))
    ;; left-click
    (define-key map [down-mouse-1] #'acme-mode--down-mouse-1)
    (define-key map [mouse-1] #'acme-mode--mouse-1)
    (define-key map [double-mouse-1] #'acme-mode--double-mouse-1)
    (define-key map [triple-mouse-1] #'acme-mode--double-mouse-1)
    (define-key map [drag-mouse-1] #'acme-mode--drag-mouse-1)
    ;; middle-click
    (define-key map [down-mouse-2] #'acme-mode--down-mouse-2)
    (define-key map [mouse-2] #'acme-mode--mouse-2)
    (define-key map [drag-mouse-2] #'acme-mode--mouse-2)
    ;; right-click
    (define-key map [down-mouse-3] #'acme-mode--down-mouse-3)
    (define-key map [double-down-mouse-3] #'acme-mode--down-mouse-3)
    (define-key map [triple-down-mouse-3] #'acme-mode--down-mouse-3)
    (define-key map [mouse-3] #'acme-mode--mouse-3)
    (define-key map [drag-mouse-3] #'acme-mode--drag-mouse-3)
    map)
  "`acme-mode' keymap.")

(defvar acme-mode-prior-delete-selection-mode nil
  "Whether `delete-selection-mode' was enabled prior to `acme-mode'.")

(defun acme-mode-enable ()
  "Setup for `acme-mode'."
  (setq acme-mode-prior-delete-selection-mode (symbol-value delete-selection-mode))
  (delete-selection-mode t)
  (setq acme-mode--state 'noselect
        acme-mode--buttons acme-mode--nobuttons
        acme-mode--region-start nil
        acme-mode--region-end nil
        acme-mode--seltext nil
        acme-mode--argtext nil)
  (message "acme-mode enabled"))

(defun acme-mode-disable ()
  "Teardown for `acme-mode'."
  (delete-selection-mode acme-mode-prior-delete-selection-mode)
  (message "acme-mode disabled"))

;;;###autoload
(define-minor-mode acme-mode
  "Global minor mode to replicate Plan 9 Acme mouse behavior.

When called interactively, toggle `acme-mode'. With prefix ARG,
enable `acme-mode' if ARG is positive, otherwise disable it.

When called from Lisp code, enable `acme-mode' if ARG is omitted,
nil or positive. If ARG is `toggle', toggle `acme-mode'.

\\{acme-mode-map}"
  :lighter " A"
  :init-value nil
  :keymap acme-mode-map
  :global t
  (if acme-mode
      (acme-mode-enable)
    (acme-mode-disable)))

(provide 'acme-mode)

;;; acme-mode.el ends here
