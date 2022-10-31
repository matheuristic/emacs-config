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
;; On trackpads, z, x and c can be used to simulate a left-click, middle-click
;; and right-click during a chord (at least one mouse button held down),
;; allowing for 1-2 and 1-3 chords.
;;
;; TODO:
;; * Plumbing. Use wand for this? Need to have a catchall case to do
;;   `acme-mode--search'. Or just implement from scratch.
;;
;; Adapted from https://github.com/akrito/acme-mouse/blob/master/acme-mouse.el
;; with additional functionality for executing text in an external
;; shell and other Acme functionality.

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
* 'donothing (2-3 chord or 3-2 chord pressed, to cancel execute or look)")

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

(defvar acme-mode--last-mouse-event nil
  "Last button 1, 2 or 3 mouse event.")

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
  "Set `acme-mode--state' to 'noselect if no buttons are pressed."
  (when (acme-mode--down-p acme-mode--nobuttons)
    (setq acme-mode--state 'noselect)))

(defun acme-mode--select-region ()
  "Acme mode region selection function to faciliate 1-2 and 1-3 chords."
  (let ((range (mouse-start-end (mark)
                                (point)
                                mouse-selection-click-count)))
    (setq acme-mode--region-start (nth 0 range))
    (setq acme-mode--region-end (nth 1 range))
    (set-mark acme-mode--region-start)
    (goto-char acme-mode--region-end)))

(defun acme-mode--update-last-mouse-events (event)
  "Update `acme-mode--last-mouse-event' with new EVENT."
  (setq acme-mode--last-mouse-event event))

(defun acme-mode--make-mouse-event (type)
  "Generate a mouse event of given TYPE at point.

For example,

  (acme-mode--make-mouse-event 'double-down-mouse-1)

creates an event similar to that generated when the left mouse
is pressed twice and using last tracked mouse event position."
  ;; Kludge so can keep repeating cut and paste chords with keyboard,
  ;; else we end up progressively undo-ing instead of back and forth
  (when (eq last-command 'undo)
    (setq last-command 'left-char))
  (cond ((= mouse-selection-click-count 0)
         (list type (posn-at-point)))
        (acme-mode--last-mouse-event
         (list type (nth 1 acme-mode--last-mouse-event)))
        (t
         (error "No last mouse event but `mouse-selection-click' greater than zero"))))

;; Button 1 down-press
(defun acme-mode--down-mouse-1 (event)
  "Acme mode handler for left-button press EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
  (acme-mode--button-down acme-mode--lbutton)
  (cond ((eq acme-mode--state 'noselect)
         (setq acme-mode--state 'textselect)
         (mouse-set-mark event)
         (mouse-drag-region event))))

;; Button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--mouse-1 (event)
  "Acme mode handler for left-button release EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
  (acme-mode--button-up acme-mode--lbutton)
  (cond ((eq acme-mode--state 'textselect)
         (setq deactivate-mark nil)
         (mouse-set-point event)
         (setq transient-mark-mode (cons 'only t))))
  (acme-mode--maybe-reset-state))

;; Double button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--double-mouse-1 (event)
  "Acme mode handler for double or triple left-click EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
  (acme-mode--button-up acme-mode--lbutton)
  (cond ((eq acme-mode--state 'textselect)
         (setq deactivate-mark nil)
         (mouse-set-point event)
         (acme-mode--select-region)
         (setq transient-mark-mode (cons 'only t))))
  (acme-mode--maybe-reset-state))

;; Button 1 release, mouse pointer moved since down-mouse-1
(defun acme-mode--drag-mouse-1 (event)
  "Acme mode handler for left-button drag release EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
  (acme-mode--button-up acme-mode--lbutton)
  (cond ((eq acme-mode--state 'textselect)
         (setq deactivate-mark nil)
         (mouse-set-region event)
         (setq transient-mark-mode (cons 'only t)))
        ((eq major-mode 'completion-list-mode)
         (choose-completion event)))
  (acme-mode--maybe-reset-state))

;; Button 2 down-press
(defun acme-mode--down-mouse-2 (event)
  "Acme mode handler for middle-button press EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
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
         (call-interactively 'undo))
        ((eq acme-mode--state 'textselect3)
         (setq acme-mode--state 'donothing))))

;; Button 2 release, no mouse movement since down-mouse-2
(defun acme-mode--mouse-2 (event)
  "Acme mode handler for middle-button release EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
  (acme-mode--button-up acme-mode--mbutton)
  (cond ((eq acme-mode--state 'textselect2)
         (acme-mode--execute event))
        ((eq major-mode 'completion-list-mode)
         (choose-completion event)))
  (acme-mode--maybe-reset-state))

;; Button 3 down-press
(defun acme-mode--down-mouse-3 (event arg)
  "Acme mode handler for right-button press EVENT.

Specify a prefix ARG to insert a specific kill ring entry.

For example, '<down-mouse-left> Control-u 3 <down-mouse-right>'
will insert the 3rd most recent entry in the kill ring."
  (interactive "e\nP")
  (acme-mode--update-last-mouse-events event)
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
         (call-interactively 'undo)
         (set-mark acme-mode--region-start)
         (goto-char acme-mode--region-end))
        ((eq acme-mode--state 'textselect2)
         (setq acme-mode--state 'donothing))))

;; Button 3 release
(defun acme-mode--mouse-3 (event)
  "Acme mode handler for right-button release EVENT."
  (interactive "e")
  (acme-mode--update-last-mouse-events event)
  (acme-mode--button-up acme-mode--rbutton)
  (cond ((eq acme-mode--state 'textselect3)
         (acme-mode--search event)))
  (acme-mode--maybe-reset-state))

;; same as mouse-3, no drag-mouse-3 select and look currently, so just
;; perform a normal drag-mouse-1 select then mouse-3 instead
(defun acme-mode--drag-mouse-3 (event)
  "Acme mode handler for right-button drag release EVENT."
  (interactive "e")
  (acme-mode--mouse-3 event))

(defun acme-mode--insert-z ()
  "Wrapper for z binding, acts as left-mouse-click when chording.

If `acme-mode--state' is not 'noselect, this will simulate
effects of down mouse 1 then mouse 1 at point, otherwise it
will insert a z character as normal."
  (interactive)
  (cond ((eq acme-mode--state 'noselect)
         (when (region-active-p)
           (delete-region (region-beginning) (region-end)))
         (self-insert-command 1 ?z))
        (t
         (acme-mode--down-mouse-1 (acme-mode--make-mouse-event 'down-mouse-1))
         (if (> mouse-selection-click-count 1)
             (acme-mode--double-mouse-1 (acme-mode--make-mouse-event 'double-mouse-1)))
           (acme-mode--mouse-1 (acme-mode--make-mouse-event 'mouse-1)))))

(defun acme-mode--insert-x ()
  "Wrapper for x binding, acts as middle-mouse-click when chording.

If `acme-mode--state' is not 'noselect, this will simulate
effects of down mouse 2 then mouse 2 at point, otherwise it
will insert a x character as normal."
  (interactive)
  (cond ((eq acme-mode--state 'noselect)
         (when (region-active-p)
           (delete-region (region-beginning) (region-end)))
         (self-insert-command 1 ?x))
        (t
         (acme-mode--down-mouse-2 (acme-mode--make-mouse-event 'down-mouse-2))
         (acme-mode--mouse-2 (acme-mode--make-mouse-event 'mouse-2)))))

(defun acme-mode--insert-c (&optional arg)
  "Wrapper for c binding, acts as right-mouse-click when chording.

If `acme-mode--state' is not 'noselect, this will simulate
effects of down mouse 3 then mouse 3 at point, otherwise it
will insert a x character as normal.

Can be optionally specified with prefix ARG to insert a specific
kill ring entry when doing a 1-3 chord."
  (interactive "P")
  (cond ((eq acme-mode--state 'noselect)
         (when (region-active-p)
           (delete-region (region-beginning) (region-end)))
         (self-insert-command 1 ?c))
        (t
         (acme-mode--down-mouse-3 (acme-mode--make-mouse-event 'down-mouse-3) arg)
         (acme-mode--mouse-3 (acme-mode--make-mouse-event 'mouse-3)))))

;; Search - modified from Dan McCarthy's acme-search.el

(defun acme-mode--header-line-active-p ()
  "Check if there is an active head-line in the window."
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
    ;; keyboard char wrappers (for chording when using trackpads)
    (define-key map (kbd "z") #'acme-mode--insert-z)
    (define-key map (kbd "x") #'acme-mode--insert-x)
    (define-key map (kbd "c") #'acme-mode--insert-c)
    map)
  "Acme mode keymap.")

(defvar acme-mode--prior-delete-selection-mode nil
  "Whether Delete Selection mode was enabled prior to Acme mode.")

(defvar acme-mode--prior-transient-mark-mode nil
  "Whether Transient Mark mode was enabled prior to Acme mode.")

(defun acme-mode--enable ()
  "Setup for Acme mode'."
  (setq acme-mode--prior-delete-selection-mode (symbol-value delete-selection-mode))
  (setq acme-mode--prior-transient-mark-mode (symbol-value transient-mark-mode))
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  (setq acme-mode--state 'noselect
        acme-mode--buttons acme-mode--nobuttons
        acme-mode--region-start nil
        acme-mode--region-end nil
        acme-mode--last-mouse-event nil)
  (message "Acme mode enabled"))

(defun acme-mode--disable ()
  "Teardown for Acme mode'."
  (delete-selection-mode acme-mode--prior-delete-selection-mode)
  (transient-mark-mode acme-mode--prior-transient-mark-mode)
  (message "Acme mode disabled"))

;;;###autoload
(define-minor-mode acme-mode
  "Acme mode, a global minor mode to replicate Plan 9 Acme mouse behavior.

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
      (acme-mode--enable)
    (acme-mode--disable)))

(provide 'acme-mode)

;;; acme-mode.el ends here
