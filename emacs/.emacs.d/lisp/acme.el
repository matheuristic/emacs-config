;;; acme.el --- Minor mode for emulating Plan 9 Acme interface -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Thiam Lee

;; Author: Thiam Lee
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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

;; Global minor mode for emulating the Plan 9 Acme interface.
;;
;; See http://acme.cat-v.org/mouse for more about the Acme mouse
;; interface.
;;
;; For text execution, the Acme keywords Del (kill buffer and its
;; window), Snarf (copy region into kill ring), Get (revert the buffer
;; to the saved version), Undo, Redo (requires undo-tree-mode), Put,
;; Zerox (create new window with the same buffer) and Look (but no
;; support for arguments) are supported. Acme pipes <, | and > are
;; implemented but not ranges:
;; - Selecting "abcdef" and executing "|tr '[abc]' '[123]' to replace
;;   the selected "abcdef" with "123def" will work.
;; - Executing ",|tr '[abc]' '[123]'" to change all a, b and c chars
;;   in the buffer to 1, 2 and 3 will not work.
;;
;; There is a window tag implementation that functions more like the
;; root window tag in Acme. When text is searched (using mouse 3) or
;; executed (mouse 2) there, the search or execution is done in the
;; last selected window. For example, if text "abcdef" is selected in
;; a file window and after that "|sed -e 's/abc/123/g'" is executed in
;; a tag buffer, then the text "abcdef" is changed to "123def". Use
;; the minor mode menu or the `acme-mode-pop-tag-buffer' function to
;; open a tag buffer. Additional tag buffers may be opened by
;; prefixing a numeric argument when calling that function with "C-u".
;;
;; On trackpads, clicks with modifier keys pressed can be used to
;; simulate middle- and right-clicks, and keyboard keys can be used to
;; simulate a left-click, middle-click and right-click during a chord
;; (at least one mouse button held down), allowing for 1-2 and 1-3
;; chords. This can be enabled by adding the appropriate bindings to
;; `acme-mode-map' and customizing `acme-mode-keyboard-chord-keylist'
;; prior to enabling Acme mode, e.g.,
;;
;;   (require 'acme)
;;   (with-eval-after-load 'acme
;;     ;; open tag buffer key binding
;;     (define-key acme-mode-map [C-M-return] #'acme-mode-pop-tag-buffer)
;;     ;; trackpad support
;;     (cond ((eq system-type 'darwin)
;;            ;; middle-click
;;            (define-key acme-mode-map [M-down-mouse-1] #'acme-mode--down-mouse-2)
;;            (define-key acme-mode-map [M-mouse-1] #'acme-mode--mouse-2)
;;            (define-key acme-mode-map [M-drag-mouse-1] #'acme-mode--mouse-2)
;;            ;; right-click
;;            (define-key acme-mode-map [s-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [s-double-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [s-triple-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [s-mouse-1] #'acme-mode--mouse-3)
;;            (define-key acme-mode-map [s-drag-mouse-1] #'acme-mode--mouse-3)
;;            (setq acme-mode-keyboard-chord-keylist
;;                  '(("1" . 1) ("s-1" . 1) ("M-1" . 1)
;;                    ("2" . 2) ("s-2" . 2) ("M-2" . 2)
;;                    ("3" . 3) ("s-3" . 3) ("M-3" . 3))))
;;           ((eq system-type 'gnu/linux)
;;            ;; middle-click
;;            (define-key acme-mode-map [C-down-mouse-1] #'acme-mode--down-mouse-2)
;;            (define-key acme-mode-map [C-mouse-1] #'acme-mode--mouse-2)
;;            (define-key acme-mode-map [C-drag-mouse-1] #'acme-mode--mouse-2)
;;            ;; right-click
;;            (define-key acme-mode-map [M-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-double-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-triple-down-mouse-1] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-mouse-1] #'acme-mode--mouse-3)
;;            (define-key acme-mode-map [M-drag-mouse-1] #'acme-mode--mouse-3)
;;            ;; in ChromeOS, Alt-left-click gets auto-translated to right-click
;;            (define-key acme-mode-map [M-down-mouse-3] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-double-down-mouse-3] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-triple-down-mouse-3] #'acme-mode--down-mouse-3)
;;            (define-key acme-mode-map [M-mouse-3] #'acme-mode--mouse-3)
;;            (define-key acme-mode-map [M-drag-mouse-3] #'acme-mode--mouse-3)
;;            (setq acme-mode-keyboard-chord-keylist
;;                  '(("1" . 1) ("C-1" . 1) ("M-1" . 1)
;;                    ("2" . 2) ("C-2" . 2) ("M-2" . 2)
;;                    ("3" . 3) ("C-3" . 3) ("M-3" . 3)))))
;;     ;; enable `acme-mode'
;;     (acme-mode 1))
;;
;; Basic plumbing support is implemented, by specifying the
;; `acme-mode-plumbing-rules' customization variable, whose value
;; should be an associative list of the form
;; '((REGEXP1 . FUNCTION1) (REGEXP2 . FUNCTION2) ...)
;; where plumbed text is matched against the regular expressions
;; and the first match will have its function called with the
;; plumbed text. Should no regular expression in the associative
;; list keys match the plumbed text, the plumbed text is checked
;; to see if it is the path to an existing file and opened in a
;; new buffer window if so, or used to do a forward search in the
;; buffer otherwise. See the code for `acme-mode--plumb-python-error'
;; and the definition of `acme-mode-default-plumbing-rules' for
;; examples of plumbing regexps and functions. The default rules
;; include plumbing website URLs and Python error locations. Example:
;;
;;   (setq acme-mode-plumbing-rules
;;         '(("https?://.*" . browse-url)
;;           (" *File \"[a-zA-Z¡-￿0-9_./-]+\", line [0-9]+.*" . acme-mode--plumb-python-error))
;;
;; Original chording and file finding code is from
;; https://github.com/akrito/acme-mouse/blob/master/acme-mouse.el
;; (which incorporates code for Acme-like search from Dan McCarthy's
;; https://www.emacswiki.org/emacs/acme-search.el) that was adapted
;; into a minor mode, with modifications that include middle-click for
;; executing text in an external shell, expanded chording, plumbing,
;; tag implementation, Acme pipes (though no range selection), and
;; accommodations for trackpad use.
;;
;; TODO:
;; - Ranges for pipes like "12,23-/[Aa]bc/|tr '[abc]' '[123]'"
;;
;; Caveats:
;; - Redo is only supported when undo-tree-mode is enabled, see
;;   https://elpa.gnu.org/packages/undo-tree.html
;;
;; Out-of-scope for now:
;; - Edit (i.e., no structural regexps)
;; - Cut and Paste (use 1-2 and 1-3 chords)
;; - Delcol and Newcol (window management in Emacs is involved)
;; - Delete (use Del, or revert using Get then Del)
;; - Dump and Load (use `desktop-save' and `desktop-load-file')
;; - Exit (close using the menubar or title bar close button)
;; - Font (use the menubar)
;; - ID (window IDs as strings are not helpful for the user)
;; - Indent (use standard Emacs controls for this)
;; - Kill (use `kill-process')
;; - New (less useful without in-window Acme C-f path completion)
;; - Send (term implementations each have their own mechanism)
;; - Sort (window management in Emacs is involved)
;; - Tab (maybe later)
;;
;; Known bugs:
;; - When the same buffer is visible in multiple windows, the
;;   highlighting in non-selected windows can be wonky. Do
;;     (add-hook 'acme-mode-hook (lambda () (setq highlight-nonselected-windows nil)))
;;   if this is too distracting to turn off highlighting of regions
;;   in all non-selected windows. For more info, see email thread at
;;   https://lists.gnu.org/archive/html/emacs-devel/2015-03/msg01062.html

;;; Code:

(require 'ring)
(require 'subr-x)

(defgroup acme nil
  "Acme mouse emulation."
  :group 'environment
  :group 'editing
  :group 'mouse)

;; INTERNAL VARIABLES

;; Default plumbing rules
(defvar acme-mode-default-plumbing-rules
  '(("https?://.*" . browse-url)
    (" *File \"[~a-zA-Z¡-￿0-9_./-]+\", line [0-9]+.*" . acme-mode--plumb-python-error))
  "Default plumbing rules for Acme mode.

See `acme-mode-plumbing-rules'.")

;; Button values
(defvar acme-mode--lbutton 1)
(defvar acme-mode--mbutton 2)
(defvar acme-mode--rbutton 4)
(defvar acme-mode--allbuttons (+ acme-mode--lbutton
                                 acme-mode--mbutton
                                 acme-mode--rbutton))
(defvar acme-mode--nobuttons 0)

(defvar acme-mode--state 'noselect
  "State of Acme mode.

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

(defvar acme-mode--prior-delete-selection-mode nil
  "Whether Delete Selection mode was enabled prior to Acme mode.")

(defvar acme-mode--prior-highlight-nonselected-windows nil
  "Whether highlighting of non-selected windows was active prior to Acme mode.")

(defvar acme-mode--prior-transient-mark-mode nil
  "Whether Transient Mark mode was enabled prior to Acme mode.")

(defvar acme-mode--most-recent-windows (make-ring 2)
  "Most recently visited windows.")

;; CUSTOMIZATION VARIABLES

(defcustom acme-mode-use-frames nil
  "Use new frames instead of windows when popping to a tag file or plumbed file."
  :type 'boolean)

(defcustom acme-mode-plumbing-rules acme-mode-default-plumbing-rules
  "Association list ((REGEXP . FUNCTION) ...) for plumb dispatch.

Whenever a string is plumbed by `acme-mode--plumb', it is
dispatched to the function associated with the first key in
`acme-mode-plumbing-rules' which regexp-matches the string.

The corresponding function is called with the plumbed string.

If there is no key that regexp-matches the plumbed string, it
is instead dispatched to `acme-mode--find-file-or-search'."
  :type '(alist :key-type (string :tag "Key") :value-type (function :tag "Value")))

(defcustom acme-mode-keyboard-chord-keylist '(("z" . 1) ("x" . 2) ("c" . 3))
  "List of (KEY MOUSEBUTTON) pairs to specify keyboard chord keys.

When a mouse button is held down in Acme mode, each specified KEY
will trigger a press-down and release of its associated MOUSEBUTTON."
  :type '(alist :key-type (string :tag "Key") :value-type (integer :tag "Mouse button")))

(defcustom acme-mode-exclude-major-modes '(completion-list-mode
                                           dired-mode
                                           ibuffer-mode
                                           info-mode
                                           minibuffer-inactive-mode
                                           minibuffer-mode)
  "List of major modes for which to not use Acme mode mouse interace.

Leverages approach from https://emacs.stackexchange.com/a/59509
for https://emacs.stackexchange.com/questions/59494 that shows
how to wrap/intercept commands bound to a given key in Emacs."
  :type '(list :tag "Major modes"
           (symbol :tag "Major mode")))

(defcustom acme-mode-tag-buffer-name "*Acme tag buffer*"
  "Base name of Acme mode tag buffer."
  :type 'string)

;; MODE DEFINITIONS AND FUNCTIONS

(defun acme-mode--enable ()
  "Setup for Acme mode."
  (setq acme-mode--prior-delete-selection-mode (symbol-value delete-selection-mode))
  (setq acme-mode--prior-highlight-nonselected-windows 'highlight-nonselected-windows)
  (setq acme-mode--prior-transient-mark-mode (symbol-value transient-mark-mode))
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  (setq highlight-nonselected-windows t)
  (add-hook 'post-command-hook 'acme-mode--update-most-recent-windows)
  (acme-mode--update-most-recent-windows))

(defun acme-mode--disable ()
  "Teardown for Acme mode."
  (delete-selection-mode acme-mode--prior-delete-selection-mode)
  (setq highlight-nonselected-windows acme-mode--prior-highlight-nonselected-windows)
  (transient-mark-mode acme-mode--prior-transient-mark-mode)
  (remove-hook 'post-command-hook 'acme-mode--update-most-recent-windows))

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
    (define-key map [drag-mouse-3] #'acme-mode--mouse-3)
    map)
  "Acme mode keymap.")

;; Mode-line and menu
(easy-menu-define acme-mode-menu acme-mode-map "Acme mode menu."
  '("Acme mode"
    [ "Open tag buffer" acme-mode-pop-tag-buffer t ]
    "--"
    [ "Turn off Acme mode" acme-mode t ]))

;;;###autoload
(define-minor-mode acme-mode
  "Acme mode, a global minor mode to replicate Plan 9 Acme mouse behavior.

When called interactively, toggle `acme-mode'. With prefix ARG,
enable `acme-mode' if ARG is positive, otherwise disable it.

When called from Lisp code, enable `acme-mode' if ARG is omitted,
nil or positive. If ARG is `toggle', toggle `acme-mode'.

\\{acme-mode-map}"
  :init-value nil
  :lighter " A"
  :keymap acme-mode-map
  :global t
  (if acme-mode
      (acme-mode--enable)
    (acme-mode--disable)))

;; HELPER FUNCTIONS

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

(defun acme-mode--clear-secondary-selection ()
  "Clears the secondary selection."
  (gui-set-selection 'SECONDARY nil)
  (delete-overlay mouse-secondary-overlay))

(defvar acme-mode--argtext nil
  "Cached argument for 2-1 chords.")

(defun acme-mode--get-active-region-text ()
  "Return selected text without properties, if any."
  (when (region-active-p)
    (let* ((str (buffer-substring (mark) (point)))
             (start 0)
             (end (length str)))
        (set-text-properties start end nil str)
        str)))

(defun acme-mode--update-argtext ()
  "Update `acme-mode--argtext' with selected region, if any."
  (if (region-active-p)
      (setq acme-mode--argtext (acme-mode--get-active-region-text))
    (setq acme-mode--argtext nil)))

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
         (let* ((mousepos (mouse-pixel-position))
                (frame (car mousepos))
                (x (cadr mousepos))
                (y (cddr mousepos))
                (posn (posn-at-x-y x y frame)))
           (list type posn)))
        (acme-mode--last-mouse-event
         (list type (nth 1 acme-mode--last-mouse-event)))
        (t
         (error "No last mouse event but `mouse-selection-click' greater than zero"))))

(defun acme-mode--make-keyboard-chord-transient-map ()
  "Set a transient key map to support `acme-mode-keyboard-chord-keylist'."
  (when acme-mode-keyboard-chord-keylist
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (elt acme-mode-keyboard-chord-keylist)
         (let ((key (car elt))
               (but (cdr elt)))
           (cond ((eql but 1)
                  (define-key map (kbd key)
                    (lambda () (interactive)
                      (acme-mode--down-mouse-1 (acme-mode--make-mouse-event 'down-mouse-1))
                      (acme-mode--mouse-1 (acme-mode--make-mouse-event 'mouse-1)))))
                 ((eql but 2)
                  (define-key map (kbd key)
                    (lambda () (interactive)
                      (acme-mode--down-mouse-2 (acme-mode--make-mouse-event 'down-mouse-2))
                      (acme-mode--mouse-2 (acme-mode--make-mouse-event 'mouse-2))
                      ;; Kludge to work around 3-2 chords sometimes sending only
                      ;; down-mouse-3 down-mouse-2 mouse-2 (no final mouse-3)
                      (when (eq acme-mode--state 'donothing)
                        (acme-mode--button-up acme-mode--rbutton)
                        (acme-mode--maybe-reset-state)))))
                 ((eql but 3)
                  (define-key map (kbd key)
                    (lambda (arg) (interactive "P")
                      (acme-mode--down-mouse-3 (acme-mode--make-mouse-event 'down-mouse-3) arg)
                      (acme-mode--mouse-3 (acme-mode--make-mouse-event 'mouse-3))
                      ;; Kludge to work around 2-3 chords sometimes sending only
                      ;; down-mouse-2 down-mouse-3 mouse-3 (no final mouse-2)
                      (when (eq acme-mode--state 'donothing)
                        (acme-mode--button-up acme-mode--mbutton)
                        (acme-mode--maybe-reset-state)))))
                 (t
                  (error "Unsupported button number")))))
       map)
     t nil)))

(defun acme-mode--pop-buffer-window (buffer-or-name &optional use-frames pop-above size)
  "Switch to buffer BUFFER-OR-NAME if visible, and if not open in new window.

If USE-FRAMES is t, switching can be across frames. If it is nil,
then switching is only on the same frame.

When opening BUFFER-OR-NAME in a new window, split a new window
below, switch to it, and switch the displayed buf to that of
BUFFER-OR-NAME.

If POP-ABOVE is non-nil, new windows are split above, not below.

SIZE is the size in rows of the new window, if any, that
follows the same convention as `split-window-vertically'."
  (let* ((win (get-buffer-window buffer-or-name use-frames)))
    (cond (win
           (progn
             (select-window win)
             (raise-frame)
             win))
          (use-frames
           (select-frame (make-frame-command))
           (switch-to-buffer buffer-or-name)
           (raise-frame)
           (selected-window))
          (t
           (split-window-vertically size)
           (unless pop-above              ; same buffer is shown in split windows,
             (other-window 1))          ; so only switch if popping above
           (switch-to-buffer buffer-or-name)
           (selected-window)))))

(defun acme-mode--pop-file-window (file-name &optional use-frames)
  "Switch to window with FILE-NAME if visible, and if not open in new window.

If USE-FRAMES is t, switching can be across frames. If it is nil,
then switching is only on the same frame.

When opening FILE-NAME in a new window, open it in a new buffer
if needed, split a new window below, switch to it, and switch the
displayed buf to that of FILE-NAME.

If FILE-NAME is not an existing file, this creates a buffer with
that name."
  (let ((buffer (find-file-noselect file-name)))
    (acme-mode--pop-buffer-window buffer use-frames)))

(defun acme-mode-pop-tag-buffer (arg)
  "Pops to an Acme mode tag buffer, optionally numbered with prefix ARG.

If the tag buffer is visible in a window, that window is
selected. If the tag buffer is not already visible, a window is
created showing that buffer. If the tag buffer does not exist, it
is created. If `acme-mode-use-frames' is nil, selection and
window creation are within the current frame. If
`acme-mode-use-frames' is t, selection can be across frames and
new windows are created in their own frames.

Tag buffers have name determined by `acme-mode-tag-buffer-name'.
Numbered tag buffers are additionally suffixed by <prefix ARG>.
If prefixed with an ARG that is not an integer, ARG is ignored.

To hide the tag buffer after, right-click on its modeline if it
is in a frame with two windows or more, or close the frame if it
occupies a frame by itself."
  (interactive "P")
  (let* ((this-frame (selected-frame))
         (buffer-name (concat acme-mode-tag-buffer-name
                              (when (integerp arg)
                                (concat "<" (number-to-string arg) ">"))))
         (maybe-buffer (get-buffer buffer-name))
         (buffer (or maybe-buffer
                     (generate-new-buffer buffer-name))))
    (acme-mode--pop-buffer-window buffer acme-mode-use-frames t 3)
    ;; Insert base tag keywords for new tag buffers
    (unless maybe-buffer
      (with-current-buffer buffer
        (insert "Del Snarf Get Undo Redo Put Zerox | Look ")))
    (when (eq this-frame (selected-frame))
      (other-window 1))))

(defun acme-mode--tag-buffer-p ()
  "Return t if the current buffer is the tag buffer, else nil."
  (string-prefix-p acme-mode-tag-buffer-name (buffer-name)))

(defun acme-mode--update-most-recent-windows ()
  "Update `acme-mode--most-recent-windows' with the current window."
  (let ((this-window (selected-window))
        (most-recent-window (if (ring-empty-p acme-mode--most-recent-windows)
                                nil
                              (ring-ref acme-mode--most-recent-windows 0))))
    ;; Only update most recent windows ring if current window is
    ;; neither a minibuffer window nor the most recent window
    (unless (or (eq this-window most-recent-window)
                (window-minibuffer-p this-window))
      (ring-insert acme-mode--most-recent-windows this-window))))

;; MOUSE FUNCTIONS

;; Button 1 down-press
(defun acme-mode--down-mouse-1 (event)
  "Acme mode handler for left-button press EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-down acme-mode--lbutton)
         (cond ((eq acme-mode--state 'noselect)
                (setq acme-mode--state 'textselect)
                (mouse-set-mark event)
                (acme-mode--make-keyboard-chord-transient-map)
                (mouse-drag-region event))
               ((eq acme-mode--state 'textselect2) ; 2-1 chord
                (setq acme-mode--state 'donothing)
                (acme-mode--execute event acme-mode--argtext))))))

;; Button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--mouse-1 (event)
  "Acme mode handler for left-button release EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--lbutton)
         (cond ((eq acme-mode--state 'textselect)
                (setq deactivate-mark nil)
                (mouse-set-point event)
                (setq transient-mark-mode (cons 'only t))
                (acme-mode--update-argtext)))
         (acme-mode--maybe-reset-state))))

;; Double button 1 release, no mouse pointer movement since down-mouse-1
(defun acme-mode--double-mouse-1 (event)
  "Acme mode handler for double or triple left-click EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--lbutton)
         (cond ((eq acme-mode--state 'textselect)
                (setq deactivate-mark nil)
                (mouse-set-point event)
                (acme-mode--select-region)
                (setq transient-mark-mode (cons 'only t))
                (acme-mode--update-argtext)))
         (acme-mode--maybe-reset-state))))

;; Button 1 release, mouse pointer moved since down-mouse-1
(defun acme-mode--drag-mouse-1 (event)
  "Acme mode handler for left-button drag release EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--lbutton)
         (cond ((eq acme-mode--state 'textselect)
                (setq deactivate-mark nil)
                (mouse-set-region event)
                (setq transient-mark-mode (cons 'only t))
                (acme-mode--update-argtext)))
         (acme-mode--maybe-reset-state))))

;; Button 2 down-press
(defun acme-mode--down-mouse-2 (event)
  "Acme mode handler for middle-button press EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-down acme-mode--mbutton)
         (cond ((eq acme-mode--state 'noselect)
                (setq acme-mode--state 'textselect2)
                (acme-mode--make-keyboard-chord-transient-map)
                (acme-mode--mouse-drag-secondary event))
               ((eq acme-mode--state 'textselect)
                (setq acme-mode--state 'textselect-cut)
                (mouse-set-point event)
                (acme-mode--select-region)
                (kill-region (mark) (point)))
               ((eq acme-mode--state 'textselect-paste)
                (setq acme-mode--state 'textselect-cut)
                (call-interactively 'undo))
               ((eq acme-mode--state 'textselect3)
                (acme-mode--clear-secondary-selection)
                (setq acme-mode--state 'donothing))))))

;; Button 2 release, no mouse movement since down-mouse-2
(defun acme-mode--mouse-2 (event)
  "Acme mode handler for middle-button release EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--mbutton)
         (cond ((eq acme-mode--state 'textselect2)
                (acme-mode--execute event)))
         (acme-mode--maybe-reset-state))))

;; Button 3 down-press
(defun acme-mode--down-mouse-3 (event arg)
  "Acme mode handler for right-button press EVENT.

Specify a prefix ARG to insert a specific kill ring entry.

For example, '<down-mouse-left> Control-u 3 <down-mouse-right>'
will insert the 3rd most recent entry in the kill ring."
  (interactive "e\nP")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-down acme-mode--rbutton)
         (cond ((eq acme-mode--state 'noselect)
                (setq acme-mode--state 'textselect3)
                (acme-mode--make-keyboard-chord-transient-map)
                (acme-mode--mouse-drag-secondary event))
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
                (acme-mode--clear-secondary-selection)
                (setq acme-mode--state 'donothing))))))

;; Button 3 release
(defun acme-mode--mouse-3 (event)
  "Acme mode handler for right-button release EVENT."
  (interactive "e")
  (cond ((member major-mode acme-mode-exclude-major-modes)
         (let ((acme-mode nil))
           (call-interactively (key-binding (this-command-keys)))))
        (t
         (acme-mode--update-last-mouse-events event)
         (acme-mode--button-up acme-mode--rbutton)
         (cond ((eq acme-mode--state 'textselect3)
                (acme-mode--plumb event)))
         (acme-mode--maybe-reset-state))))

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

(defun acme-mode--search (sym &optional no-warp)
  "Search forward for the next occurence of SYM.

When searching forward, the mouse is warped to the search result
if one exists, unless NO-WARP is non-nil."
  (if (search-forward sym nil t)
      (acme-mode--highlight-search sym)
    (let ((saved-point (point)))
      (message "Wrapped search")
      (goto-char (point-min))
      (if (search-forward sym nil t)
          (acme-mode--highlight-search sym)
        (goto-char saved-point))))
  ;; recenter screen if search result is beyond the viewport
  (unless (posn-at-point)
    (universal-argument)
    (recenter))
  ;; warp the mouse to the result
  (unless no-warp
    (acme-mode--move-mouse-to-point)))

(defun acme-mode--find-file (filename)
  "Find given FILENAME in another window if it exists.

FILENAME may be specified with a linenumber and a column number:
  <filepath>
  <filepath>:<linenum>
  <filepath>:<linenum>:<colnum>

If the given file is successfully opened, the function returns t.
If the given file does not exist, the function returns nil."
  (let ((filepath)
        (linenum)
        (colnum))
   (save-match-data
     (cond ((string-match "\\([~.a-zA-Z¡-￿0-9_/@-]*[a-zA-Z¡-￿0-9_/-]\\):\\([0-9]+\\)[:.]\\([0-9]+\\)" filename)
            (setq filepath (match-string 1 filename)
                  linenum (string-to-number (match-string 2 filename))
                  colnum (string-to-number (match-string 3 filename))))
           ((string-match "\\([~.a-zA-Z¡-￿0-9_/@-]*[a-zA-Z¡-￿0-9_/-]\\):\\([0-9]+\\)" filename)
            (setq filepath (match-string 1 filename)
                  linenum (string-to-number (match-string 2 filename))))
           ((string-match "\\([~.a-zA-Z¡-￿0-9_/@-]*[a-zA-Z¡-￿0-9_/-]\\)" filename)
            (setq filepath (match-string 1 filename))))
     (when (and filepath
                (file-readable-p filepath))
       (acme-mode--pop-file-window filepath acme-mode-use-frames)
       (when linenum
         (goto-char (point-min))
         (forward-line (1- linenum)))
       (when colnum
         (forward-char (1- colnum)))
       t))))

(defun acme-mode--find-file-or-search (sym)
  "Open SYM if it is a file path, else search forward for its next occurence.

When searching forward, the mouse is warped to the search result
if one exists."
  (or (acme-mode--find-file sym)
      (acme-mode--search sym)))

(defun acme-mode--get-seltext (event thing)
  "Get text for plumbing based on EVENT, THING, and selections.

THING should be one of the choices from `thing-at-point'.

Priority order is secondary selection if it exists, then selected
text if EVENT position is within the selected text, then the
THING-at-point."
  (let* ((posn (event-end event))
         (bufpos (posn-point posn))
         (win (posn-window posn)))
    (with-selected-window win
      (cond ((secondary-selection-exist-p)
             (gui-get-selection 'SECONDARY))
            ((and (region-active-p)
                  (>= bufpos (min (mark) (point)))
                  (<= bufpos (max (mark) (point))))
             (buffer-substring (mark) (point)))
            (t
             (let* ((restorepointevent (list 'mouse-1 (posn-at-point)))
                    (_ (mouse-set-point event))
                    (seltext (thing-at-point thing)))
               (mouse-set-point restorepointevent)
               seltext))))))

(defun acme-mode--plumb (event)
  "Plumb selected text or symbol at EVENT position."
  (let ((sym (acme-mode--get-seltext event 'filename)))
    (acme-mode--clear-secondary-selection)
    (when sym
      ;; See https://emacs.stackexchange.com/questions/69743/use-regex-as-key-car-in-alist
      (let ((res (assoc sym acme-mode-plumbing-rules 'string-match-p)))
        (if res
            (funcall (cdr res) sym)
          (acme-mode--find-file-or-search sym))))))

(defun acme-mode--execute-special-command (command)
  "Acme mode executor for special COMMAND keywords."
  (cond ((string-equal command "Del")
         (if (buffer-modified-p)
             (message "Buffer modified. Save before closing.")
           (kill-buffer-and-window))
         (sleep-for 0.2)
         t)
        ((string-equal command "Get")
         (revert-buffer)
         (sleep-for 0.2)
         t)
        ((string-equal command "Look")
         (let ((event (list 'mouse-3 (posn-at-point))))
           (let ((sym (acme-mode--get-seltext event 'filename)))
             (acme-mode--clear-secondary-selection)
             (when sym
               (acme-mode--search sym t)
               ;; Kludge, wait to update so users are guided into
               ;; clicking slower, else events have incorrect position
               (sleep-for 0.2)))
           t))
        ((string-equal command "Put")
         (save-buffer)
         (sleep-for 0.2)
         t)
        ((string-equal command "Putall")
         (save-some-buffers)
         (sleep-for 0.2)
         t)
        ((string-equal command "Redo")
         (if (fboundp 'undo-tree-redo)
             (progn
               (deactivate-mark)
               (undo-tree-redo))
           (message "Redo is supported only when undo-tree-mode is enabled."))
         (undo-more 1)
         (sleep-for 0.2)
         t)
        ((string-equal command "Snarf")
         (when (region-active-p)
           (setq deactivate-mark nil)
           (kill-ring-save (mark) (point))
           (sleep-for 0.2))
         t)
        ((string-equal command "Undo")
         (if (fboundp 'undo-tree-undo)
             (undo-tree-undo)
           (undo-only))
         (sleep-for 0.2)
         t)
        ((string-equal command "Zerox")
         (if acme-mode-use-frames
             (make-frame-command)
           (split-window-below))
         (sleep-for 0.2)
         t)))

(defun acme-mode--execute-dispatch (command)
  "Acme mode dispatcher for executing a given COMMAND."
  ;; Priority order: special command, execute in an external shell
  (let ((command (string-trim command)))
    (unless (acme-mode--execute-special-command command)
      (let* ((seltext (acme-mode--get-active-region-text))
             (end (point))
             (start (if seltext (mark) end))
             (command-type (cond ((string-prefix-p "<" command) 'insert)
                                 ((string-prefix-p "|" command) 'replace)
                                 ((string-prefix-p ">" command) 'pipe)
                                 (t nil)))
             (command (if command-type
                          (string-trim (substring command 1))
                        command))
             ;; Append to instead of overwriting output buffer
             (shell-command-dont-erase-buffer t)
             ;; Use a temp buffer as needed
             (temp-buffer (generate-new-buffer "*Acme mode temp buffer*")))
        (unwind-protect
            (cond ((eq command-type 'insert)
                   (when seltext
                     (let ((left (min start end))
                           (right (max start end)))
                       (setq start left)
                       (setq end right)))
                   (delete-region start end)
                   (shell-command-on-region start start command temp-buffer t))
                  ((eq command-type 'replace)
                   (shell-command-on-region start end command temp-buffer t))
                  ((eq command-type 'pipe)
                   (shell-command-on-region start end command nil nil))
                  (t
                   (shell-command command)))
          (kill-buffer temp-buffer))))))

(defun acme-mode--execute (event &optional arg)
  "Execute selected text or sexp at EVENT posn, with optional ARG."
  (let ((sym (acme-mode--get-seltext event 'sexp))) ; sexp to get leading <, | or >
    (acme-mode--clear-secondary-selection)
    (if (acme-mode--tag-buffer-p)
        (when (> (ring-length acme-mode--most-recent-windows) 1)
          (let ((apply-window (ring-ref acme-mode--most-recent-windows 1)))
            (with-selected-window apply-window
              (acme-mode--execute-dispatch (string-join (list sym arg) " ")))))
      (acme-mode--execute-dispatch (string-join (list sym arg) " ")))))

;; PLUMBING FUNCTIONS

(defun acme-mode--plumb-python-error (error-line)
  "Function to plumb a Python ERROR-LINE.

Specifically, this parses lines like
  File \"somefile.py\", line 5
and opens the relevant file at the appropriate line in a new window."
  (save-match-data
    (and (string-match " *File \"\\([~a-zA-Z¡-￿0-9_./-]+\\)\", line \\([0-9]+\\).*" error-line)
         (acme-mode--find-file
          (concat (match-string 1 error-line)
                  ":"
                  (match-string 2 error-line))))))


;; OTHER HELPER FUNCTIONS (POTENTIALLY REPLACEABLE BY BUILT-IN EMACS FUNCTIONS)

(defun acme-mode--mouse-drag-secondary (start-event)
  "Set the secondary selection to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event that is START-EVENT.
vThe function returns a non-nil value if it creates a secondary selection.

This is a duplicated `mouse-drag-secondary' but modified to use
`set-transient-map' like `'mouse-drag-track' (the original
function eats the first non-movement event so a simple mouse 3
press-drag-release will not result in an immediate forward
search), and without the 1-second wait if there is no selected
region (e.g., a click without dragging)."
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* ((echo-keystrokes 0)
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (click-count (1- (event-click-count start-event)))
         (old-track-mouse track-mouse))
    (with-current-buffer (window-buffer start-window)
      (setq mouse-secondary-click-count click-count)
      (if (> (mod click-count 3) 0)
          ;; Double or triple press: make an initial selection
          ;; of one word or line.
          (let ((range (mouse-start-end start-point start-point click-count)))
            (set-marker mouse-secondary-start nil)
            (move-overlay mouse-secondary-overlay (car range) (nth 1 range)
                          (window-buffer start-window)))
        ;; Single-press: cancel any preexisting secondary selection.
        (or mouse-secondary-start
            (setq mouse-secondary-start (make-marker)))
        (set-marker mouse-secondary-start start-point)
        (delete-overlay mouse-secondary-overlay))
      ;; Use a transient map like in `mouse-drag-track'
      (setq track-mouse 'drag-tracking)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [switch-frame] #'ignore)
         (define-key map [select-window] #'ignore)
         (define-key map [mouse-movement]
           (lambda (event) (interactive "e")
             (let* ((end (event-end event))
                    (end-point (posn-point end)))
               (cond ((and (eq (posn-window end) start-window)
                           (integer-or-marker-p end-point))
                      (let ((range (mouse-start-end start-point end-point click-count)))
                        (if (or (/= start-point end-point)
                                (null (marker-position mouse-secondary-start)))
                            (progn
                              (set-marker mouse-secondary-start nil)
                              (move-overlay mouse-secondary-overlay
                                            (car range) (nth 1 range))))))
                     (t
                      (let ((mouse-row (cdr (cdr (mouse-position)))))
                        (cond
                         ((null mouse-row))
                         ((< mouse-row top)
                          (mouse-scroll-subr start-window (- mouse-row top)
                                             mouse-secondary-overlay start-point))
                         ((>= mouse-row bottom)
                          (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                             mouse-secondary-overlay start-point)))))))))
         map)
       t
       (lambda ()
         (setq track-mouse old-track-mouse)
         (if (marker-position mouse-secondary-start)
             (progn
               (delete-overlay mouse-secondary-overlay)
               (gui-set-selection 'SECONDARY nil)
               nil)
           (gui-set-selection
            'SECONDARY
            (buffer-substring (overlay-start mouse-secondary-overlay)
                              (overlay-end mouse-secondary-overlay)))))))))

(provide 'acme)

;;; acme.el ends here
