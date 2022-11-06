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
;; For text execution, the following Acme keywords are supported:
;; - Del (kill window but no the buffer)
;; - Get (revert the buffer to the saved version)
;; - Indent (needs to be called with an arg, which can be on or off to
;;   enable or disable `electric-indent-local-mode', or ON or OFF to
;;   enable or disable `electric-indent-mode')
;; - Look (search for next occurrence optional arguments, treated as
;;   literal search strings, or if no arg then the highlighted region
;;   or word at point)
;; - Put (save buffer)
;; - Redo (requires undo-tree-mode)
;; - Snarf (copy region into kill ring)
;; - Spaces (needs to be called with an arg, which can be on or off to
;;   set `indent-tabs-mode' to t or nil locally, or ON or OFF for the
;;   same but also set the default value of `indent-tabs-mode')
;; - Tab (needs to be called with an arg, which should be a number
;;   that `tab-width' will be set to)
;; - Undo
;; - Zerox (create new window with the same buffer)
;;
;; Acme pipes <, | and > are implemented but not ranges:
;; - Selecting "abcdef" and executing "|tr '[abc]' '[123]' to replace
;;   the selected "abcdef" with "123def" will work.
;; - Executing ",|tr '[abc]' '[123]'" to change all a, b and c chars
;;   in the buffer to 1, 2 and 3 will not work.
;;
;; There is a limited tag implementation, in the form of a global tag
;; buffer that can be opened with the minor mode menu or by calling
;; `acme-mode-pop-buffer' (optionally with a prefix numeric argument
;; to open additional tag buffers). This is a regular buffer named
;; according to `acme-mode-tag-buffer-name' and it is automatically
;; populated with some executable keywords when created. It is
;; designed to be used in the following manner: select a target window
;; (where the operations are to be applied) and click or highlight in
;; in it as needed, then mouse 2 (execute) or mouse 3 (search) a word
;; or highlighted region in the global tag buffer without selecting
;; its window to the apply the corresponding operation on the selected
;; window. For example, if text "abcdef" is selected in a file window
;; and after that "|sed -e 's/abc/123/g'" is executed in a tag buffer
;; without selecting the tag buffer, then the text "abcdef" is changed
;; to "123def". To 2-1 chord in the tag buffer but apply the operation
;; on another selected window, highlight the argument text in the tag
;; buffer, then select the target window by clicking on its _modeline_
;; (important so argument text is not reset), then 2-1 chord the tag
;; buffer keyword without selecting the tag buffer. Note that the
;; above behavior holds for any buffer. It is just the tag buffer is
;; more convenient (easy to open, transient and sized appropriately).
;;
;; On trackpads, clicks with modifier keys pressed can be used to
;; simulate middle- and right-clicks, and keyboard keys can be used to
;; simulate a left-click, middle-click and right-click during a chord
;; (at least one mouse button held down), allowing for 1-2 and 1-3
;; chords. This can be enabled by adding the appropriate bindings to
;; `acme-mode-map' and customizing `acme-mode-keyboard-chord-keylist'
;; prior to enabling Acme mode (see sample configuration below).
;;
;; Sample configuration:
;;
;;   (require 'acme)
;;   (with-eval-after-load 'acme
;;     ;; Customize options
;;     (setq acme-mode-use-frames nil
;;           acme-mode-per-dir-shell-output nil)
;;     ;; Open tag buffer key binding
;;     (define-key acme-mode-map [C-M-return] #'acme-mode-pop-tag-buffer)
;;     ;; Trackpad support
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
;; - Delcol, Newcol and New (useful for window management)
;; - Ranges for pipes like "12,23-/[Aa]bc/|tr '[abc]' '[123]'"
;;   (for now select manually, e.g., "C-x h" for the whole buffer)
;;
;; Differences versus Plan 9 Acme:
;; - Window tags (see above description of tag buffer)
;; - Execution output is not streaming, only displays on shell return
;; - Indent ON and Indent OFF do not automatically apply its settings
;;   to already open buffers aside from the currently selected one,
;;   and these settings may be superceded by other major-mode or
;;   minor-mode bindings (e.g., in markdown-mode <return> is bound
;;   to `markdown-enter-key')
;; - Spaces keyword
;; - Del keyword closes the window but not the buffer (in Plan 9 Acme,
;;   when a file's last window is closed the file is closed too)
;; - Many others, in all likelihood
;;
;; Limitations:
;; - Redo is only supported when undo-tree-mode is enabled, see
;;   https://elpa.gnu.org/packages/undo-tree.html
;;
;; Out-of-scope for now:
;; - Edit (need to implement structural regexps)
;; - Cut and Paste (use 1-2 and 1-3 chords)
;; - Delete (keep to Emacs way of separating windows and buffers)
;; - Dump and Load (use `desktop-save' and `desktop-load-file')
;; - Exit (close using the menubar or title bar close button)
;; - Font (use the menubar)
;; - ID (window IDs as strings are not helpful for the user)
;; - Kill (use `kill-process')
;; - Send (term implementations each have their own mechanism)
;; - Sort (window management in Emacs is involved)
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
    ;; Python error locations
    (" *File \"[~a-zA-Z¡-￿0-9_./-]+\", line [0-9]+.*" . acme-mode--plumb-python-error)
    ;; EPUB files (open generically)
    ("[a-zA-Z¡-￿0-9_./\\(\\)&-][ a-zA-Z¡-￿0-9_./\\(\\)&-]*\\.[Ee][Pp][Uu][Bb]" . acme-mode--plumb-file-generic)
    ;; PDF files (open generically)
    ("[a-zA-Z¡-￿0-9_./\\(\\)&-][ a-zA-Z¡-￿0-9_./\\(\\)&-]*\\.[Pp][Dd][Ff]" . acme-mode--plumb-file-generic))
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

(defvar acme-mode--argtext nil
  "Cached argument for 2-1 chords.")

(defvar acme-mode--prior-delete-selection-mode nil
  "Whether Delete Selection mode was enabled prior to Acme mode.")

(defvar acme-mode--prior-highlight-nonselected-windows nil
  "Whether highlighting of non-selected windows was active prior to Acme mode.")

(defvar acme-mode--prior-transient-mark-mode nil
  "Whether Transient Mark mode was enabled prior to Acme mode.")

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

(defcustom acme-mode-per-dir-shell-output nil
  "Use a buffer per directory for Acme text execution shell output."
  :type 'boolean)

;; MODE DEFINITIONS AND FUNCTIONS

(defun acme-mode--enable ()
  "Setup for Acme mode."
  (setq acme-mode--prior-delete-selection-mode (symbol-value delete-selection-mode))
  (setq acme-mode--prior-highlight-nonselected-windows 'highlight-nonselected-windows)
  (setq acme-mode--prior-transient-mark-mode (symbol-value transient-mark-mode))
  (delete-selection-mode 1)
  (transient-mark-mode 1)
  (setq highlight-nonselected-windows t)
  (setq acme-mode--state 'noselect
        acme-mode--buttons acme-mode--nobuttons
        acme-mode--region-start nil
        acme-mode--last-mouse-event nil
        acme-mode--argtext nil))

(defun acme-mode--disable ()
  "Teardown for Acme mode."
  (delete-selection-mode acme-mode--prior-delete-selection-mode)
  (setq highlight-nonselected-windows acme-mode--prior-highlight-nonselected-windows)
  (transient-mark-mode acme-mode--prior-transient-mark-mode))

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
  '("Acme"
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

(defun acme-mode--pop-buffer-window (buffer-or-name &optional use-frames size side window)
  "Switch to buffer BUFFER-OR-NAME if visible, and if not open in new window.

If USE-FRAMES is t, switching can be across frames. If it is nil,
then switching is only on the same frame.

When opening BUFFER-OR-NAME in a new window, split a new window
below, switch to it, and switch the displayed buf to that of
BUFFER-OR-NAME.

If a new window is created, if SIZE is positive then the window
used for split will be SIZE lines tall after the split, if SIZE
is negative then split is so the new window is -SIZE lines tall,
and if SIZE is nil then the window split is done evenly. See
`split-window' for more about this parameter.

SIDE controls on which side the new window is opened if in the
same frame, and can be 'above , 'below , 'left , 'right or nil.
See `split-window' for more about this parameter.

If WINDOW is non-nil, split it instead of the current window if
creating a new window."
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
           (let ((new-win (split-window window size side)))
             (select-window new-win)
             (switch-to-buffer buffer-or-name)
             new-win)))))

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
         (this-win (selected-window))
         (buffer-name (concat acme-mode-tag-buffer-name
                              (when (integerp arg)
                                (concat "<" (number-to-string arg) ">"))))
         (maybe-buffer (get-buffer buffer-name))
         (buffer (or maybe-buffer
                     (generate-new-buffer buffer-name))))
    ;; Pop tag buffer in a small window and dedicate it to the tag buffer
    (let ((tag-win (acme-mode--pop-buffer-window buffer
                                                 acme-mode-use-frames
                                                 -3
                                                 'above
                                                 (frame-root-window))))
      (unless (window-dedicated-p tag-win)
        (set-window-dedicated-p tag-win t)))
    ;; Insert base tag keywords for new tag buffers
    (unless maybe-buffer
      (with-current-buffer buffer
        (insert "Del Snarf Get Undo Redo Put Zerox | Look ")))
    (when (eq this-frame (selected-frame))
      (select-window this-win))))

(defun acme-mode--tag-buffer-p ()
  "Return t if the current buffer is the tag buffer, else nil."
  (string-prefix-p acme-mode-tag-buffer-name (buffer-name)))

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

(defun acme-mode--find-file-or-search (seltext)
  "Open SELTEXT if it is a file path, else search forward for next occurrence.

When searching forward, the mouse is warped to the search result
if one exists."
  (or (acme-mode--find-file seltext)
      (acme-mode--search seltext)))

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
  (cond ((string-equal command "Del")   ; delete window but not buffer
         (when (or (not (buffer-modified-p))
                   (y-or-n-p "Buffer modified. Delete window anyway? "))
           (delete-window))
         ;; Kludge, wait to update so users are guided into
         ;; clicking slower, else events have incorrect position
         (sleep-for 0.2)
         t)
        ((string-equal command "Get")
         (revert-buffer)
         (sleep-for 0.2)
         t)
        ((string-prefix-p "Indent " command)
         (let ((arg (substring command 5)))
           (when (> (length arg) 0)
             (cond ((string-equal arg "on")
                    (electric-indent-local-mode 1))
                   ((string-equal arg "ON")
                    (electric-indent-local-mode 1)
                    (electric-indent-mode 1))
                   ((string-equal arg "off")
                    (electric-indent-local-mode 0))
                   ((string-equal arg "OFF")
                    (electric-indent-local-mode 0)
                    (electric-indent-mode 0))))
           (sleep-for 0.2)
           t))
        ((string-equal command "Look")
         (let ((event (list 'mouse-3 (posn-at-point))))
           (let ((seltext (acme-mode--get-seltext event 'filename)))
             (acme-mode--clear-secondary-selection)
             (when seltext
               (acme-mode--search seltext t)))
           (sleep-for 0.2)
           t))
        ((string-prefix-p "Look " command)
         (let ((seltext (substring command 5)))
           (when (> (length seltext) 0)
             (acme-mode--search seltext t))
           (sleep-for 0.2)
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
         (sleep-for 0.2)
         t)
        ((string-equal command "Snarf")
         (when (region-active-p)
           (setq deactivate-mark nil)
           (kill-ring-save (mark) (point)))
         (sleep-for 0.2)
         t)
        ((string-prefix-p "Spaces " command)
         (let ((arg (substring command 5)))
           (when (> (length arg) 0)
             (cond ((string-equal arg "on")
                    (setq indent-tabs-mode nil))
                   ((string-equal arg "ON")
                    (setq indent-tabs-mode nil)
                    (setq-default indent-tabs-mode nil))
                   ((string-equal arg "off")
                    (setq indent-tabs-mode t))
                   ((string-equal arg "OFF")
                    (setq indent-tabs-mode t)
                    (setq-default indent-tabs-mode nil))))
           (sleep-for 0.2)
           t))
        ((string-prefix-p "Tab " command)
         (let ((arg (substring command 5)))
           (when (> (length arg) 0)
             (setq tab-width (string-to-number arg)))
           (sleep-for 0.2)
           t))
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
             ;; Make sure shell command output does resize the minibuffer
             (max-mini-window-height 0.01)
             ;; Use directory specific output buffer
             (disp-buffer-name
              (let ((bname (buffer-name))
                    (fname (buffer-file-name)))
                (cond ((and acme-mode-per-dir-shell-output fname)
                       (concat "*Acme Shell Output*<" (file-name-directory fname) ">"))
                      ((string-prefix-p "*Acme Shell Output*" bname)
                       bname)
                      (t
                       "*Acme Shell Output*"))))
             (disp-buffer (or (get-buffer disp-buffer-name)
                              (generate-new-buffer disp-buffer-name)))
             ;; Use a temp buffer to cache output for insert or replace region
             (temp-buffer (generate-new-buffer "*Acme mode temp buffer*")))
        (unwind-protect
            (cond ((eq command-type 'insert)
                   (when seltext
                     (let ((left (min start end))
                           (right (max start end)))
                       (setq start left)
                       (setq end right)))
                   (delete-region start end)
                   (shell-command-on-region start start command temp-buffer t disp-buffer t))
                  ((eq command-type 'replace)
                   (shell-command-on-region start end command temp-buffer t disp-buffer t))
                  ((eq command-type 'pipe)
                   (unless (get-buffer-window disp-buffer)
                     (let ((win (selected-window)))
                       (acme-mode--pop-buffer-window disp-buffer nil)
                       (select-window win)))
                   (with-current-buffer disp-buffer
                     (goto-char (point-max)))
                   (shell-command-on-region start end command disp-buffer nil))
                  (t
                   (unless (get-buffer-window disp-buffer)
                     (let ((win (selected-window)))
                       (acme-mode--pop-buffer-window disp-buffer nil)
                       (select-window win)))
                   (with-current-buffer disp-buffer
                     (goto-char (point-max)))
                   (shell-command command disp-buffer)))
          (kill-buffer temp-buffer))))))

(defun acme-mode--execute (event &optional arg)
  "Execute selected text or sexp at EVENT posn, with optional ARG."
  (let ((sym (acme-mode--get-seltext event 'sexp))) ; sexp to get leading <, | or >
    (acme-mode--clear-secondary-selection)
    (acme-mode--execute-dispatch (string-join (list sym arg) " "))))

;; PLUMBING FUNCTIONS

(defun acme-mode--plumb-file-generic (filename)
  "Function to generically open given FILENAME using system 'xdg-open' or 'open'."
  (let ((system-open-command (or (executable-find "xdg-open")
                                 (executable-find "open"))))
    (if system-open-command
        (start-process "default-app" nil system-open-command filename)
      (message "No xdg-open or open on the system to generically open file."))))

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
         (start-buffer (window-buffer start-window))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds))))
         (click-count (1- (event-click-count start-event)))
         (old-track-mouse track-mouse))
    (with-current-buffer start-buffer
      (setq mouse-secondary-click-count click-count)
      (if (> (mod click-count 3) 0)
          ;; Double or triple press: make an initial selection
          ;; of one word or line.
          (let ((range (mouse-start-end start-point start-point click-count)))
            (set-marker mouse-secondary-start nil)
            (move-overlay mouse-secondary-overlay (car range) (nth 1 range)
                          start-buffer))
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
                                            (car range) (nth 1 range)
                                            start-buffer)))))
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
         (with-current-buffer start-buffer
           (if (marker-position mouse-secondary-start)
               (progn
                 (delete-overlay mouse-secondary-overlay)
                 (gui-set-selection 'SECONDARY nil)
                 nil)
             (gui-set-selection
              'SECONDARY
              (buffer-substring (overlay-start mouse-secondary-overlay)
                                (overlay-end mouse-secondary-overlay))))))))))

(provide 'acme)

;;; acme.el ends here
