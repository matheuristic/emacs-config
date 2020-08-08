;;; org-readitlater.el --- bookmarking and web archiving for Org mode.

;; Modified work Copyright (C) 2020 matheuristic
;; Original work Copyright (C) 2016-2019 Charles A. Roelli

;; Author: matheuristic
;; Version: 0.1
;; Keywords: org, bookmarks, archives
;; Homepage: https://github.com/matheuristic/emacs-config
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; org-readitlater uses `org-attach' and a user-defined backend
;; (`monolith' by default) to provide bookmarking and web archiving
;; directly from an Org file. Every snapshot is logged and saved to an
;; automatically generated folder, and snapshots for the same link can
;; be compared using `zdiff` or `ediff'. Arbitrary functions can also
;; be run after an archive, allowing for extensive user customization.
;;
;; Commands defined here:
;;
;;   `org-readitlater-archive', `org-readitlater-archive-dry-run',
;;   `org-readitlater-cancel', `org-readitlater-delete-all',
;;   `org-readitlater-diff', `org-readitlater-diff3',
;;   `org-readitlater-new', `org-readitlater-open',
;;   `org-readitlater-run-after-archive-function'.
;;
;; Functions defined here:
;;
;;   `org-readitlater-expand-regexp-alist',
;;   `org-readitlater-extend-default-path',
;;   `org-readitlater-make-timestamp', `org-readitlater-open-with',
;;   `org-readitlater-test-after-archive-function',
;;   `org-readitlater-thing-at-point', `org-readitlater-backend-call',
;;   `org-readitlater-backend-process-sentinel-function'.
;;
;; Variables defined here:
;;
;;   `org-readitlater-after-archive-functions',
;;   `org-readitlater-archive-date-format',
;;   `org-readitlater-default-browser',
;;   `org-readitlater-domain-regexp-alist',
;;   `org-readitlater-log-backend-invocation',
;;   `org-readitlater-backend-program',
;;   `org-readitlater-backend-show-buffer',
;;   `org-readitlater-backend-options',
;;   `org-readitlater-make-relative'.
;;
;; Keymap defined here:
;;
;;   `org-readitlater-keymap'.
;;
;; Functions advised here:
;;
;;   `org-thing-at-point', with `org-readitlater-thing-at-point'.
;;
;;; Documentation:
;;
;;;; * Motivation
;;
;;  org-readitlater is a bookmarking and web archival system for Emacs
;;  Org mode, building on ideas from Pinboard <https://pinboard.in>.
;;  It archives your bookmarks so that you can access them even when
;;  you're not online, or when the site hosting them goes down.
;;  Archival is done using a user-defined backend like `monolith'.
;;  This means you can download whole sites for archival with a couple
;;  of keystrokes, while keeping track of your archives from a simple
;;  Org file.
;;
;;  org-readitlater is forked from org-board
;;  https://github.com/scallywag/org-board and modified to support
;;  generic user-defined backends rather than hardcoded to use `wget'.
;;  The original intent of the fork is to allow usage of `monolith' so
;;  that each archived URL is single HTML file with embedded images,
;;  CSS and Javascript.
;;
;;;; * Summary
;;
;;  In org-readitlater, a bookmark is represented by an Org heading of
;;  any level, with a `URL' property containing exactly one URL. Once
;;  such a heading is created, a call to `org-readitlater-archive'
;;  creates a unique ID and directory for the entry via `org-attach',
;;  archives the contents and requisites of the page(s) listed in the
;;  `URL' property using the configured backend, and saves them inside
;;  the entry's directory. A link to the (timestamped) root archive
;;  folder is created in the property `ARCHIVED_AT'. Multiple archives
;;  can be made for each entry. Additional options for the backend can
;;  be specified via the property `READITLATER_BACKEND_OPTIONS'. The
;;  variable `org-readitlater-after-archive-functions' (defaulting to
;;  nil) holds a list of functions to run after each archival
;;  operation.
;;
;;;; * User commands
;;
;;  `org-readitlater-archive' archives the current entry, creating a
;;    unique ID and directory via `org-attach' if necessary.
;;
;;  `org-readitlater-archive-dry-run' shows the backend program
;;    invocation for this entry in the echo area.
;;
;;  `org-readitlater-new' prompts for a URL to add to the current
;;    entry's properties, then archives the entry immediately.
;;
;;  `org-readitlater-delete-all' deletes all the archives for this
;;    entry by deleting the `org-attach' directory.
;;
;;  `org-readitlater-open' opens the bookmark at point in a browser.
;;    Default to the built-in browser, `eww', and with prefix, the
;;    native operating system browser.
;;
;;  `org-readitlater-diff' uses `zdiff` if available or `ediff' to
;;    recursively diff two archives of the same entry.
;;
;;  `org-readitlater-diff3' uses `ediff' to recursively diff three
;;    archives of the same entry.
;;
;;  `org-readitlater-cancel' cancels the current org-readitlater
;;    archival process.
;;
;;  `org-readitlater-run-after-archive-function' prompts for a
;;    function and an archive in the current entry, and applies the
;;    function to the archive.
;;
;;  These are all bound in the `org-readitlater-keymap' variable (not
;;  bound to any key by default).
;;
;;;; * Customizable options
;;
;;  `org-readitlater-backend-program' is the path to the backend
;;    archival program.
;;
;;  `org-readitlater-backend-options' are the command line options to
;;    use with the backend, specific as a list. This defaults to nil.
;;
;;  `org-readitlater-backend-show-buffer' controls whether the
;;    archival process buffer is shown in a window (defaults to true).
;;
;;  `org-readitlater-log-backend-invocation' controls whether to log
;;    the archival process command in the root of the archival
;;    directory (defaults to true).
;;
;;  `org-readitlater-domain-regexp-alist' applies certain options when
;;    a domain matches a regular expression. See the docstring for
;;    details. As an example, this is used to make sure that the
;;    backend does not send a User Agent string when archiving from
;;    Google Cache, which will not normally serve pages to it.
;;
;;  `org-readitlater-after-archive-functions' (default nil) holds a
;;    list of functions to run after an archival takes place. This is
;;    intended for user extensions to `org-readitlater'. The functions
;;    receive three arguments: the URL downloaded, the folder name
;;    where they were downloaded and the process filter event string
;;    (see the Elisp manual for details on the possible values of this
;;    string). For an example use of
;;    `org-readitlater-after-archive-functions', see the "Example
;;    usage" section below.
;;
;;  `org-readitlater-make-relative' (default nil) makes the stored
;;    link in mode to be relative to the org file holding it.
;;
;;;; * Known limitations
;;
;;  At the moment, only one archive can be done at a time.
;;
;;;; * Example usage
;;
;;;;; ** Archiving
;;
;;  Suppose you found a list of articles on linkers that you want to
;;  bookmark and keep locally for offline reading. In a dedicated org
;;  file for bookmarks you would create this entry:
;;
;;  ** TODO Linkers (20-part series)
;;  :PROPERTIES:
;;  :URL:                       http://a3f.at/lists/linkers
;;  :END:
;;
;;  The `URL' property is a page that already lists the URL that you
;;  want to download. With point inside the entry, you would run "M-x
;;  org-readitlater-archive". An `org-attach' directory will be
;;  created and the configured archiving backend starts downloading
;;  the pages to it. Afterwards, the entry will look like this:
;;
;;  ** TODO Linkers (20-part series)
;;  :PROPERTIES:
;;  :URL:                       http://a3f.at/lists/linkers
;;  :READITLATER_BACKEND_OPTIONS: --isolate
;;  :ID:                        D3BCE79F-C465-45D5-847E-7733684B9812
;;  :ARCHIVED_AT:               [2016-08-30-Tue-15-03-56]
;;  :END:
;;
;;  The value in the `ARCHIVED_AT' property is a link that points to
;;  the root of the timestamped archival directory. The ID property
;;  was automatically generated by `org-attach'.
;;
;;;;; ** Diffing
;;
;;  You can diff between two archives done for the same entry using
;;  `org-readitlater-diff', so you can see how a page has changed over
;;  time. The diff will highlight any changes that have been made
;;  using `zdiff` if available or `ediff'. `org-readitlater-diff3'
;;  also offers diffing between three different archives.
;;
;;;;; ** `org-readitlater-after-archive-functions'
;;
;;  `org-readitlater-after-archive-functions' is a list of functions
;;  run after an archive is finished. You can use it to do anything
;;  you like with newly archived pages. For example, you could add a
;;  function that copies the new archive to an external hard disk, or
;;  opens the archived page in your browser as soon as it is done
;;  downloading. You could also, for instance, copy all of the media
;;  files that were downloaded to your own media folder, and pop up a
;;  Dired buffer inside that folder to give you the chance to organize
;;  them.
;;
;;  Here is an example function that copies the archived page to an
;;  external service called `IPFS' <http://ipfs.io/>, a decentralized
;;  versioning and storage system geared towards web content (thanks
;;  to Alan Schmitt):
;;
;;  (defun org-readitlater-add-to-ipfs (url output-folder event &rest _rest)
;;    "Add the downloaded site to IPFS."
;;    (unless (string-match "exited abnormally" event)
;;      (let* ((parsed-url (url-generic-parse-url url))
;;             (domain (url-host parsed-url))
;;             (path (url-filename parsed-url))
;;             (output (shell-command-to-string
;;                   (concat "ipfs add -r "
;;                           (concat output-folder domain))))
;;             (ipref
;;           (nth 1 (split-string
;;                   (car (last (split-string output "\n" t))) " "))))
;;        (with-current-buffer (get-buffer-create "*org-readitlater-post-archive*")
;;          (princ (format "your file is at %s\n"
;;                      (concat "http://localhost:8080/ipfs/" ipref path))
;;              (current-buffer))))))
;;
;;  (eval-after-load "org-readitlater"
;;    '(add-hook 'org-readitlater-after-archive-functions 'org-readitlater-add-to-ipfs))
;;
;;  Note that for forward compatibility, it's best to add to a final
;;  `&rest' argument to every function listed in
;;  `org-readitlater-after-archive-functions', since a future update
;;  may provide each function with additional arguments (like a marker
;;  pointing to a buffer position where the archive was initiated, for
;;  example).
;;
;;  For more info on `org-readitlater-after-archive-functions', see
;;  its docstring and `org-readitlater-test-after-archive-function'.
;;
;;  You can also interactively run an after-archive function with the
;;  command `org-readitlater-run-after-archive-function'. See its
;;  docstring for details.
;;
;;;;
;;;; * Getting started
;;
;;;;; ** Installation
;;
;;  Clone this file to a directory and add the directory to your
;;  load-path manually.
;;
;;  (add-to-list 'load-path "/path/to/org-readitlater")
;;  (require 'org-readitlater)
;;
;;  Alternatively, you can download the package directly from Emacs
;;  using MELPA <https://github.com/melpa/melpa>.  M-x
;;  package-install RET org-readitlater RET will take care of it.
;;
;;;; ** Keybindings
;;
;;  The following keymap is defined in `org-readitlater-keymap':
;;
;;  | Key | Command                                    |
;;  | a   | org-readitlater-archive                    |
;;  | n   | org-readitlater-new                        |
;;  | k   | org-readitlater-delete-all                 |
;;  | o   | org-readitlater-open                       |
;;  | d   | org-readitlater-diff                       |
;;  | 3   | org-readitlater-diff3                      |
;;  | c   | org-readitlater-cancel                     |
;;  | x   | org-readitlater-run-after-archive-function |
;;  | O   | org-attach-reveal-in-emacs                 |
;;  | ?   | Show help for this keymap.                 |
;;
;;  To install the keymap give it a prefix key, e.g.:
;;
;;  (define-key org-mode-map (kbd "C-c o") org-readitlater-keymap)
;;
;;  `C-c o a' would run `org-readitlater-archive' in an Org buffer.
;;
;;;; * Miscellaneous
;;
;;  `org-readitlater-backend-program' should be customized to point to the
;;    backend to be used to retrieve the article from the URL.
;;
;;;;; ** Support for org-capture from Firefox (thanks to Alan Schmitt):
;;
;;  On the Firefox side, install org-capture from here:
;;
;;    http://chadok.info/firefox-org-capture/
;;
;;  Alternatively, you can do it manually by following the
;;  instructions here:
;;
;;    http://weblog.zamazal.org/org-mode-firefox/
;;      (in the “The advanced way” section)
;;
;;  When org-capture is installed, add `(require 'org-protocol)' to
;;  your init file (`~/.emacs').
;;
;;  Then create a capture template like this:
;;
;;    (setq org-readitlater-capture-file "my-org-readitlater.org")
;;
;;    (setq org-capture-templates
;;          `(...
;;            ("c" "capture through org protocol" entry
;;              (file+headline ,org-readitlater-capture-file "Unsorted")
;;              "* %?%:description\n:PROPERTIES:\n:URL: %:link\n:END:\n\n Added %U")
;;            ...))
;;
;;  And add a hook to `org-capture-before-finalize-hook':
;;
;;    (defun do-org-readitlater-dl-hook ()
;;      (when (equal (buffer-name)
;;              (concat "CAPTURE-" org-readitlater-capture-file))
;;        (org-readitlater-archive)))
;;
;;    (add-hook 'org-capture-before-finalize-hook 'do-org-readitlater-dl-hook)
;;
;;;; * Acknowledgements
;;
;;  Thanks to Alan Schmitt for the code to combine `org-readitlater' and
;;  `org-capture', and for the example function used in the
;;  documentation of `org-readitlater-after-archive-functions' above.
;;
;;; Code:

(require 'find-lisp)
(require 'org-attach)
(require 'url)                          ; See `org-readitlater-open'.
(require 'ztree nil t)                  ; Used for `ztree-diff', not required.
(declare-function ztree-diff "ztree")

(defgroup org-readitlater nil
  "Options concerning the bookmarking archival system."
  :tag "Org Readitlater"
  :group 'org
  :group 'hypermedia
  :prefix "org-readitlater-"
  :link '(url-link :tag "GitHub repository"
           "https://github.com/matheuristic/emacs-config")
  :link '(emacs-commentary-link :tag "Commentary" "org-readitlater"))

(defcustom org-readitlater-backend-program (executable-find "monolith")
  "The absolute path to the backend program binary.
Defaults to `monolith'."
  :type 'file)

(defcustom org-readitlater-backend-dest-format '("--output"
                                                 "%s/index.html")
  "List of format strings for populating the backend download destination.
There should be exactly one \"%s\" string format specification
for the destination directory."
  :type 'string)

(defcustom org-readitlater-domain-regexp-options-alist
  '(("webcache\\.googleusercontent\\.com.*" . ("--isolate"
                                               "--user-agent=\"\""))
    (".*" . ("--isolate")))
  "Alist with backend options to use for domains matching a regexp.
Only the first match is used, so it is a good idea to have the
car of the last entry match against catchall regexp \".*\" if
there are default options should be applied."
  :type '(alist :key-type regexp :value-type (list string)))

(defvar org-readitlater-after-archive-functions nil
  "Special hook run after archiving a site.
Each function there is called with three arguments:

 - the URL downloaded,
 - the folder name where the URL was downloaded to,
 - and the process filter event string.

Generally, if the event string matches \"exited abnormally\" then
something in the archive process went wrong.  The functions added
to this special hook should check for this case.

If the event string does not match \"exited abnormally\" then it
can be assumed that the download completed successfully.  If you
want to be completely sure, check that the string matches
\"finished\\n\" -- see (info \"(elisp) Sentinels\").

For interactive development of functions meant for
`org-readitlater-after-archive-functions', see
`org-readitlater-run-after-archive-function'.")

(defcustom org-readitlater-backend-show-buffer nil
  "Show the buffer with the backend output while it is running.

If the backend exited abnormally, the buffer will be shown
regardless."
  :type 'boolean)

(defcustom org-readitlater-log-backend-invocation t
  "Log the backend invocation to org-readitlater-{ID}.log.

The log is kept in the root of the timestamped archival folder.

You can use it as a shell script if you want to run it on another
machine, for example."
  :type 'boolean)

(defcustom org-readitlater-archive-date-format
  (if (or (eq system-type 'windows-nt)
          (eq system-type 'ms-dos)
          (eq system-type 'cygwin))
      'hyphenate
    'iso-8601)
  "String format for the archive folder name.

Can be either the symbol `hyphenate', or `iso-8601'.  `hyphenate'
is used on systems not supporting colons in filenames, while
`iso-8601' is used everywhere else."
  :type '(choice (const :tag "hyphenate: like 2016-08-18-Thu-20-19-02"
                   hyphenate)
                 (const :tag "iso-8601: like 2017-02-06T17:37:11+0100"
                   iso-8601)))

(defcustom org-readitlater-default-browser (if (require 'eww nil t)
                                               'eww
                                             'system)
  "Default browser for opening archived web pages.

`eww' is used if available, otherwise the page will be opened in
the system browser."
  :type '(choice (const :tag "Use `eww'" eww)
                 (const :tag "Use the native OS browser" system)))

(defcustom org-readitlater-make-relative t
  "Non-nil means make the resulting path link relative."
  :type 'boolean)

(defun org-readitlater-test-after-archive-function (url output-folder
                                                        event &rest _rest)
  "This is a template for designing post-archive functions.

URL is the URL to be archived. OUTPUT-FOLDER is the folder to
archive the URL to. EVENT is the process event text.

To add a function to `org-readitlater-after-archive-functions', use the
following code:

\(add-hook 'org-readitlater-after-archive-functions 'function-name).

Please note the `&rest' argument to the archive function.  This
is for forward compatibility with `org-readitlater' releases that might
one day make use of further arguments passed to
`org-readitlater-after-archive-functions'."

  (with-current-buffer (get-buffer-create
                        "*org-readitlater-post-archive*")
    (princ "Downloaded " (current-buffer))
    (princ url (current-buffer))
    (princ ".\n" (current-buffer))))

;;;###autoload
(defvar org-readitlater-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "a" 'org-readitlater-archive)
    (define-key keymap "r" 'org-board-archive-dry-run)
    (define-key keymap "n" 'org-readitlater-new)
    (define-key keymap "k" 'org-readitlater-delete-all)
    (define-key keymap "o" 'org-readitlater-open)
    (define-key keymap "d" 'org-readitlater-diff)
    (define-key keymap "3" 'org-readitlater-diff3)
    (define-key keymap "c" 'org-readitlater-cancel)
    (define-key keymap "x" 'org-readitlater-run-after-archive-function)
    (define-key keymap "O" 'org-attach-reveal-in-emacs)
    keymap)
  "Keymap for org-readitlater usage.")


;;; Internal functions

(defun org-readitlater-backend-process-sentinel-function (process event)
  "Outputs debug info to org-readitlater buffer on abnormal backend exit.

PROCESS is the org-readitlater process. EVENT is the process event
text.

Prints success message to echo area otherwise."

  (if (and (string-match-p "exited abnormally" event)
           (buffer-live-p (process-buffer process)))
      ;; If the process did not exit successfully, we copy the process
      ;; buffer output and append the event string to it, to present
      ;; the error to the user.
      (let ((inhibit-read-only t)
            (current-buffer-contents
             (with-current-buffer (process-buffer process)
               (buffer-string))))
        (with-output-to-temp-buffer (process-buffer process)
          (princ (concat current-buffer-contents
                         (combine-and-quote-strings
                          (process-command process))
                         " " event))))
    ;; Else, if the process exited successfully, inform the user.
    (if (string-match-p "finished" event)
        (message "org-readitlater finished archive for %s"
                 (process-get process 'org-entry))))
  (when org-readitlater-log-backend-invocation
    (ignore-errors
      (let ((backend-output-directory
             (process-get process 'backend-output-directory))
            (org-id-token
             (process-get process 'org-id)))
        (write-region (combine-and-quote-strings
                       (process-command process))
                      nil (concat backend-output-directory
                                  "org-readitlater-"
                                  org-id-token ".log")))))
  (run-hook-with-args 'org-readitlater-after-archive-functions
                      (process-get process 'url)
                      (process-get process 'backend-output-directory)
                      event))

(defun org-readitlater-backend-process-arg-list (path directory args site)
  "Return the `start-process' arg list for running the org-readitlater backend.

PATH is the absolute path to the backend binary.
DIRECTORY is the (unique) directory to save the archived file or files.
ARGS is a list of strings each containing a command line argument.
SITE is a URL to archive."
  (let* ((output-destination-options
          (mapcar (lambda (fstring) (format fstring directory))
                  org-readitlater-backend-dest-format))
         (output-buffer-name "org-readitlater-backend-call"))
    (append (list "org-readitlater-backend-process"
                  output-buffer-name
                  path)
            output-destination-options
            args
            (list site))))

(defun org-readitlater-backend-call (process-arg-list)
  "Start the backend in a temporary buffer.

PROCESS-ARG-LIST is a list that will be passed to
`start-process' to begin the backend. See the docstring for
`start-process' for what it should contain.

Returns the backend process."
  (let* ((output-buffer-name (nth 1 process-arg-list))
         (backend-process (apply 'start-process process-arg-list)))
    (if org-readitlater-backend-show-buffer
        (with-output-to-temp-buffer output-buffer-name
          (set-process-sentinel
           backend-process
           'org-readitlater-backend-process-sentinel-function))
      (set-process-sentinel
       backend-process
       'org-readitlater-backend-process-sentinel-function))
    backend-process))



;;;###autoload
(defun org-readitlater-archive ()
  "Archive the URL given by the current entry's `URL' property.

The attachment directory and the unique ID are created if not
already present.  See the docstring of `org-attach-dir'.

Every snapshot is stored in its own timestamped folder, and is
added as a link in the `ARCHIVED_AT' property."
  (interactive)
  ;; check that exactly one URL property is specified
  (let ((num-urls (length
                   (org-entry-get-multivalued-property (point) "URL"))))
    (when (/= num-urls 1)
      (error "Exactly one URL property must be specified")))
  (org-readitlater-expand-regexp-alist)
  (let* ((attach-directory (org-attach-dir t))
         (url (org-entry-get (point) "URL"))
         (options
          (org-entry-get-multivalued-property (point)
                                              "READITLATER_BACKEND_OPTIONS"))
         (timestamp (org-readitlater-make-timestamp))
         (output-directory (concat (file-name-as-directory attach-directory)
                                   (file-name-as-directory timestamp)))
         (org-id-token (org-id-get))
         (link-to-output (if (not org-readitlater-make-relative)
                             (concat "[[file:" output-directory "]["
                                     timestamp "]]")
                           (concat "[[file:" (file-relative-name output-directory)"][" timestamp "]]"))))
    (make-directory output-directory t)
    (let* ((process-arg-list (org-readitlater-backend-process-arg-list
                              org-readitlater-backend-program
                              output-directory
                              options
                              url))
           (backend-process (org-readitlater-backend-call
                             process-arg-list)))
      (process-put backend-process 'org-entry
                   (org-display-outline-path nil t "/" t))
      (process-put backend-process 'backend-output-directory
                   output-directory)
      (process-put backend-process 'org-id
                   org-id-token)
      (process-put backend-process 'url
                   url)
      (org-entry-add-to-multivalued-property (point) "ARCHIVED_AT"
                                             link-to-output))))

;;;###autoload
(defun org-board-archive-dry-run ()
  "Show the backend invocation that will be run, in the echo area.

This command takes into account the current options.  It also
creates an `org-attach' directory and property if not already
present."
  (interactive)
  ;; check that exactly one URL property is specified
  (let ((num-urls (length
                   (org-entry-get-multivalued-property (point) "URL"))))
    (when (/= num-urls 1)
      (error "Exactly one URL property must be specified")))
  (org-readitlater-expand-regexp-alist)
  (let* ((attach-directory (org-attach-dir t))
         (url (org-entry-get (point) "URL"))
         (options
          (org-entry-get-multivalued-property (point)
                                              "READITLATER_BACKEND_OPTIONS"))
         (timestamp (org-readitlater-make-timestamp))
         (output-directory (concat (file-name-as-directory attach-directory)
                                   (file-name-as-directory timestamp)))
         (process-arg-list (org-readitlater-backend-process-arg-list
                            org-readitlater-backend-program
                            output-directory
                            options
                            url)))
    (message (mapconcat 'identity
                        (cl-subseq process-arg-list 2)
                        " "))))

;;;###autoload
(defun org-readitlater-expand-regexp-alist ()
  "Add to `READITLATER_BACKEND_OPTIONS' w.r.t. `org-readitlater-domain-regexp-options-alist'."
  (let* ((url (org-entry-get (point) "URL"))
         (regexp-options
          (cl-some (lambda (pair) (if (string-match-p (car pair) url)
                                      (cdr pair)
                                    nil))
                   org-readitlater-domain-regexp-options-alist)))
    (dolist (org-readitlater-backend-option regexp-options)
      (org-entry-add-to-multivalued-property
       (point)
       "READITLATER_BACKEND_OPTIONS"
       org-readitlater-backend-option))))

;;;###autoload
(defun org-readitlater-make-timestamp ()
  "Return a timestamp suitable for the native operating system.

See also `org-readitlater-archive-date-format'."
  (cond ((eq org-readitlater-archive-date-format 'hyphenate)
         (format-time-string "%Y-%m-%d-%a-%H-%M-%S"
                             (current-time)))
        ((or (eq org-readitlater-archive-date-format 'iso-8601) t)
         (format-time-string "%FT%T%z"))))

;;;###autoload
(defun org-readitlater-delete-all ()
  "Delete all archives for the entry at point.

The parent attachment directory is not removed.  Note that ALL
attachments to the entry are deleted."
  (interactive)
  (org-attach-delete-all)
  (org-entry-delete (point) "ARCHIVED_AT"))

;;;###autoload
(defun org-readitlater-open (arg)
  "Open the archived page pointed to by the `URL' property.

With prefix argument ARG, temporarily flip the value of
`org-readitlater-default-browser' and open there instead.

If that does not work, open a list of HTML files from the
most recent archive, in Dired."
  (interactive "P")
  (let* ((link (car (last
                     (org-entry-get-multivalued-property
                      (point) "ARCHIVED_AT"))))
         (folder
          (expand-file-name
           (progn
             (string-match "^\\[\\[file:\\(.*\\)\\]\\[.*\\]\\]$" link)
             (match-string-no-properties 1 link))
           (file-name-directory (or buffer-file-name ""))))
         (url-string (org-entry-get (point) "URL")))
    (let* ((url-parsed (url-generic-parse-url url-string))
           (url-host-string (url-host url-parsed))
           (url-path-string (url-filename url-parsed))
           (url-combined-string (concat folder
                                        url-host-string
                                        url-path-string))
           ;; get all html files in the save folder
           (try-files (directory-files-recursively folder ".html")))
      ;; try opening first file, and throw to `find-name-dired' if
      ;; does not work
      (unless (and try-files
                   (eq (org-readitlater-open-with (car try-files) arg) 0))
        (find-name-dired folder "*.html")))))

;;;###autoload
(defun org-readitlater-open-with (filename-string arg)
  "Open FILENAME-STRING in default external program and return exit code.

Toggle the meaning of `org-readitlater-default-browser' if ARG is
non-nil."
  (when filename-string
    ;; With an argument and `system' for `org-readitlater-default-browser',
    ;; or no argument and `eww' for `org-readitlater-default-browser', try
    ;; to open the file in `eww' and return 0 (success), and if an
    ;; error occurs, throw it back to the user.
    (if (or (and arg (eq org-readitlater-default-browser 'system))
            (and (not arg) (eq org-readitlater-default-browser 'eww)))
        (condition-case nil
            (progn
              (message (format "opening %s" filename-string))
              (eww-open-file filename-string)
              0)
          (error 1))
      ;; Otherwise, use `open' on a Mac, `xdg-open' on GNU/Linux and
      ;; BSD, and prompt for a shell command otherwise.  (What would
      ;; be the best for Windows?)  Return the exit code of the
      ;; process call.
      (call-process (cond
                     ((eq system-type 'darwin) "open")
                     ((member system-type
                              '(gnu gnu/linux gnu/kfreebsd)) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    nil nil nil
                    filename-string))))

;;;###autoload
(defun org-readitlater-extend-default-path (filename-string)
  "Extend FILENAME-STRING to end in `/index.html'.

Examples: `aurox.ch'  => `aurox.ch/index.html'
          `aurox.ch/' => `aurox.ch/index.html'."
  (if (string= (substring filename-string -1) "/")
      (concat filename-string "index.html")
    (concat filename-string "/index.html")))

;;;###autoload
(defun org-readitlater-new (url)
  "Ask for a URL, create a property with it, and archive it."
  (interactive (list (completing-read "URL: " nil)))
  (org-entry-add-to-multivalued-property nil "URL" url)
  (org-readitlater-archive))

;;;###autoload
(defun org-readitlater-diff (archive1 archive2)
  "Recursively ARCHIVE1 and ARCHIVE2 (both directories)."
  (interactive
   (let ((dir-default (org-attach-dir)))
     (list (read-directory-name "Directory A to compare: "
                                dir-default nil 'must-match)
           (read-directory-name "Directory B to compare: "
                                dir-default nil 'must-match))))
  ;; We use `ztree' if it's available.  It doesn't do anything that
  ;; `ediff' can't, but it is more user-friendly and faster to use.
  (if (require 'ztree nil t)
      (ztree-diff archive1 archive2)
    (ediff-directories archive1 archive2 nil)))

;;;###autoload
(defun org-readitlater-diff3 (archive1 archive2 archive3)
  "Recursively diff ARCHIVE1, ARCHIVE2 and ARCHIVE3 (all directories)."
  (interactive
   (let ((dir-default (org-attach-dir)))
     (list (read-directory-name "Directory A to compare: "
                                dir-default nil 'must-match)
           (read-directory-name "Directory B to compare: "
                                dir-default nil 'must-match)
           (read-directory-name "Directory C to compare: "
                                dir-default nil 'must-match))))
  (ediff-directories3 archive1 archive2 archive3 nil))

;;;###autoload
(defun org-readitlater-cancel ()
  "Cancel the current org-readitlater archival process.

Leave the output buffer intact."
  (interactive)
  ;; Ideally, we should remove the link to the archive too.  But what
  ;; if the user wants to keep the partial download and resume it
  ;; later?  Maybe with a prefix argument only.
  (kill-process "org-readitlater-backend-process"))

(defun org-readitlater-run-after-archive-function (arg function archive)
  "Interactively run FUNCTION on ARCHIVE.  ARG is unused.

Run a function on an archive in the entry at point.  The function
should have the same format as recommended for
`org-readitlater-after-archive-functions'.  Prompt first for the
function to run, and then the name of the archive folder to run
it on.  With a prefix ARG, only prompt for functions already
present inside `org-readitlater-after-archive-functions'.

The function is provided with a successful exit string, as if the
archive has just been finished.

This is useful for debugging functions added to
`org-readitlater-after-archive-functions', or for interactively running
post-archive functions on select bookmarks."
  (interactive
   (list
    ;; ARG, not yet used by the function body.
    current-prefix-arg
    ;; FUNCTION
    (intern (completing-read "Function name: "
                             (if current-prefix-arg
                                 org-readitlater-after-archive-functions
                               obarray)
                             'functionp 'REQUIRE-MATCH))
    ;; ARCHIVE
    (read-directory-name
     "Archive directory (resembles a timestamp): "
     (org-attach-dir) nil 'must-match)))
  (let ((url (org-entry-get (point) "URL")))
    (funcall function url archive
             ;; See (info "(elisp) Sentinels").
             "finished\n")))

(provide 'org-readitlater)

;;; org-readitlater.el ends here
