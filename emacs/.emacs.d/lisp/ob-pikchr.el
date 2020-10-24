;;; ob-ditaa.el --- Babel Functions for pikchr        -*- lexical-binding: t; -*-

;; Copyright (C) 2020 matheuristic

;; Author: matheuristic

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides Org Babel support for evaluating Pikchr code.
;;
;; To use this, add to your Emacs initialization file either
;;   (org-babel-do-load-languages 'org-babel-load-languages '((pikchr . t)))
;; or
;;   (require 'ob-pikchr)
;; For more information, see https://orgmode.org/manual/Languages.html

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:pikchr
  '((:results . "file")
    (:exports . "results")
    (:svg-only . "yes")) ; yes - just SVG code, no - HTML wrapped SVG code
  "Default arguments for evaluating a pikchr source block.")

(defcustom org-babel-pikchr-cmd "pikchr"
  "Executable to use when evaluating pikchr blocks."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:pikchr (body params)
  "Execute a block of pikchr code with org-babel.
This function is called by `org-babel-execute-src-block'.

BODY contains the contents of the code block and PARAMS are an
alist containing the header key-value pairs of that code block."
  (let* ((out-file (or (cdr (assq :file params))
                       (error
                        "Pikchr requires \":file\" header argument")))
         (cmdline (cdr (assq :cmdline params)))
         (in-file (org-babel-temp-file "pikchr-"))
         (svg-only (cdr (assq :svg-only params)))
         (svg-only-args (cond ((string= svg-only "yes") "--svg-only")
                              ((string= svg-only "no") nil)
                              (t (error ":svg-only only supports \"yes\" or \"no\" values"))))
         (cmd (mapconcat #'identity
                         (list
                          org-babel-pikchr-cmd
                          svg-only-args
                          cmdline
                          (org-babel-process-file-name in-file)
                          ">"
                          (org-babel-process-file-name out-file)
                          )
                         " ")))
    (with-temp-file in-file (insert body))
    (message cmd) (shell-command cmd)
    nil)) ; signal that output has already been written to file

(defun org-babel-prep-session:pikchr (_session _params)
  "Return an error because pikchr does not support sessions."
  (error "Pikchr does not support sessions"))

(provide 'ob-pikchr)

;;; ob-pikchr.el ends here
