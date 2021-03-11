;;; j-help.el --- Documentation extention for j-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL: http://github.com/zellio/j-mode
;; Version: 1.1.1
;; Keywords: J, Languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; j-help provides access to the J software vocabulary via two functions
;; j-help-lookup-symbol and j-help-lookup-symbol-at-point. j-help-look-symbol
;; takes one string argument ( generally via the mini-buffer ) which it then
;; looks up.  j-help-lookup-symbol-at-point attempts to determine which symbol
;; is under your cursor and then passes that to j-help-lookup-symbol.
;;
;; The module provides the following key bindings for convenience
;;
;; * <kbd>C-c h</kbd> runs j-help-lookup-symbol
;; * <kbd>C-c C-h</kbd> j-help-lookup-symbol-at-point

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

(require 'cl-lib)
(require 'seq)

(unless (fboundp 'some)
  (defun some ( fn list )
    (when list
      (let ((val (funcall fn (car list))))
	(if val val (cl-some fn (cdr list)))))))

(unless (fboundp 'caddr)
  (defun caddr ( list )
    (car (cdr (cdr list)))))

(defgroup j-help nil
  "Documentation extension for j-mode."
  :group 'applications
  :prefix "j-help-")

(defcustom j-help-local-dictionary-url ""
  "Path to the local instance of the j-dictionary."
  :type 'string
  :group 'j-help)

(defcustom j-help-remote-dictionary-url "https://code.jsoftware.com/wiki/Vocabulary"
  "Path to the remote instance of the j-dictionary."
  :type 'string
  :group 'j-help)

(defcustom j-help-symbol-search-branch-limit 5
  "Distance from initial point the system can search for a valid symbol."
  :type 'integer
  :group 'j-help)

(defcustom j-help-browser-function 'browse-url-default-browser
  "Browser function used to open j-dictionary URLs.

Typical candidates are: `browse-url-default-browser', `eww-browse-url'."
  :type 'function
  :group 'j-help)

(defconst j-help-voc-alist
  '(("=" . "eq")       ("=." . "eqdot")       ("=:" . "eqco")       ; 0
    ("<" . "lt")       ("<." . "ltdot")       ("<:" . "ltco")
    (">" . "gt")       (">." . "gtdot")       (">:" . "gtco")
    ("_" . "under")    ("_." . "underdot")    ("_:" . "underco")
    ("+" . "plus")     ("+." . "plusdot")     ("+:" . "plusco")     ; 4
    ("*" . "star")     ("*." . "stardot")     ("*:" . "starco")
    ("-" . "minus")    ("-." . "minusdot")    ("-:" . "minusco")
    ("%" . "percent")  ("%." . "percentdot")  ("%:" . "percentco")
    ("^" . "hat")      ("^." . "hatdot")      ("^:" . "hatco")      ; 8
    ("$" . "dollar")   ("$." . "dollardot")   ("$:" . "dollarco")
    ("~" . "tilde")    ("~." . "tildedot")    ("~:" . "tildeco")
    ("|" . "bar")      ("|." . "bardot")      ("|:" . "barco")
    ("." . "dot")                                                   ; 12
    (":" . "cor")      (":." . "codot")       ("::" . "coco")
    ("," . "comma")    (",." . "commadot")    (",:" . "commaco")
    (";" . "semi")     (";." . "semidot")     (";:" . "semico")
    ("#" . "number")   ("#." . "numberdot")   ("#:" . "numberco")   ; 16
    ("!" . "bang")     ("!." . "bangdot")     ("!:" . "bangco")
    ("/" . "slash")    ("/." . "slashdot")    ("/:" . "slashco")
    ("\\" . "bslash")  ("\\." . "bslashdot")  ("\\:" . "slashco")
    ("[" . "squarelf")                        ("[:" . "squarelfco") ; 20
    ("]" . "squarert")
    ("{" . "curlylf")  ("{." . "curlylfdot")  ("{:" . "curlylfco")  ("{::" . "curlylfcoco")
    ("}" . "curlyrt")  ("}." . "curlyrtdot")  ("}:" . "curlyrtco")
    ("\"" . "quote")   ("\"." . "quotedot")   ("\":" . "quoteco")   ; 24
    ("`" . "grave")                           ("`:" . "graveco")
    ("@" . "at")       ("@." . "atdot")       ("@:" . "atco")
    ("&" . "ampm")     ("&." . "ampdot")      ("&:" . "ampco")      ("&.:" . "ampdotco")
    ("?" . "query")    ("?." . "querydot")
    ("a." . "adot")    ("a:" . "aco")         ("A." . "acapdot")    ; 29
    ("b." . "bdot")    ("C." . "ccapdot")     ("d." . "ddot")
    ("D." . "dcapdot") ("D:" . "dcapco")      ("e." . "edot")
    ("E." . "ecapdot") ("f." . "fdot")        ("F." . "fcap")       ("F:" . "fcap")
    ("F.." . "fcap")   ("F:." . "fcap")       ("F.:" . "fcap")      ("F::" . "fcap")
    ("H." . "hcapdot") ("i." . "idot")        ("i:" . "ico")        ; 33
    ("I." . "icapdot") ("j." . "jdot")        ("L." . "lcapdot")
    ("L:" . "lcapco")  ("M." . "mcapdot")     ("NB." . "ncapbcapdot")
    ("o." . "odot")    ("p." . "pdot")        ("p.." . "pdotdot")
    ("p:" . "pco")     ("q:" . "qco")         ("r." . "rdot")       ; 37
    ("s:" . "sco")     ("S:" . "scapco")      ("u:" . "uco")
    ("x:" . "xco")     ("Z:" . "zcapco")      ("_9:" . "zeroco")
    ("_8:" . "zeroco") ("_7:" . "zeroco")     ("_6:" . "zeroco")
    ("_5:" . "zeroco") ("_4:" . "zeroco")     ("_3:" . "zeroco")
    ("_2:" . "zeroco") ("_1:" . "zeroco")     ("0:" . "zeroco")
    ("1:" . "zeroco")  ("2:" . "zeroco")      ("3:" . "zeroco")
    ("4:" . "zeroco")  ("5:" . "zeroco")      ("6:" . "zeroco")
    ("7:" . "zeroco")  ("8:" . "zeroco")      ("9:" . "zeroco")
    ("assert." . "assertdot") ("break." . "breakdot") ("continue." . "continuedot") ; 41
    ("else." . "elsedot") ("elseif." . "elsedot")
    ("for." . "fordot") ("for_ijk." . "fordot")
    ;; skipped goto_lbl. and label_lbl. since lbl can vary
    ("if." . "ifdot") ("return." . "returndot")
    ("select." . "selectdot") ("case." . "selectdot") ("fcase." . "selectdot")
    ("throw." . "throwdot")
    ("try." . "trydot") ("catch." . "trydot") ("catchd." . "trydot") ("catcht." . "trydot")
    ("while." . "whiledot") ("whilst." . "whiledot"))
  "NuVoc. See https://code.jsoftware.com/wiki/NuVoc for more info.

(string * string) alist.")

(defconst j-help-dictionary-data-block
  (mapcar
   (lambda (l) (list (length (caar l))
                     (regexp-opt (mapcar 'car l))
                     l))
   (mapcar #'cdr
           (sort (seq-group-by (lambda (x) (length (car x)))
                               j-help-voc-alist)
                 (lambda (x y) (> (car x) (car y))))))
  "(int * string * (string * string) alist) list.")

(defun j-help-valid-dictionary ()
  "Return best defined dictionary."
  (replace-regexp-in-string
   "/$" ""
   (cond ((not (string= "" j-help-local-dictionary-url))
          j-help-local-dictionary-url)
         ((not (string= "" j-help-remote-dictionary-url))
          j-help-remote-dictionary-url))))

(defun j-help-symbol-pair-to-doc-url (alist-data)
  "Convert alist ALIST-DATA containing a symbol pair to the help url.

If alist-data is nil, then return the J Dictionary index page."
  (let ((dic (j-help-valid-dictionary)))
    (cond ((string= dic "") (error "%s" "No dictionary found. Please specify a dictionary."))
          ((not alist-data) dic)
          (t (let ((doc-name (cdr alist-data)))
               (format "%s/%s" dic doc-name))))))

(defun j-help-symbol-to-doc-url (j-symbol)
  "Convert J-SYMBOL into location url."
  (j-help-symbol-pair-to-doc-url (assoc j-symbol j-help-voc-alist)))

(defun j-help-determine-symbol (s point)
  "Internal function to determine j symbols. Should not be called directly.

string * int -> (string * string) list"
  (unless (or (< point 0) (< (length s) point))
    (cl-some
     (lambda (x)
       (let* ((check-size (car x)))
         (if (and
              (<= (+ check-size point) (length s))
              (string-match (cadr x) (substring s point (+ point check-size))))
           (let* ((m (match-data))
                  (ss (substring s (+ point (car m)) (+ point (cadr m)))))
             (assoc ss (caddr x))))))
     j-help-dictionary-data-block)))

(defun j-help-determine-symbol-at-point (point)
  "Determines the symbol at POINT.

int -> (string * string) list"
  (save-excursion
    (goto-char point)
    (let* ((bol (point-at-bol))
           (eol (point-at-eol))
           (s (buffer-substring-no-properties bol eol)))
      (j-help-determine-symbol s (- point bol)))))

(defun j-help-branch-determine-symbol-at-point*
  (string current-index target-index resolved-symbol)
  "Helper function for `j-help-branch-determine-symbol-at-point'."
  (if (> current-index target-index) resolved-symbol
    (let ((next-symbol (j-help-determine-symbol string current-index)))
      (j-help-branch-determine-symbol-at-point*
       string
       (+ current-index (length (or (car next-symbol) " ")))
       target-index
       next-symbol))))

(defun j-help-branch-determine-symbol-at-point (point)
  "Used by `j-help-lookup-symbol-at-point' to determine symbol at POINT."
  (save-excursion
    (goto-char point)
    (j-help-branch-determine-symbol-at-point*
     (buffer-substring-no-properties (point-at-bol) (point-at-eol))
     (- (max (- point j-help-symbol-search-branch-limit) (point-at-bol)) (point-at-bol))
     (- point (point-at-bol))
     nil)))

;;;###autoload
(defun j-help-lookup-symbol (symbol)
  "Look up SYMBOL in dictionary.

If symbol is nil, look up the dictionary index page."
  (interactive "sJ Symbol: ")
  (let ((url (j-help-symbol-to-doc-url symbol))
        (browse-url-browser-function (or j-help-browser-function
                                         browse-url-browser-function)))
    (message "Loading %s ..." url)
    (browse-url url)))

;;;###autoload
(defun j-help-lookup-symbol-at-point (point)
  "Determine the symbol nearest to POINT and look it up in the dictionary."
  (interactive "d")
  (let ((symbol (j-help-branch-determine-symbol-at-point point)))
    (if symbol
        (j-help-lookup-symbol (car symbol))
      (error "No symbol could be determined for point %d" point))))


(provide 'j-help)
;;; j-help.el ends here
