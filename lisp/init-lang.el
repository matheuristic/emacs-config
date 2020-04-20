;;; init-lang.el --- Emacs config language layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Programming languages tooling

;;; Code:

(require 'init-ui-hydra)

(defgroup init-lang-el nil
  "Language-specific settings."
  :group 'convenience)

(defcustom init-lang-enable-list '("bibtex" "clojure" "csv" "docker" "json"
                                   "lisp" "markdown" "plantuml" "python" "r"
                                   "scheme" "yaml")
  "List of languages for which to enable support."
  :type '(repeat string)
  :group 'init-lang-el)

;; support for code linting
(require 'init-linter)

;; tooling for Language Server Protocol and Debug Adaptor Protocol
(require 'init-lang-lsp)

;; CSV
(when (member "csv" init-lang-enable-list)
  (use-package csv-mode
    :commands csv-mode
    :bind (:map csv-mode-map
           ("C-c C-M-m" . my-hydra/csv-mode/body))
    :config
    (setq csv-align-style 'auto) ;; `csv-align-fields' left/right-aligns text/numbers
    (defun csv-align-visible-fields ()
      "Align visible lines in `csv-mode'. Useful for large CSV files where
`csv-align-fields' can take a very long time to run."
      (interactive)
      (csv-align-fields nil (window-start) (window-end)))
    (defhydra my-hydra/csv-mode (:color teal :columns 4)
      "CSV"
      ("s" csv-sort-fields "sort")
      ("n" csv-sort-numeric-fields "numsort")
      ("r" csv-reverse-region "reverse")
      ("d" csv-toggle-descending "toggle-desc-sort" :exit nil)
      ("t" csv-transpose "transpose")
      ("k" csv-kill-fields "cut")
      ("y" csv-yank-fields "paste")
      ("z" csv-yank-as-new-table "paste-as-new-tab")
      ("A" csv-align-visible-fields "align-visible" :exit nil)
      ("a" csv-align-fields "align" :exit nil)
      ("u" csv-unalign-fields "unalign" :exit nil)
      ("h" csv-header-line "toggle-header" :exit nil)
      ("v" csv-toggle-invisibility "toggle-invis-sep" :exit nil)
      ("q" nil "quit"))))

;; Dockerfile
(when (member "docker" init-lang-enable-list)
  (use-package dockerfile-mode
    :commands dockerfile-mode
    :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))

;; BibTeX
;;
;; this setup supports exporting Org to PDF with BibTeX bibliographies via
;; xelatex and biber, so they will need to be installed on the system
;;
;; Org documents should include the LaTeX headers for bibliographies via
;; "#+LATEX_HEADER:" structural markup elements, and "\printbibliography"
;; should be added at the desired location for the bibliography (usually
;; at the end of an article or book chapter or before the index)
;;
;; Org references to bibliography entries can be inserted by pressing `i' when
;; on an entry in ebib or by calling `ebib-insert-citation' within Org mode
;;
;; to export references from Org to LaTeX, ebib needs to be opened with the
;; bibliographies for the references that appear in the document
;;
;; use "::" in the Org link description to separate the preamble text,
;; pre-note, and post-note elements (all optional) for export to LaTeX,
;; i.e. "[[ebib:key][Preamble text::Pre-note::Post-note]]"
;; will export to "Preamble text\cite[Pre-note][Post-note]{key}"
;;
;; example:
;; ---
;; ...
;; #+LATEX_HEADER: \usepackage[backend=biber]{biblatex}
;; #+LATEX_HEADER: \addbibresource{path/to/bibtex_file.bib}
;; ...
;; [[ebib:some_ebib_entry_key]]
;; [[ebib:some_ebib_entry_key][Preamble]
;; [[ebib:some_ebib_entry_key][Preamble::::Post-note]
;; [[ebib:some_ebib_entry_key][Preamble::Pre-note::Post-note]]
;; [[ebib:incognito_1970][::see::pg. 99]]
;; ...
;; \printbibliography
;; ...
;; ---
(when (member "bibtex" init-lang-enable-list)
  ;; BibTeX reference manager
  (use-package ebib
    :commands ebib
    :bind ("C-c C-M-b e" . ebib)
    :config
    (with-eval-after-load 'org
      (require 'org-ebib)
      ;; compile LaTeX to PDF using xelatex and biber (for bibliographies)
      (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                                    "biber %b"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"
                                    "xelatex -interaction nonstopmode -output-directory %o %f"))
      (defun my-org-ebib-export (path desc format)
        "Export an ebib link. See `org-link-parameters' for details about PATH, DESC and FORMAT."
        (let* ((my-desc (or desc ""))
               (desc-parts (split-string my-desc "::"))
               (desc-name (car desc-parts))
               (desc-pre-note (or (nth 1 desc-parts) ""))
               (desc-post-note (mapconcat 'identity (nthcdr 2 desc-parts) "::")))
          (cond
            ((eq format 'html)
             (if desc
                 (format "(%s<cite>%s</cite>%s)"
                         (if (string= "" desc-pre-note) "" (concat desc-pre-note " "))
                         (if (string= "" desc-name) path desc-name)
                         (if (string= "" desc-post-note) "" (concat ", " desc-post-note)))
               (format "(<cite>%s</cite>)" path)))
            ((eq format 'latex)
             (if desc
                 (format "%s\\cite%s%s{%s}"
                         (concat desc-name " ")
                         (if (string= "" desc-pre-note) "" (format "[%s]" desc-pre-note))
                         (if (string= "" desc-post-note) "" (format "[%s]" desc-post-note))
                         path)
               (format "\\cite{%s}" path))))))
      (org-link-set-parameters "ebib" :export 'my-org-ebib-export)
      (bind-key "C-c C-M-b i" 'ebib-insert-citation org-mode-map))))

;; Clojure
(when (member "clojure" init-lang-enable-list)
  (require 'init-lang-clojure))

;; Emacs Speaks Statistics, has Flymake support for R lintr if installed
(when (member "r" init-lang-enable-list)
  (use-package ess
    :mode ("\\.R$" . R-mode)
    :commands (R-mode ess-switch-to-ESS)
    :init (setq ess-eval-visibly 'nowait
                ess-default-style 'RStudio)
    :config
    ;; open inferior processes in new frames, not split windows, in GUI Emacs
    (when (display-graphic-p)
      (add-to-list 'display-buffer-alist
                   '("*R"
                     (display-buffer-reuse-window display-buffer-pop-up-frame)
                     (reusable-frames . 0))))
    (defhydra my-hydra/ess (:color teal :hint nil)
      "
Emacs Speaks Statistics

Session     _N_   : new     _R_   : request _s_   : switch  _C-q_ : quit

Eval        _l_   : line    _f_   : func    _r_   : region  _b_   : buffer

Workspace   _D_   : chdir   _d_   : R dired

Help        _h_   : object  _H_   : browser _A_   : apropos

"
      ;; session
      ("N" (lambda () (interactive)
             (cond ((string= ess-dialect "R") (R))
                   ((string= ess-dialect "julia") (julia))
                   (t (message "Unsupported dialect")))))
      ("R" ess-request-a-process)
      ("s" ess-switch-to-ESS)
      ("C-q" ess-quit)
      ;; eval
      ("l" ess-eval-line)
      ("f" ess-eval-function)
      ("r" ess-eval-region)
      ("b" ess-eval-buffer)
      ;; workspace
      ("D" ess-change-directory)
      ("d" ess-rdired)
      ;; help
      ("h" ess-display-help-on-object)
      ("H" ess-display-help-in-browser)
      ("A" ess-display-help-apropos)
      ("q" nil "quit"))
    ;; adapted from https://emacs.stackexchange.com/questions/8041/how-to-implement-the-piping-operator-in-ess-mode
    (defun my-insert-R-pipe-operator ()
      "Insert R magrittr pipe operator '%>%'."
      (interactive)
      (just-one-space 1)
      (insert "%>%")
      (reindent-then-newline-and-indent))
    (defun my-insert-R-assignment-operator ()
      "Insert R assigment operator '<-'."
      (interactive)
      (just-one-space 1)
      (insert "<- "))
    ;; keybindings for hydra
    (with-eval-after-load 'ess-mode
      (define-key ess-mode-map (kbd "M--") #'my-insert-R-assignment-operator)
      (define-key ess-mode-map (kbd "C-S-m") #'my-insert-R-pipe-operator)
      (define-key ess-mode-map (kbd "C-c C-M-m") #'my-hydra/ess/body))
    ;; keybindings for inserting assignment and pipe operators
    (with-eval-after-load 'ess-inf
      (define-key inferior-ess-mode-map (kbd "M--") #'my-insert-R-assignment-operator)
      (define-key inferior-ess-mode-map (kbd "C-S-m") #'my-insert-R-pipe-operator))))

;; JSON
(when (member "json" init-lang-enable-list)
  (use-package json-mode
    :defer t))

;; LISP
(when (member "lisp" init-lang-enable-list)
  (require 'init-lang-lisp))

;; Markdown
(when (member "markdown" init-lang-enable-list)
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)
           ("\\.Rmd\\'" . markdown-mode)) ; R markdown
    :bind (:map markdown-mode-map
           ("C-c C-M-m" . my-hydra/markdown-mode/body)
           :map gfm-mode-map
           ("C-c C-M-m" . my-hydra/markdown-mode/body))
    :config
    ;; table of contents
    (use-package markdown-toc)
    ;; render mathematical expressions in HTML previews
    (setq markdown-xhtml-header-content
          (concat "<script type=\"text/x-mathjax-config\">"
                  "MathJax.Hub.Config({"
                  "  tex2jax: {"
                  "    inlineMath: [ ['$','$'], [\"\\\\(\",\"\\\\)\"] ],"
                  "    processEscapes: true"
                  "  }"
                  "});"
                  "</script>"
                  "<script type=\"text/javascript\" async"
                  "        src=\"https://cdnjs.cloudflare.com/ajax/libs/"
                  "mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML\">"
                  "</script>"))
    (setq markdown-asymmetric-header t ;; place header markup only at the start of a line
          markdown-fontify-code-blocks-natively t) ;; syntax highlighting in fenced code blocks
    (defhydra my-hydra/markdown-mode (:color teal :hint nil)
      "
Markdown mode

Keymaps     _c_ : commands  _s_ : styles

Outline     _n_ : next      _p_ : prev      _f_ : fwd-level _b_ : bwd-level
            _←_ : promote   _→_ : demote    _↓_ : move-down _↑_ : move-up

Shift-Rgn   _<_ : left      _>_ : right

Toggle      _E_ : math      _F_ : code-font _I_ : images    _L_ : url
            _M_ : markup

Other       _d_ : do        _o_ : follow    _'_ : edit code block
            _t_/_C-t_ : insert-or-refresh/remove table of contents

"
      ;; keymaps
      ("c" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-c\C-c"))))
      ("s" (lambda () (interactive) (setq unread-command-events (listify-key-sequence "\C-c\C-s"))))
      ;; outline
      ("n" markdown-outline-next :color red)
      ("p" markdown-outline-previous :color red)
      ("f" markdown-outline-next-same-level :color red)
      ("b" markdown-outline-previous-same-level :color red)
      ("<left>" markdown-promote :color red)
      ("<right>" markdown-demote :color red)
      ("<down>" markdown-move-down :color red)
      ("<up>" markdown-move-up :color red)
      ;; shift region
      ("<" markdown-outdent-region :color red)
      (">" markdown-indent-region :color red)
      ;; user interface
      ("E" markdown-toggle-math)
      ("F" markdown-toggle-fontify-code-blocks-natively)
      ("I" markdown-toggle-inline-images)
      ("L" markdown-toggle-url-hiding)
      ("M" markdown-toggle-markup-hiding)
      ;; other
      ("d" markdown-do)
      ("o" markdown-follow-thing-at-point)
      ("'" markdown-edit-code-block)
      ("t" markdown-toc-generate-or-refresh-toc)
      ("C-t" markdown-toc-delete-toc)
      ;; quit
      ("q" nil "quit"))))

;; PlantUML support in Org documents
;;
;; graphviz and java needs to be installed on the system
;;
;; the plantuml.jar file needs to be downloaded to `plantuml-jar-path' (the
;; default value is "~/plantuml.jar") either manually or using
;; "M-x plantuml-download-jar"
;;
;; there is also Org source block support (edit a block with "C-c '" and
;; evaluate with "C-c C-c"), e.g.
;; ---
;; ...
;; #+BEGIN_SRC plantuml :file diagram.png
;;   @startuml
;;   [*] --> State1
;;   State1 --> [*]
;;   State1 : a line description
;;   State1 : another line description
;;   State1 --> State2
;;   State2 --> [*]
;;   @enduml
;; #+END_SRC
;; ...
;; ---
(when (member "plantuml" init-lang-enable-list)
  (use-package plantuml-mode
    :defer t
    :mode ("\\.p\\(lant\\)?uml" . plantuml-mode)
    :init (setq plantuml-default-exec-mode 'jar)
    :config (with-eval-after-load 'org
              (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
              (setq org-plantuml-jar-path plantuml-jar-path)
              (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))))

;; Python
(when (member "python" init-lang-enable-list)
  (require 'init-lang-python))

;; Scheme
(when (member "scheme" init-lang-enable-list)
  (require 'init-lang-scheme))

;; YAML
(when (member "yaml" init-lang-enable-list)
  (use-package yaml-mode
    :commands yaml-mode
    :mode ("\\.ya?ml\\'" . yaml-mode)))

(provide 'init-lang)

;;; init-lang.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
