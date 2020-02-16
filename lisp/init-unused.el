;;; init-unused.el --- Emacs config unused code -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Miscellaneous pieces of unused code

;;; Code:

(defmacro my-lazy-key-seq (mode-map key-seq &rest fun)
  "When the KEY-SEQ is pressed when MODE-MAP is active, run FUN, unbind KEY-SEQ and simulate KEY-SEQ again."
  (declare (indent 1))
  `(define-key ,mode-map ,key-seq (lambda ()
                                    (interactive)
                                    (progn (define-key ,mode-map ,key-seq nil)
                                           ,fun
                                           (setq unread-command-events (listify-key-sequence ,key-seq))))))

;; DEPRECATED - use `tab-bar-mode' or `tab-line-mode' in Emacs 27+
;; manage window configs
(use-package eyebrowse
  :delight eyebrowse-mode
  :commands eyebrowse-mode
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c C-M-w e") ;; change prefix binding from "C-c C-w" to "C-c C-M-w e"
        eyebrowse-new-workspace t)
  (my-lazy-key-seq global-map (kbd "C-c C-M-w e") (lambda () (require 'eyebrowse)))
  :config (eyebrowse-mode t))

(provide 'init-unused)

;;; init-unused.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
