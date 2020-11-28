;;; init-backup.el --- Emacs config backup layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Configure automatic file backups

;;; Code:

(defvar init-backup-dir "~/.backup/"
  "Path to backups directory, set to nil for no backup directory.")

;; configure backup directory
(when init-backup-dir
  (when (not (file-directory-p init-backup-dir))
    (make-directory init-backup-dir t))
  (setq backup-directory-alist `(("." . ,init-backup-dir))
        version-control t ;; use version numbers for backups
        kept-new-versions 3 ;; number of newest versions to keep
        kept-old-versions 0 ;; number of oldest versions to keep
        delete-old-versions t ;; don't ask before deleting old backups
        backup-by-copying t)) ;; backup by copying instead of renaming

(provide 'init-backup)

;;; init-backup.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
