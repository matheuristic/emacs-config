;;; init-ui-icons.el --- Emacs config icons layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Prettify using font icons

;; The fonts from https://github.com/domtronn/all-the-icons.el have to be
;; installed on the system, either by downloading and manually installing them
;; (e.g. using the Font Book app on OS X) or by running
;; "M-x all-the-icons-install-fonts"

;;; Code:

(when (display-graphic-p)
  ;; font icons
  (use-package all-the-icons
    :config (setq all-the-icons-color-icons nil))

  ;; use font icons in Dired
  (use-package all-the-icons-dired
    :after (all-the-icons dired)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config (set-face-attribute 'all-the-icons-dired-dir-face nil
                                :weight 'normal))

  ;; use font icons in Gnus
  (use-package all-the-icons-gnus
    :after (all-the-icons gnus)
    :config (all-the-icons-gnus-setup))

  ;; use font icons in Ibuffer
  (use-package all-the-icons-ibuffer
    :after (all-the-icons ibuffer)
    :config (all-the-icons-ibuffer-mode 1)))

(provide 'init-ui-icons)

;;; init-ui-icons.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
