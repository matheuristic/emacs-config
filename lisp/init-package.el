;;; init-package.el --- Emacs config package layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up package management built around use-package

;;; Code:

;; Whether to regenerate outdated bytecode
(defvar load-prefer-newer nil)

;; ELPA-compatible package.el repositories
(defvar package-archives '(("GNU"          . "https://elpa.gnu.org/packages/")
                           ("MELPA Stable" . "https://stable.melpa.org/packages/")
                           ("MELPA"        . "https://melpa.org/packages/")))

;; Priority levels for the ELPA-compatible repositories
(defvar package-archive-priorities '(("GNU"          . 10)
                                     ("MELPA Stable" . 5)
                                     ("MELPA"        . 0)))

;; initialize package.el
(setq package-enable-at-startup nil)
(require 'package)
(package-initialize)

;; bootstrap use-package, provides configuration macros
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key, and configure imenu support for `require'
;; and `use-package'
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'bind-key)
  (setq use-package-always-ensure t))

(provide 'init-package)

;;; init-package.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
