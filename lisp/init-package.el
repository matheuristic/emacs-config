;;; init-package.el --- Emacs config package layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Package management config and tooling

;;; Code:

;; Whether to regenerate outdated bytecode
(defvar load-prefer-newer nil)

;; ELPA-compatible package.el repositories
(defvar package-archives '(("GNU"   . "https://elpa.gnu.org/packages/")
                           ("MELPA" . "https://melpa.org/packages/")))

;; Priority levels for the ELPA-compatible repositories
(defvar package-archive-priorities '(("GNU"   . 2)
                                     ("MELPA" . 6)))

;; initialize package.el
(setq package-enable-at-startup nil)
(require 'package)
(package-initialize)

;; bootstrap use-package, provides configuration macros
;; for info, see https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; preload use-package and bind-key packages
;; configure imenu support for the `require' and `use-package' keywords
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
