;;; init-http.el --- Emacs config HTTP client layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; HTTP request tooling

;;; Code:

;; HTTP REST client, see https://github.com/pashky/restclient.el
;; requests should be separated by comment lines starting with #
(use-package restclient
  :defer t
  ;; assume request source files have ".http" suffix
  :mode ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map
         ("C-c C-M-m" . my-hydra/restclient/body))
  :config
  (require 'json-mode)
  (defhydra my-hydra/restclient (:color teal :columns 3)
    "REST client"
    ("c" restclient-http-send-current "send")
    ("r" restclient-http-send-current-raw "send-raw")
    ("v" restclient-http-send-current-stay-in-window "send-bg")
    ("n" restclient-jump-next "next" :exit nil)
    ("p" restclient-jump-prev "prev" :exit nil)
    ("." restclient-mark-current "mark")
    ("u" restclient-copy-curl-command "copy-curl")
    ("N" (lambda () (interactive) (if (buffer-narrowed-p) (widen) (restclient-narrow-to-current))) "narrow" :exit nil)
    ("f" (lambda () (interactive) (if (fboundp 'json-mode-pretty-print-dwim) (call-interactively 'json-mode-pretty-print-dwim) (message "`json-mode-pretty-print-dwim' unavailable"))) "fmt-json-rgn")
    ("q" nil "quit"))
  ;; pulse *HTTP Response* buffer after receiving request response
  ;; adapted from https://github.com/jordonbiondo/.emacs.d/blob/master/init.el
  (defun my-restclient-pulse-buffer ()
    "Pulses the current buffer."
    (save-excursion
      (goto-char (point-min))
      (pulse-momentary-highlight-region (point-min) (point-max))))
  (add-hook 'restclient-response-loaded-hook #'my-restclient-pulse-buffer))

(provide 'init-http)

;;; init-http.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
