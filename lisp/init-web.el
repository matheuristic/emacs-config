;;; init-web.el --- Emacs config web layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Web browsing

;;; Code:

;; Emacs text web browser
(use-package eww
  :ensure nil ;; built-in
  :commands (eww eww-follow-link)
  :bind (:map eww-mode-map
         ("C-c C-M-m" . my-hydra/eww/body))
  :init (setq browse-url-browser-function 'eww-browse-url
              eww-search-prefix "https://duckduckgo.com/?q=")
  :config (defhydra my-hydra/eww (:color teal :columns 3)
            "Emacs Web Wowser"
            ("d" eww-download "download-link")
            ("G" eww "search")
            ("o" eww-open-file "open-file")
            ("l" eww-back-url "back")
            ("r" eww-forward-url "forward")
            ("g" eww-reload "reload")
            ("v" eww-view-source "view-source")
            ("w" eww-copy-url "copy-url")
            ("&" eww-browse-with-external-browser "browse-ext")
            ("b" eww-add-bookmark "bookmark-page")
            ("B" eww-list-bookmarks "bookmark-list")
            ("R" eww-readable "readable-only")
            ("F" eww-toggle-fonts "toggle-var-fonts")
            ("M-C" eww-toggle-colors "toggle-colors")
            ("D" eww-toggle-paragraph-direction "toggle-text-dir")
            ("s" eww-switch-to-buffer "eww-switch-buf")
            ("S" eww-list-buffers "eww-list-buf")
            ("H" eww-list-histories "history")
            ("C" url-cookie-list "cookie-list")
            ("q" nil "quit")))

;; define search engines and query shortcuts for them
;; which are "<prefix> <keybinding>", default prefix is "C-x /"
(use-package engine-mode
  :config
  (defengine amazon
    "https://www.amazon.com/s?ie=UTF8&field-keywords=%s"
    :docstring "Search Amazon (amazon.com)"
    :keybinding "a"
    :browser 'browse-url-default-browser) ;; open in external browser
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :docstring "Search DuckDuckGo (duckduckgo.com)"
    :keybinding "d")
  (defengine google
    "https://www.google.com/search?q=%s"
    :docstring "Search Google (google.com)"
    :keybinding "g"
    :browser 'browse-url-default-browser)
  (defengine google-dataset
    "https://datasetsearch.research.google.com/search?query=%s"
    :docstring "Google Dataset Search (datasetsearch.research.google.com)"
    :keybinding "D"
    :browser 'browse-url-default-browser)
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Search Google Maps (maps.google.com)"
    :keybinding "m"
    :browser 'browse-url-default-browser)
  (defengine google-trends
    "https://trends.google.com/trends/explore?q=%s"
    :docstring "Search Google Trends (trends.google.com)"
    :keybinding "T"
    :browser 'browse-url-default-browser)
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :docstring "Search StackOverflow (stackoverflow.com)"
    :keybinding "s")
  (defengine twitter
    "https://mobile.twitter.com/search?q=%s"
    :docstring "Search Twitter (twitter.com)"
    :keybinding "t")
  (defengine wikipedia
    "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :docstring "Search Wikipedia (wikipedia.org)"
    :keybinding "w")
  (engine-mode 1))

(provide 'init-web)

;;; init-web.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
