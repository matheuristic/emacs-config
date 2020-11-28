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
         ("I" . my-eww-toggle-images)
         ("C-c C-M-m" . my-hydra/eww/body))
  :init (setq eww-search-prefix "https://duckduckgo.com/lite?q=")
  :config
  (setq-default shr-inhibit-images t) ;; don't render images in HTML pages by default
  (defun my-eww-toggle-images ()
    "Toggle displaying of images when rendering HTML."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload)
    (message "Images are now %s" (if shr-inhibit-images "off" "on")))
  (defhydra my-hydra/eww (:color teal :columns 3)
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
    ("F" eww-toggle-fonts "toggle-fonts")
    ("I" my-eww-toggle-images "toggle-images")
    ("M-C" eww-toggle-colors "toggle-colors")
    ("D" eww-toggle-paragraph-direction "toggle-text-dir")
    ("s" eww-switch-to-buffer "eww-switch-buf")
    ("S" eww-list-buffers "eww-list-buf")
    ("H" eww-list-histories "history")
    ("C" url-cookie-list "cookie-list")
    ("q" nil "quit")))

;; define search engines and query shortcuts "<prefix> <keybinding>"
;; the default prefix is "C-x /" and can be
;; changed using `engine/set-keymap-prefix'
(use-package engine-mode
  :config
  (defengine amazon
    "https://www.amazon.com/s?ie=UTF8&field-keywords=%s"
    :docstring "Search Amazon (amazon.com)"
    :keybinding "a")
  (defengine clojuredocs
    "https://clojuredocs.org/search?q=%s"
    :docstring "Search ClojureDocs (clojuredocs.org)."
    :keybinding "c")
  (defengine duckduckgo
    "https://duckduckgo.com/lite?q=%s"
    :docstring "Search DuckDuckGo (duckduckgo.com)."
    :keybinding "d"
    :browser 'eww-browse-url)
  (defengine google
    "https://www.google.com/search?q=%s"
    :docstring "Search Google (google.com)."
    :keybinding "g")
  (defengine google-dataset
    "https://datasetsearch.research.google.com/search?query=%s"
    :docstring "Google Dataset Search (datasetsearch.research.google.com)."
    :keybinding "D")
  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Search Google Maps (maps.google.com)."
    :keybinding "m")
  (defengine google-trends
    "https://trends.google.com/trends/explore?q=%s"
    :docstring "Search Google Trends (trends.google.com)"
    :keybinding "T")
  (defengine lobsters
    "https://lobste.rs/search?order=newest&q=%s"
    :docstring "Search Lobste.rs (lobste.rs)."
    :keybinding "l"
    :browser 'eww-browse-url)
  (defengine melpa
    "https://melpa.org/#/?q=%s"
    :docstring "Search MELPA (melpa.org)."
    :keybinding "m")
  (defengine python-docs
    "https://docs.python.org/3/search.html?q=%s"
    :docstring "Search Python documentation (docs.python.org)."
    :keybinding "p")
  (defengine reddit
    "https://www.reddit.com/search.compact?q=%s"
    :docstring "Search Reddit (reddit.com)."
    :keybinding "r"
    :browser 'eww-browse-url)
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :docstring "Search StackOverflow (stackoverflow.com)."
    :keybinding "s")
  (defengine twitter
    "https://mobile.twitter.com/search?q=%s"
    :docstring "Search Twitter (twitter.com)."
    :keybinding "t")
  (defengine wikipedia
    "https://en.m.wikipedia.org/w/index.php?search=%s"
    :docstring "Search Wikipedia (wikipedia.org)."
    :keybinding "w"
    :browser 'eww-browse-url)
  (engine-mode 1))

(provide 'init-web)

;;; init-web.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
