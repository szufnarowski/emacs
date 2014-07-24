;; Custom directory structure
(defvar custom-directory-root
  (file-name-as-directory  "~/.emacs.d"))

(defvar custom-directory-themes
  (concat custom-directory-root "themes"))

(defvar custom-directory-packages
  (concat custom-directory-root "site-packages"))

(defvar custom-directory-settings
  (concat custom-directory-root "settings"))

(add-to-list 'load-path custom-directory-themes)
(add-to-list 'load-path custom-directory-packages)
(add-to-list 'load-path custom-directory-settings)

;; External packages (call this first)
(load "custom-external-packages")

;; Look and feel
(load "custom-interface-settings")

;; Development (python, git etc.)
(load "custom-dev-settings")

;; Misc modes (org, dired etc.)
(load "custom-misc-settings")

;; Keyboard (call it last)
(load "custom-keyboard-settings")
