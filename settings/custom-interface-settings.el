;; Simplistic interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show time
(display-time-mode 1)

;; Speedbar no frame (sr-speedbar)
;;(require 'sr-speedbar)

;; add newline automatically
(setq next-line-add-newlines t)

;; Ido mode
(require 'ido)
(ido-mode t)

;; Ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window) ;'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Winner
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; dire?

;; Redo?

;; Transient mark mode (disable to enhance kill ring capabilities)
(transient-mark-mode t)

;; Require final newlines in files when they are saved
(setq fill-column 70)

;; Specify the fringe width for windows
;(require 'fringe)
;(fringe-mode 10)
;(setq overflow-newline-into-fringe t)
;(setq truncate-lines t)
;(setq truncate-partial-width-windows t)

;; Highlight current line
;(require 'highlight-current-line)
;(global-hl-line-mode t)
;(setq highlight-current-line-globally t)
;(setq highlight-current-line-high-faces nil)
;(setq highlight-current-line-whole-line nil)
;(setq hl-line-face (quote highlight))

;; Don't blink the cursor
;(blink-cursor-mode nil)

;; Themes
(add-to-list 'custom-theme-load-path custom-directory-themes)
(load-theme 'zenburn t)

;; No start-up messages nor splashes
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Full-screen, also (toggle-frame-fullscreen) -> F11
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; Short answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; Numbering lines/columns
(require 'linum-relative)
(global-linum-mode 1)
(column-number-mode 1)

;; Truncate long lines visually
(global-visual-line-mode)

;; Use Super to move around windows (+ frames)
(require 'framemove)
(require 'buffer-move)
;(windmove-default-keybindings 'shift)
(windmove-default-keybindings 'super)
(setq framemove-hook-into-windmove t)
(show-paren-mode t)

;; Turn beep off
(setq visible-bell nil)

;; Use 4 spaces for a tab, force spaces for indentation
(setq-default default-tab-width 4)
(setq indent-tabs-mode nil)

;; Ignore case when searching
(setq case-fold-search t)

;; Highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)

;; Use mouse wheel even in plain terminal
(require 'mwheel)
(mouse-wheel-mode t)
