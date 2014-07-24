;; Keyboard bindings

;; move by paragraph
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(defun scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "C-<down>") 'scroll-down-one-line)
(global-set-key (kbd "C-<up>") 'scroll-up-one-line)

;; make cmd meta key
;(setq x-super-keysym 'meta)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; compilation
(global-set-key "\C-xc" 'compile)

;; (re-)load init.elg
(global-set-key [XF86Tools] (lambda ()
							  (interactive)
							  (load-file "~/.emacs.d/init.el")))

;; org-mode global keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; use mouse scroll to zoom in/out
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; shortcut goto-line [f4]

;; shortcut magit-status [f7]

;; shortcut run shell

;; local keybinding python mode -> change venv

;; eval region/buffer of elisp

;; make dedicated windows / freeze buffer
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-set-key [kp-enter] 'toggle-window-dedicated)
