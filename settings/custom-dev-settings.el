;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode settings
(require 'org)
(require 'ox-latex)
(setq org-src-fontify-natively t)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-export-latex-listings 'minted)
;; For keeping buffers up-to-date with tangled files
;; (global-auto-revert-mode t)
(defun revert-all-buffers ()
          "Refreshes all open buffers from their respective files"
          (interactive)
          (let* ((list (buffer-list))
                 (buffer (car list)))
            (while buffer
              (when (and (buffer-file-name buffer)
                         (not (buffer-modified-p buffer)))
                (set-buffer buffer)
                (revert-buffer t t t))
              (setq list (cdr list))
              (setq buffer (car list))))
          (message "Refreshed open files"))
;; For tangling code automatically when saving org-files
(defun tangle-on-save ()
	"Extract source code from org-files upon saving."
  (message "Tangling sources...")
  (org-babel-tangle)
  (revert-all-buffers))
(add-hook 'org-mode-hook
          (lambda ()
			(add-hook 'after-save-hook
					  'tangle-on-save 'make-it-local)))

;; Expand region
(require 'expand-region)

;; Magit
(require 'magit)

;; Yasnippet
;; Configure yasnippet before changing ac-settings
(require 'yasnippet) ; in global-mode loads snippets only on demand
(yas-global-mode 1) ; (add-hook 'prog-mode-hook 'yas-minor-mode)
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;; Autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default) ; integrates with yasnippet
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)
;;ac-dwim nil)
;;ac-expand-on-auto-complete nil)
(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
;;(setq completion-cycle-threshold 5)

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; Flycheck
(require 'flycheck) ; (install flake8, clang etc.)
;(global-flycheck-mode t)

;; Indentation
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#6F6F6F")
(set-face-background 'highlight-indentation-current-column-face "#6F6F6F")

;; Fill-column indicator
(require 'fill-column-indicator)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq fci-rule-color "white")
	    (fci-mode 1)))

;; iEdit mode
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; Skip trailing whitespace on save (leave one)
(add-hook 'prog-mode-hook
		  (lambda ()
			(custom-set-variables
			 '(require-final-newline t))
			(add-to-list 'write-file-functions
						 'delete-trailing-whitespace)))

;; Compile command
(require 'compile)
(add-hook 'prog-mode-hook
		  (lambda ()
			(if (boundp 'default-directory)
				(make-local-variable 'compile-command)
			  (cond
			   ((file-exists-p
				 (concat (default-directory) "SConstruct"))
				(set 'compile-command "scons"))
			   ((file-exists-p
				 (concat (default-directory) "Makefile"))
				(set 'compile-command "make -k"))
			   (t
				(set 'compile-command
					 ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
					 (let ((file
							(file-name-nondirectory buffer-file-name)))
					   (format "%s -c -o %s.o %s %s %s"
							   (or (getenv "CC") "g++")
							   (file-name-sans-extension file)
							   (or (getenv "CPPFLAGS") "-DDEBUG=9")
							   (or (getenv "CFLAGS")
								   "-pedantic -Wall -g")
							   file))))))))

;; Allow customizing project directory
(setq enable-local-eval t)
(put 'default-directory 'safe-local-variable #'stringp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "custom-dev-c++-settings")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "custom-dev-python-settings")
