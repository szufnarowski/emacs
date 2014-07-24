;; Member functions
(require 'member-functions)
;; Make to body of mf--infer-c-filename (buffer-name (ido-switch-buffer))
;; Comment out (find-file-noselect [header|c-file]) in expand-member-functions
(setq mf--source-file-extension "cpp")
(add-hook 'c-mode-common-hook
		  (lambda ()
			(local-set-key "\C-cm" #'expand-member-functions)))

;; Commmon CC settings
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(setq
 c-default-style "linux"
 c-basic-offset 4)

;; Edit h-files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Delete as much whitespace as possible
(add-hook 'c-mode-common-hook (lambda ()
								(c-toggle-hungry-state 1)))


;; Autocomplete header names
(defun local-ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
;  (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
  (add-to-list 'achead:include-directories
			   '"/usr/include/c++/4.8"))
(add-hook 'c-mode-common-hook 'local-ac-c-header-init)

(defvar custom-includes '("inc"))
(defun get-includes (inclist)
  (let ((incpath ()))
	(hack-local-variables)
	(mapcar (lambda (item)
			  (if (file-name-absolute-p item)
				  item
				(if (boundp 'default-directory)
					(expand-file-name (concat default-directory item))
				  (expand-file-name item)))) inclist)))


;; For Intellisense use irony-mode
(require 'irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'c-mode-common-hook 'irony-mode)

(defun local-irony-hook ()
  (require 'company)
  (company-mode)
  (define-key irony-mode-map (kbd "<C-tab>")
	'company-irony) ;'irony-completion-at-point-async)
  (mapc (lambda (item)
		  (setq irony--clang-options
				(append '("-I") (list item) irony--clang-options)))
		(get-includes custom-includes)))

(add-hook 'irony-mode-hook 'local-irony-hook)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
			(setq flycheck-clang-include-path
				  (get-includes custom-includes))))


;; Google C Style
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)
