(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)

;; Python-environment
(require 'python-environment)
(setq python-environment-directory "~/.virtualenvs")
(setq python-environment-default-root-name "default") ; symlink

;; Jedi & Virtualenvwrapper settings
(require 'jedi)
(require 'pos-tip)
(setq jedi:tooltip-method '(pos-tip))
(setq jedi:complete-on-dot t)
(setq jedi:server-command '("jediepcserver")) ; on PATH in every venv
;(setq jedi:server-args
;	  '("--sys-path" ""))

(require 'virtualenvwrapper)
(setq venv-location (file-name-as-directory python-environment-directory))
(put 'project-venv-name 'safe-local-variable #'stringp)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

(defun local-jedi-server-setup ()
  (hack-local-variables)
  (when (boundp 'project-venv-name)
    (venv-workon project-venv-name))
  (jedi:setup))
(add-hook 'python-mode-hook 'local-jedi-server-setup)


;; Outline mode / code folding (hs-minor-mode)

;; Tree view
;; (eval-after-load "python"
;;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)

;; Ipython shell integration (;--pylab inline)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Modeline with venv indication?
;; PythonTidy?
