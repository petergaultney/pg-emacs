;; requires node v18+ to be installed!
(message "Loading github copilot... sometimes this breaks randomly")
(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :config
  (define-key copilot-completion-map (kbd "M-v") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "<escape>") 'copilot-clear-overlay)
  (add-hook 'prog-mode-hook 'copilot-mode)

  ;; all of this comes from https://github.com/copilot-emacs/copilot.el/issues/312
  ;; and i don't understand it and i doubt I ever will.
  ;; but it stops the warning
  ;; copilot--infer-indentation-offset found no mode-specific indentation offset
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )
(global-unset-key (kbd "M-v"))

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
