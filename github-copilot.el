;; requires node v18+ to be installed!
(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :config
  (define-key copilot-completion-map (kbd "M-v") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-next-completion)
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "Esc") 'copilot-clear-overlay)
  (add-hook 'prog-mode-hook 'copilot-mode)
  )
(global-unset-key (kbd "M-v"))

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
