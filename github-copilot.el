;; requires node to be installed!
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "M-e") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "<backtab>") 'copilot-previous-completion)
(define-key copilot-completion-map (kbd "Esc") 'copilot-clear-overlay)
