;; requires node to be installed!
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))

(add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "RET") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "<backtab>") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "Esc") 'copilot-clear-overlay)
