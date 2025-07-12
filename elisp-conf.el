(use-package
  elisp-autofmt
  :ensure t
  :config
  (setq elisp-autofmt-style 'fixed)
  (setq elisp-autofmt-on-save-p t))

;; surely comments get kept, right?

(add-hook
  'emacs-lisp-mode-hook
  (lambda () (add-hook 'before-save-hook 'elisp-autofmt-buffer nil t)))

(define-prefix-command 'elisp-prefix-map)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'elisp-prefix-map)

(define-key elisp-prefix-map (kbd "r") 'eval-region)
(define-key elisp-prefix-map (kbd "b") 'eval-buffer)
(define-key elisp-prefix-map (kbd "e") 'eval-last-sexp)
(define-key elisp-prefix-map (kbd "f") 'elisp-autofmt-buffer)
