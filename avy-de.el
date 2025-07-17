(use-package
  avy
  :ensure t
  :config (setq avy-keys '(?a ?s ?e ?t ?n ?i ?o ?h ?d))
  ;; (global-set-key (kbd "C-t") 'avy-goto-char-timer)
  ;; (global-set-key (kbd "M-l") 'avy-goto-line)
  ;; (global-set-key (kbd "M-t") 'avy-goto-char-2)
  (define-key isearch-mode-map (kbd "C-t") 'avy-isearch)

  (defvar my-avy-commands-map (make-sparse-keymap)
    "A keymap for avy text jumping commands.")

  (global-set-key (kbd "C-t") my-avy-commands-map)

  (define-key my-avy-commands-map (kbd "t") 'avy-goto-char-timer)
  (define-key my-avy-commands-map (kbd "C-t") 'avy-goto-char-timer)
  (define-key my-avy-commands-map (kbd "l") 'avy-goto-line)
  (define-key my-avy-commands-map (kbd "C-l") 'avy-goto-line)
  (define-key my-avy-commands-map (kbd "c") 'avy-goto-char-2))
