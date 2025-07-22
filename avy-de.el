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

;; (use-package
;;   frog-jump-buffer
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-b") 'frog-jump-buffer)
;;   (setq frog-jump-buffer-include-current-buffer nil)
;;   (setq frog-jump-buffer-max-buffers 20)
;;   (setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-file-buffers)
;;   (setq frog-jump-buffer-default-filters-capital-letters nil)
;;   (setq frog-menu-avy-padding t)
;;   :custom
;;   (frog-menu-avy-keys
;;     (append
;;       (string-to-list "asetnioh")
;;       (string-to-list "qwdfurl;")
;;       (string-to-list "zxcvm,./")
;;       (string-to-list (upcase "asethoin"))
;;       (string-to-list (upcase "qwdfurl;"))
;;       (string-to-list (upcase "zxcvm,./")))))

(provide 'avy-de)
