;; (use-package lsp-mode
;;   :hook ((python-mode) . lsp-deferred)
;;   :commands lsp
;;   :config
;;   (define-key lsp-mode-map (kbd "M-p") lsp-command-map)
;;   (setq lsp-keymap-prefix "M-p"))

;; ;; (use-package lsp-pyright
;; ;;   :hook (python-mode . (lambda () (require 'lsp-pyright)))
;; ;;   :init (when (executable-find "python3")
;; ;;           (setq lsp-pyright-python-executable-cmd "python3")))
;; (use-package lsp-ui
;;   :commands lsp-ui-mode)
