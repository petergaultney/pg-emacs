(load "flow-minor-mode.el")
(require 'flycheck-flow)

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  'flow-minor-mode
  )

(add-hook 'web-mode-hook  'web-mode-init-hook)
