(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq indent-tabs-mode nil)
  (add-node-modules-path)
  ;; (prettier-js-mode)
  (js2-minor-mode))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(provide 'javascript)
;;; javascript.el ends here
