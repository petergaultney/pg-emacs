;; for vterm to work, i needed to brew install cmake libvterm
;; just installing cmake didn't work because it couldn't find glibtool to install libvterm itself.

(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "xonsh")
  (setq vterm-max-scrollback 100000)
  (setq vterm-kill-buffer-on-exit t)
  (add-to-list 'vterm-eval-cmds
			   '("update-pwd" vterm-update-pwd))
  ;; unbind some keys where i want them to act like emacs
  (define-key vterm-mode-map (kbd "C-v") nil)
  (define-key vterm-mode-map (kbd "C-f") nil)
  :bind (("C-c v" . vterm)))


(defun vterm-update-pwd (path)
  ;; (message "Updating vterm pwd to %s" path)
  (setq default-directory path))
