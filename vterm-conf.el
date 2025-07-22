;; for vterm to work, i needed to brew install cmake libvterm
;; just installing cmake didn't work because it couldn't find glibtool to install libvterm itself.

(use-package
  vterm
  :ensure t
  :bind (("C-c v" . vterm))
  :config
  (setq vterm-shell "xonsh")
  (setq vterm-max-scrollback 100000)
  (setq vterm-kill-buffer-on-exit t)
  (add-to-list 'vterm-eval-cmds '("update-pwd" vterm-update-pwd)) ;; needs shell support
  (add-to-list 'vterm-eval-cmds '("e" (lambda (path) (find-file path)))) ;; needs shell support
  ;; unbind some keys where i want them to act like emacs
  (define-key vterm-mode-map (kbd "C-v") nil)
  (define-key vterm-mode-map (kbd "C-f") nil)
  (define-key vterm-mode-map (kbd "C-b") nil)
  (advice-add 'vterm--set-title :override #'my-vterm-override-set-title)
  :hook (vterm-mode . (lambda () (setq-local scroll-margin 1))))


(defun vterm-update-pwd (path)
  ;; (message "Updating vterm pwd to %s" path)
  (setq default-directory path))


(defun my-vterm-generate-buffer-name (raw-title)
  "Parse raw vterm title and generate the desired buffer name."
  (let*
    (
      (parts (split-string raw-title "|" t "[ ]+"))
      (process (file-name-nondirectory (or (car parts) "")))
      (dir-info (or (cl-find-if (lambda (s) (string-match ": " s)) parts) ""))
      (dir (and (string-match ": \\(.*\\)$" dir-info) (match-string 1 dir-info))))
    (when dir
      (if (memq (intern-soft process) '(mise xonsh zsh bash))
        (format "vterm %s" dir)
        (format "vterm %s %s" process dir)))))


(defun my-vterm-override-set-title (title)
  "Completely override vterm's title setting to use our custom name."
  ;; If our function generates a name, use it. Otherwise, do nothing.
  (when-let ((new-name (my-vterm-generate-buffer-name title)))
    (rename-buffer new-name t)))
