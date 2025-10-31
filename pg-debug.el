;; this is how we try out things on their own.
;; you need to load specific files at the bottom
;; and then you need to change .emacs to load this file instead of pg-emacs.el

(setq pg-emacs-dir (file-name-directory (or load-file-name (buffer-file-name))))

(load "elpaca.el")


(elpaca
  elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  ;; do it in a separate file so this doesn't break every time i upgrade elpaca.
  (elpaca-use-package-mode))


;; down here, we load whichever very specific files contain the use-package declarations
;; for things we want to debug.

;; (load "magit-conf.el")
(use-package
  frog-jump-buffer
  :ensure t
  :config (global-set-key (kbd "C-b") 'frog-jump-buffer))


;; comment the below out when you're done
