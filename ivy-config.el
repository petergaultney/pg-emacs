;; requires (package-install 'counsel) as well - Ctrl-X Ctrl-E
;; (use-package swiper
;;   :ensure t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (global-set-key "\C-s" 'swiper)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file))

;; (with-eval-after-load 'counsel
;;   (let ((done (where-is-internal #'ivy-done     ivy-minibuffer-map t))
;;         (alt  (where-is-internal #'ivy-alt-done ivy-minibuffer-map t)))
;;     (define-key counsel-find-file-map done #'ivy-alt-done)
;;     (define-key counsel-find-file-map alt  #'ivy-done)))
