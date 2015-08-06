(require 'header2)
;;(require 'multiple-cursors)

(require 'markdown-mode)

(require 'key-chord)
(autoload 'key-chord-mode "key-chord-mode")
(key-chord-define-global ",b" 'switch-to-previous-buffer)
(key-chord-define-global "fk" 'scroll-down)
(key-chord-define-global "fj" 'scroll-up)

(define-globalized-minor-mode my-global-key-chord-mode key-chord-mode
  (lambda () (key-chord-mode 1)))

(my-global-key-chord-mode 1)

;;
;; indenting things
;; (setq-default indent-tabs-mode nil)
;; (smart-tabs-add-language-support c++ c++-mode-hook
;;   ((c-indent-line . c-basic-offset)
;;    (c-indent-region . c-basic-offset)))
;; (smart-tabs-insinuate 'c 'c++ 'javascript 'python)
;; (add-hook 'c-mode-common-hook
;;           (lambda () (setq indent-tabs-mode t)))

(require 'ace-jump-mode)
;;
;; ace jump mode major function
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "M-s") 'ace-jump-mode)

;; this seems necessary for it to work in dired. not sure why...
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "s") 'ace-jump-mode)))       
  

;; enable a more powerful jump back function from ace jump mode

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

; (add-to-list 'custom-theme-load-path "~/.emacs.d/local_config/themes/")
; (load-theme 'zenburn t)

; (add-to-list 'load-path "~/.emacs.d/local_config/helm")
; (require 'helm-config) ;; still not sure how much i'll use this
