;; the package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; end use-package init

(require 'header2)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-l" . mc/edit-lines)
		 ("C-* n" . mc/mark-next-like-this)
		 ("C-* p" . mc/mark-previous-like-this)
		 ("C-* C-*" . mc/mark-all-like-this)
		 ("C-c C-* C-*" . mc/mark-more-like-this)

		 ("C-* i" . mc/insert-numbers)
		 ("C-* s" . mc/sort-regions)
		 ("C-* r" . mc/reverse-regions)
		 ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :config
    (require 'mc-extras))

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

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package lua-mode
  :interpreter ("lua" . lua-mode))
