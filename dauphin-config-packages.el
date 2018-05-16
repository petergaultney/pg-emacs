;; the package manager
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;; (setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; (package-initialize)

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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist  '("\\.yml\\'" . yaml-mode))
