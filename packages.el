;; this is old package.el stuff. I am leaving it in place despite also
;; using straight.el because i'm not yet convinced that straight
;; is necessarily 100% better.
;; However, for most new packages, I will just install with use-package, which
;; is currently configured to use straight.el.
;; I may later decide to try out elpaca.

;; For loading things that are packages that may need to be installed.
;;; Code:

(require 'package)

(let*
  (
    (no-ssl (and (memq system-type '(windows-nt ms-dos)) (not (gnutls-available-p))))
    (proto
      (if no-ssl
        "http"
        "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "elpa" (concat proto "://elpa.gnu.org/packages/"))
    t)
  ;;  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
;; end use-package init

(use-package highlight-numbers :ensure t)
(use-package
  color-identifiers-mode
  :ensure t
  :config (add-hook 'elpaca-after-init-hook 'global-color-identifiers-mode))

;; (use-package multiple-cursors

;;   :ensure t
;;   :bind (("C-S-l" . mc/edit-lines)
;; 		 ("C-* n" . mc/mark-next-like-this)
;; 		 ("C-* p" . mc/mark-previous-like-this)
;; 		 ("C-* C-*" . mc/mark-all-like-this)
;; 		 ("C-c C-* C-*" . mc/mark-more-like-this)e

;; 		 ("C-* i" . mc/insert-numbers)
;; 		 ("C-* s" . mc/sort-regions)
;; 		 ("C-* r" . mc/reverse-regions)
;; 		 ("M-<mouse-1>" . mc/add-cursor-on-click))
;;   :init
;;   (global-unset-key (kbd "M-<down-mouse-1>"))
;;   :config
;;     (require 'mc-extras))

(use-package
  yaml-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package
  sqlite-mode-extras
  ;; :straight nil  ;; this avoids a weird error where straight tries to download something it can't find.
  :demand ;; not sure what this is for
  :bind
  (:map
    sqlite-mode-map
    ("n" . next-line)
    ("p" . previous-line)
    ("b" . sqlite-mode-extras-backtab-dwim)
    ("f" . sqlite-mode-extras-tab-dwim)
    ("<backtab>" . sqlite-mode-extras-backtab-dwim)
    ("<tab>" . sqlite-mode-extras-tab-dwim)
    ("RET" . sqlite-mode-extras-ret-dwim)))


(use-package scala-mode :interpreter ("scala" . scala-mode))

(use-package lua-mode :ensure t :interpreter ("lua" . lua-mode))

(eval-after-load "lispy"
  (progn
    ;; replace a global binding with own function
    ;; (define-key lispy-mode-map (kbd "C-e") 'my-custom-eol)
    ;; replace a global binding with major-mode's default
    ;; (define-key lispy-mode-map (kbd "C-j") nil)
    ;; replace a local binding
    ;; (define-key lispy-mode-map "n" 'lispy-left)
    ;; (define-key lispy-mode-map "i" 'lispy-down)
    ;; (define-key lispy-mode-map "o" 'lispy-right)
    ;; (define-key lispy-mode-map "r" 'lispy-up)
    ;; (define-key lispy-mode-map "[" 'lispy-brackets)
    ;; (define-key lispy-mode-map "u" 'lispy-backward)
    ;; (define-key lispy-mode-map "l" 'lispy-forward)
    ;; (define-key lispy-mode-map (kbd "C-d") nil)
    ))

;; (use-package cider-mode)
;; (require 'cider-mode)
;; (define-key cider-repl-mode-map (kbd "<prior>") (lambda () (interactive) (cider-repl-previous-input)))
;; (define-key cider-repl-mode-map (kbd "<next>") (lambda () (interactive) (cider-repl-next-input)))

;;; packages.el ends here

(use-package xonsh-mode :ensure t)

(use-package adoc-mode :ensure t)

(use-package visual-fill-column :ensure t)

(use-package hjson-mode :ensure (:host github :repo "hjson/hjson-emacs"))

(use-package
  expand-region
  :ensure (:host github :repo "petergaultney/expand-region.el")
  :bind (("C-c x" . er/expand-region))
  :init
  (setq expand-region-show-expansion-message nil)) ;; disable expansion message

(use-package
  diredfl
  :ensure t
  :init
  (diredfl-global-mode 1) ;; enable diredfl globally
  )

(use-package
  rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode) (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package
  which-key
  :ensure t
  :init (setq which-key-lighter "")
  :config (which-key-mode 1))
