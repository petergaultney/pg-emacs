;;; pg-emacs.el ---
;;; this is where everything starts....
;; (setq debug-on-message "Package cl is deprecated")

;; BEGIN "VERY SAFE" config - needs no packages (i hope!)

;; ask emacs to save settings in a special file:
;; from http://tychoish.com/rhizome//useful-emacs-and-orgmode-hacks/
(setq pg-emacs-dir (file-name-directory (or load-file-name (buffer-file-name))))
;; load-file-name is only non-nil if using load-file.
;; the or buffer-file-name is for when you're using eval-buffer or some other form of eval
(buffer-file-name)
(setq pg-emacs-config-file load-file-name) ;; save for later use
(setq custom-file (concat pg-emacs-dir "custom.el"))
(menu-bar-mode 0)
(global-font-lock-mode t)
(show-paren-mode 1)
(column-number-mode t)
(set-face-attribute 'highlight nil :background "#3e4446" :foreground 'unspecified)

;; DEFINE MY PERSONAL SHORTCUTS (that don't depend on other stuff)
;; equivalent to (define-key global-map ...)
;; https://stackoverflow.com/questions/906368/what-is-the-difference-between-global-set-key-and-define-key-global-map-in-e
(global-set-key (kbd "C-c ,") 'decrease-left-margin)
(global-set-key (kbd "C-c .") 'increase-left-margin)
(global-set-key (kbd "C-c d") 'delete-region)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-f") 'find-file-other-window)
(global-set-key (kbd "C-c g") 'abort-recursive-edit)

(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-c a") 'org-agenda)
; join line from top line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; MOUSE SCROLL (I know this means I'm weak)
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode 1)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)

  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))

;; SET A BUNCH OF VARIABLES - maybe these should someday go into customize?
(setq truncate-partial-width-windows nil)
(setq default-truncate-lines t)  ; i hate wrapping lines
(setq inhibit-startup-message t) ; startup screen is useless
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil) ;;emacs23
;; (fset 'yes-or-no-p 'y-or-n-p) ; y and n are easier
(setq set-mark-command-repeat-pop 1) ; C-u C-SPC uses mark ring. now C-SPC simply continues.
(setq next-screen-context-lines 5) ; 2 lines isn't enough context on pgdown
(setq scroll-preserve-screen-position t) ; this is a beautiful thing

(setq tab-stop-list (number-sequence 4 120 4))
(setq tramp-default-method "ssh")
;; END SET A BUNCH OF VARIABLES

;; END "VERY SAFE" config

(load "elpaca.el")

(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  ;; do it in a separate file so this doesn't break every time i upgrade elpaca.
  (elpaca-use-package-mode))

(load "errors.el")

;; load that custom file
(load custom-file 'no-error)

;; (load "treesit-config.el")

(use-package highlight-numbers
  :ensure t)
(use-package color-identifiers-mode
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook 'global-color-identifiers-mode))
(use-package elisp-autofmt
  :ensure t)

;; remove annoying prompt http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
;; (server-start)
;; (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))



;; TEMP FILES - DON'T LITTER MY WORKSPACE!!
(load "temp-files.el")
;; END TEMP FILES

;; CUSTOM FUNCTIONS
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))
(defun open-dotemacs ()
  (interactive)
  (find-file pg-emacs-dir))
(defun open-lclcfg ()
  (interactive)
  (find-file pg-emacs-config-file))
;; (defun switch-to-previous-buffer ()
;;   "Switch to previously open buffer.
;; Repeated invocations toggle between the two most recently open buffers."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

;; DEFINE KEY BINDINGS FOR CUSTOM FUNCTIONS
(global-set-key (kbd "C-c e") 'open-dotemacs)
(global-set-key (kbd "C-c C-m") 'reload-dotemacs)
(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)

(with-eval-after-load 'minibuffer
  (keymap-set minibuffer-local-completion-map "<down>" #'minibuffer-next-completion)
  (keymap-set minibuffer-local-completion-map "<up>" #'minibuffer-prev-completion))


(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(global-set-key "\M-Q" 'unfill-paragraph)

(load "minibuffer.el")
;; LOAD OTHER FILES WITH CUSTOM FUNCTIONS
(load "load-files.el")
(load (concat pg-emacs-dir "packages.el")) ;; manual hack to see if packages are working
(load "pg-replace.el")
(load "abbrevs.el")
(load "smerge-deactivate.el")
