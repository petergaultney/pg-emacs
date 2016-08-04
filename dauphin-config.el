;;; dauphin-config.el ---
(load "better-defaults.el")
(load "window-half-scroll.el")
(load "compile-window-placement.el")
;; remove annoying prompt http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
;; (server-start)
;; (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; ask emacs to save settings in a special file:
;; from http://tychoish.com/rhizome//useful-emacs-and-orgmode-hacks/
(setq dauphin-emacs-dir (file-name-directory load-file-name))
(setq dauphin-emacs-config-file load-file-name) ;; save for later use
(setq custom-file (concat dauphin-emacs-dir "custom.el"))
;; load that custom file
(load custom-file 'noerror)

(load "coding-standards.el")
(load "unfill-paragraph.el")

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

(menu-bar-mode 0)
(global-font-lock-mode t)
(show-paren-mode 1)
(column-number-mode t)

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

;; TEMP FILES - DON'T LITTER MY WORKSPACE!!
(make-directory "~/.emacs.d/autosaves/" t)
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        ))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
;; END TEMP FILES

;; DEFINE MY PERSONAL SHORTCUTS (that don't depend on other stuff)
(define-key global-map (kbd "C-c ,") 'decrease-left-margin)
(define-key global-map (kbd "C-c .") 'increase-left-margin)
(define-key global-map (kbd "C-c d") 'delete-region)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "C-x s") 'save-buffer)

;; CUSTOM FUNCTIONS
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))
(defun open-dotemacs ()
  (interactive)
  (find-file "~/.emacs"))
(defun open-lclcfg ()
  (interactive)
  (find-file dauphin-emacs-config-file))
;; (defun switch-to-previous-buffer ()
;;   "Switch to previously open buffer.
;; Repeated invocations toggle between the two most recently open buffers."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

;; LOAD OTHER FILES WITH CUSTOM FUNCTIONS
(load "smarter_move_beginning_of_line")

;; DEFINE KEY BINDINGS FOR CUSTOM FUNCTIONS
(define-key global-map (kbd "C-c e") 'open-dotemacs)
(define-key global-map (kbd "C-c p") 'open-lclcfg)
(define-key global-map (kbd "C-c C-e") 'reload-dotemacs)
(define-key global-map (kbd "C-c b") 'switch-to-prev-buffer)


; join line from top line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; SET A BUNCH OF VARIABLES
(setq truncate-partial-width-windows nil)
(setq default-truncate-lines t)  ; i hate wrapping lines
(setq inhibit-startup-message t) ; startup screen is useless
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil) ;;emacs23
(fset 'yes-or-no-p 'y-or-n-p) ; y and n are easier
(setq set-mark-command-repeat-pop 1) ; C-u C-SPC uses mark ring. now C-SPC simply continues.
(setq next-screen-context-lines 5) ; 2 lines isn't enough context on pgdown
(setq scroll-preserve-screen-position t) ; this is a beautiful thing

(setq tab-stop-list (number-sequence 4 120 4))
(setq tramp-default-method "ssh")

(windmove-default-keybindings)

;; (load (concat dauphin-emacs-dir "dauphin-config-packages.el")) ;; manual hack to see if packages are working
;; then, finally, load my package-dependent things
(condition-case nil
    (load (concat dauphin-emacs-dir "dauphin-config-packages.el"))
  (error
   (message "Could not find a package. Attempting to download all packages.")
   (condition-case nil
       (load (concat dauphin-emacs-dir "autopackaging.el"))
     (error nil)
     )
   )
  )

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

(load "org-custom")
