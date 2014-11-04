(load "better-defaults.el")

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
  (find-file "~/.emacs.d/dauphin-emacs/dauphin-config.el"))
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; LOAD OTHER FILES WITH CUSTOM FUNCTIONS
(load "smarter_move_beginning_of_line")

;; DEFINE KEY BINDINGS FOR CUSTOM FUNCTIONS
(define-key global-map (kbd "C-c e") 'open-dotemacs)
(define-key global-map (kbd "C-c p") 'open-lclcfg)
(define-key global-map (kbd "C-c C-e") 'reload-dotemacs)
(define-key global-map (kbd "C-c b") 'switch-to-previous-buffer)

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

;; TABS and other coding things
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(c-basic-offset 4)
 '(c-default-style (quote ((java-mode . "java") (awk-mode . "awk") (other . "k&r"))))
 '(ido-mode (quote both) nil (ido))
 '(org-hide-leading-stars t)
 '(org-startup-indented t)
 '(safe-local-variable-values (quote ((c-offsets-alist (inexpr-class . +) (inexpr-statement . +) (lambda-intro-cont . +) (inlambda . c-lineup-inexpr-block) (template-args-cont c-lineup-template-args +) (incomposition . +) (inmodule . +) (innamespace . +) (inextern-lang . +) (composition-close . 0) (module-close . 0) (namespace-close . 0) (extern-lang-close . 0) (composition-open . 0) (module-open . 0) (namespace-open . 0) (extern-lang-open . 0) (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +) (objc-method-args-cont . c-lineup-ObjC-method-args) (objc-method-intro . [0]) (friend . 0) (cpp-define-intro c-lineup-cpp-define +) (cpp-macro-cont . +) (cpp-macro . [0]) (inclass . +) (stream-op . c-lineup-streamop) (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist) (arglist-cont c-lineup-gcc-asm-reg 0) (arglist-intro . +) (catch-clause . 0) (else-clause . 0) (do-while-closure . 0) (label . 2) (access-label . -) (substatement-label . 2) (substatement . +) (statement-case-open . 0) (statement-case-intro . +) (statement-block-intro . +) (statement-cont . +) (statement . 0) (brace-entry-open . 0) (brace-list-entry . 0) (brace-list-intro . +) (brace-list-close . 0) (brace-list-open . 0) (block-close . 0) (inher-cont . c-lineup-multi-inher) (inher-intro . +) (member-init-cont . c-lineup-multi-inher) (member-init-intro . +) (annotation-var-cont . +) (annotation-top-cont . 0) (topmost-intro-cont . c-lineup-topmost-intro-cont) (topmost-intro . 0) (knr-argdecl . 0) (func-decl-cont . +) (inline-close . 0) (inline-open . +) (class-close . 0) (class-open . 0) (defun-block-intro . +) (defun-close . 0) (defun-open . 0) (string . c-lineup-dont-change) (arglist-close . c-lineup-arglist) (substatement-open . 0) (case-label . 0) (block-open . 0) (c . 1) (comment-intro . 0) (knr-argdecl-intro . -)) (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi) (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks) (c-hanging-colons-alist (member-init-intro before) (inher-intro) (case-label after) (label after) (access-label after)) (c-hanging-braces-alist (substatement-open after) (brace-list-open after) (brace-entry-open) (defun-open after) (class-open after) (inline-open after) (block-open after) (block-close . c-snug-do-while) (statement-case-open after) (substatement after)) (c-comment-only-line-offset . 0) (c-tab-always-indent . t))))
 '(standard-indent 4)
 '(tab-width 4))
(setq tab-stop-list (number-sequence 4 120 4))

;; FACES
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color) (background light)) (:background "#500"))))
 '(flymake-infoline ((((class color) (background light)) (:background "DarkGreen" :foreground "White"))))
 '(font-lock-comment-face ((nil (:foreground "red"))))
 '(hi-pink ((t (:background "pink" :foreground "black"))))
 '(highlight ((((class color) (min-colors 8) (background light)) (:background "#080"))))
 '(org-hide ((nil (:foreground "black"))))
 '(region ((((class color) (min-colors 8) (background light)) (:background "#03c" :foreground "black"))))
 '(secondary-selection ((((class color) (min-colors 8) (background light)) (:background "yellow1" :foreground "black")))))

(load "dauphin-config-packages.el")
