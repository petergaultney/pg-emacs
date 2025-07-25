(customize-set-variable
  'c-default-style
  '((java-mode . "java") (awk-mode . "awk") (other . "linux")))
(customize-set-variable 'standard-indent 4)
(customize-set-variable 'tab-width 4)
(customize-set-variable 'c-basic-offset 4)
(customize-set-variable 'indent-tabs-mode 0)
(customize-set-variable 'header-auto-update-enabled t)

;; (customize-set-variable
;;   'c-offsets-alist
;;   '
;;   ((inexpr-class . +)
;;     (inexpr-statement . +)
;;     (lambda-intro-cont . +)
;;     (inlambda . c-lineup-inexpr-block)
;;     (template-args-cont c-lineup-template-args +)
;;     (incomposition . +)
;;     (inmodule . +)
;;     (innamespace . 0)
;;     (inextern-lang . +)
;;     (composition-close . 0)
;;     (module-close . 0)
;;     (namespace-close . 0)
;;     (extern-lang-close . 0)
;;     (composition-open . 0)
;;     (module-open . 0)
;;     (namespace-open . 0)
;;     (extern-lang-open . 0)
;;     (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
;;     (objc-method-args-cont . c-lineup-ObjC-method-args)
;;     (objc-method-intro . [0])
;;     (friend . 0)
;;     (cpp-define-intro c-lineup-cpp-define +)
;;     (cpp-macro-cont . +)
;;     (cpp-macro . [0])
;;     (inclass . +)
;;     (stream-op . c-lineup-streamop)
;;     (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
;;     (arglist-cont c-lineup-gcc-asm-reg 0)
;;     (arglist-intro . +)
;;     (catch-clause . 0)
;;     (else-clause . 0)
;;     (do-while-closure . 0)
;;     (label . 2)
;;     (access-label . -)
;;     (substatement-label . 2)
;;     (substatement . +)
;;     (statement-case-open . 0)
;;     (statement-case-intro . +)
;;     (statement-block-intro . +)
;;     (statement-cont . +)
;;     (statement . 0)
;;     (brace-entry-open . 0)
;;     (brace-list-entry . 0)
;;     (brace-list-intro . +)
;;     (brace-list-close . 0)
;;     (brace-list-open . 0)
;;     (block-close . 0)
;;     (inher-cont . c-lineup-multi-inher)
;;     (inher-intro . +)
;;     (member-init-cont . c-lineup-multi-inher)
;;     (member-init-intro . +)
;;     (annotation-var-cont . +)
;;     (annotation-top-cont . 0)
;;     (topmost-intro-cont . c-lineup-topmost-intro-cont)
;;     (topmost-intro . 0)
;;     (knr-argdecl . 0)
;;     (func-decl-cont . +)
;;     (inline-close . 0)
;;     (inline-open . 0)
;;     (class-close . 0)
;;     (class-open . 0)
;;     (defun-block-intro . +)
;;     (defun-close . 0)
;;     (defun-open . 0)
;;     (string . c-lineup-dont-change)
;;     (arglist-close . c-lineup-arglist)
;;     (substatement-open . 0)
;;     (case-label . 0)
;;     (block-open . 0)
;;     (c . 1)
;;     (comment-intro . 0)
;;     (knr-argdecl-intro . -)
;;     (c-cleanup-list
;;       scope-operator
;;       brace-else-brace
;;       brace-elseif-brace
;;       brace-catch-brace
;;       empty-defun-braces
;;       list-close-comma
;;       defun-close-semi)
;;     (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks)
;;     (c-hanging-colons-alist
;;       (member-init-intro before)
;;       (inher-intro)
;;       (case-label after)
;;       (label after)
;;       (access-label after))
;;     (c-hanging-braces-alist
;;       (substatement-open after)
;;       (brace-list-open after)
;;       (brace-entry-open)
;;       (defun-open after)
;;       (class-open after)
;;       (inline-open after)
;;       (block-open after)
;;       (block-close . c-snug-do-while)
;;       (statement-case-open after)
;;       (substatement after))
;;     (c-comment-only-line-offset . 0)
;;     (c-tab-always-indent . t)))
;; end customize-set-variables c-offset

(font-lock-add-keywords
  'c++-mode
  '(("nullptr" . font-lock-keyword-face) ("override" . font-lock-keyword-face)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Lua custom stuff
(defun my-lua-setup ()
  "Cconfigures indentation the way
I want it. Makes sure spaces are used for indentation, not tabs."
  (setq indent-tabs-mode nil)
  (setq lua-indent-level 4))
(add-hook 'lua-mode-hook 'my-lua-setup)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
