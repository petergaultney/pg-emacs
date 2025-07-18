(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
  '(c-basic-offset 4)
  '(c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "linux")))
  '(color-identifiers-avoid-faces '(font-lock-comment-face))
  '(color-identifiers:num-colors 20 t)
  '
  (copilot-node-executable
    (let
      (
        (node
          (replace-regexp-in-string
            "\12$"
            ""
            (shell-command-to-string "command -v node"))))
      node))
  '
  (custom-safe-themes
    '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
  '(ediff-split-window-function 'split-window-vertically)
  '(elpy-rpc-python-command "python3")
  '(eval-expression-debug-on-error nil)
  '(evil-move-beyond-eol t)
  '(evil-want-Y-yank-to-eol nil)
  '(fill-column 90)
  '(flycheck-disabled-checkers '(python-mypy))
  '(frog-menu-avy-padding t)
  '(gc-cons-threshold 100000000)
  '(global-auto-revert-mode t)
  '(global-color-identifiers-mode t)
  '(global-hl-line-mode nil)
  '(header-auto-update-enabled t t)
  '(helm-ff-DEL-up-one-level-maybe t nil nil "where is this going?")
  '(helm-ff-auto-update-initial-value nil)
  '(ibuffer-saved-filter-groups nil)
  '
  (ibuffer-saved-filters
    '
    (("vterm" (name . "vterm"))
      ("programming"
        (or (derived-mode . prog-mode) (mode . ess-mode) (mode . compilation-mode)))
      ("text document" (and (derived-mode . text-mode) (not (starred-name))))
      ("TeX"
        (or (derived-mode . tex-mode)
          (mode . latex-mode)
          (mode . context-mode)
          (mode . ams-tex-mode)
          (mode . bibtex-mode)))
      ("web"
        (or (derived-mode . sgml-mode)
          (derived-mode . css-base-mode)
          (derived-mode . js-base-mode)
          (derived-mode . typescript-ts-base-mode)
          (mode . js2-mode)
          (derived-mode . haml-mode)
          (mode . sass-mode)))
      ("gnus"
        (or (mode . message-mode)
          (mode . mail-mode)
          (mode . gnus-group-mode)
          (mode . gnus-summary-mode)
          (mode . gnus-article-mode)))))
  '(ido-enable-flex-matching t)
  '(ido-show-dot-for-dired t)
  '(indent-tabs-mode nil)
  '(js-indent-level 2)
  '(lsp-log-io t)
  '(lua-indent-level 4)
  '(lua-prefix-key "C-c")
  '
  (minibuffer-prompt-properties
    '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  '(org-default-priority 68)
  '(org-export-backends '(ascii html icalendar latex md odt))
  '(org-hide-leading-stars t)
  '(org-highest-priority 65)
  '(org-lowest-priority 68)
  '(org-priority-default 68)
  '(org-priority-highest 65)
  '(org-priority-lowest 68)
  '(org-replace-disputed-keys nil)
  '(org-return-follows-link t)
  '(org-startup-indented t)
  '
  (org-todo-keywords
    '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)")))
  '
  (package-archives
    '
    (("melpa" . "https://melpa.org/packages/")
      ("elpa" . "https://elpa.gnu.org/packages/")))
  '(package-selected-packages nil)
  '(persp-show-modestring 'header)
  '(project-vc-extra-root-markers '("pyproject.toml"))
  '(python-flymake-command '("mypy"))
  '
  ;; (safe-local-variable-values
  ;;   '
  ;;   ((eval setq-local consult-find-args "find . -type f -name '*' | sort -r")
  ;;     (c-offsets-alist
  ;;       (inexpr-class . +)
  ;;       (inexpr-statement . +)
  ;;       (lambda-intro-cont . +)
  ;;       (inlambda . c-lineup-inexpr-block)
  ;;       (template-args-cont c-lineup-template-args +)
  ;;       (incomposition . +)
  ;;       (inmodule . +)
  ;;       (innamespace . +)
  ;;       (inextern-lang . +)
  ;;       (composition-close . 0)
  ;;       (module-close . 0)
  ;;       (namespace-close . 0)
  ;;       (extern-lang-close . 0)
  ;;       (composition-open . 0)
  ;;       (module-open . 0)
  ;;       (namespace-open . 0)
  ;;       (extern-lang-open . 0)
  ;;       (objc-method-call-cont
  ;;         c-lineup-ObjC-method-call-colons
  ;;         c-lineup-ObjC-method-call
  ;;         +)
  ;;       (objc-method-args-cont . c-lineup-ObjC-method-args)
  ;;       (objc-method-intro . [0])
  ;;       (friend . 0)
  ;;       (cpp-define-intro c-lineup-cpp-define +)
  ;;       (cpp-macro-cont . +)
  ;;       (cpp-macro . [0])
  ;;       (inclass . +)
  ;;       (stream-op . c-lineup-streamop)
  ;;       (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
  ;;       (arglist-cont c-lineup-gcc-asm-reg 0)
  ;;       (arglist-intro . +)
  ;;       (catch-clause . 0)
  ;;       (else-clause . 0)
  ;;       (do-while-closure . 0)
  ;;       (label . 2)
  ;;       (access-label . -)
  ;;       (substatement-label . 2)
  ;;       (substatement . +)
  ;;       (statement-case-open . 0)
  ;;       (statement-case-intro . +)
  ;;       (statement-block-intro . +)
  ;;       (statement-cont . +)
  ;;       (statement . 0)
  ;;       (brace-entry-open . 0)
  ;;       (brace-list-entry . 0)
  ;;       (brace-list-intro . +)
  ;;       (brace-list-close . 0)
  ;;       (brace-list-open . 0)
  ;;       (block-close . 0)
  ;;       (inher-cont . c-lineup-multi-inher)
  ;;       (inher-intro . +)
  ;;       (member-init-cont . c-lineup-multi-inher)
  ;;       (member-init-intro . +)
  ;;       (annotation-var-cont . +)
  ;;       (annotation-top-cont . 0)
  ;;       (topmost-intro-cont . c-lineup-topmost-intro-cont)
  ;;       (topmost-intro . 0)
  ;;       (knr-argdecl . 0)
  ;;       (func-decl-cont . +)
  ;;       (inline-close . 0)
  ;;       (inline-open . +)
  ;;       (class-close . 0)
  ;;       (class-open . 0)
  ;;       (defun-block-intro . +)
  ;;       (defun-close . 0)
  ;;       (defun-open . 0)
  ;;       (string . c-lineup-dont-change)
  ;;       (arglist-close . c-lineup-arglist)
  ;;       (substatement-open . 0)
  ;;       (case-label . 0)
  ;;       (block-open . 0)
  ;;       (c . 1)
  ;;       (comment-intro . 0)
  ;;       (knr-argdecl-intro . -))
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
  ;;     (c-basic-offset. 4)
  ;;     (c-tab-always-indent . t)
  ;;     (header-auto-update-enabled)))
  '(smerge-command-prefix "(kbd \"C-c m\")")
  '(standard-indent 4)
  '(tab-width 4)
  '(treesit-font-lock-level 4)
  '(typescript-indent-level 2)
  '(vertico-buffer-display-action '(display-buffer-use-least-recent-window))
  '
  (vterm-eval-cmds
    '
    (("find-file" find-file)
      ("message" message)
      ("vterm-clear-scrollback" vterm-clear-scrollback)))
  '(warning-suppress-types '((emacs)))
  '(web-mode-code-indent-offset 2)
  '(web-mode-markup-indent-offset 2))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '
  (avy-lead-face
    (
      (t
        (:inherit
          (bold ef-themes-reset-soft)
          :background "black"
          :foreground "mediumseagreen"))))
  '(error ((t (:background "red" :foreground "black"))))
  '(flymake-errline ((((class color) (background light)) (:background "#500"))) t)
  '(flymake-error ((((class color) (background light)) (:background "#500"))))
  '
  (flymake-infoline
    ((((class color) (background light)) (:background "DarkGreen" :foreground "White"))))
  '(font-lock-bracket-face ((t (:foreground "orange"))))
  '(font-lock-builtin-face ((t (:foreground "gold"))))
  '(font-lock-function-name-face ((t (:foreground "dodgerblue3" :weight bold))))
  '(helm-ff-directory ((t (:extend t :foreground "cornflowerblue"))))
  '(helm-ff-dotted-directory ((t (:extend t :foreground "DimGray"))))
  '(helm-ff-executable ((t (:extend t :foreground "orangered"))))
  '
  (helm-selection
    ((t (:extend t :background "lightseagreen" :distant-foreground "black"))))
  '(outline-1 ((t (:inherit nil :foreground "blue" :weight bold))))
  '(outline-2 ((t (:foreground "green" :weight bold))))
  '(outline-3 ((t (:foreground "yellow" :weight bold))))
  '(outline-4 ((t (:foreground "orange"))))
  '(outline-5 ((t (:foreground "red"))))
  '(outline-6 ((t (:foreground "purple"))))
  '(region ((t (:extend t :inverse-video t)))))
