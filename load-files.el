;; local files that need to be loaded
(load "funcs.el")
(load "smarter_move_beginning_of_line")

(load "window-half-scroll.el")
(load "compile-window-placement.el")
(load "mac-copy.el")
(load "gptel-conf.el")
(load "windows.el") ;; this ABSOLUTELY MUST remain above consult-config.el, because it does a setq on display-buffer-alist

;; (load "find-file-in-project.el")

(load "coding-standards.el")
(load "unfill-paragraph.el")
;; (load "god-mode-config.el")
(load "indentation.el")

(require 'header2)

(load "Highlight-Indentation-for-Emacs/highlight-indentation.el")
;; (load "vtl")
(load "dos-eol")

(require 'avy-de)
; (add-to-list 'custom-theme-load-path "~/.emacs.d/local_config/themes/")
										; (load-theme 'zenburn t)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist  '("\\.yml\\'" . yaml-mode))

(load "auto-correct.el")
(require 'auto-correct)
(load "github-copilot.el")
(require 'new-python)

(use-package sqlite-mode-extras
  ;; :straight nil  ;; this avoids a weird error where straight tries to download something it can't find.
  :demand  ;; not sure what this is for
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(load "magit-conf.el")
;; (require 'org-roam-config)
(load "org-custom")  ;; sometimes this used to fail so it's at the very end

(load "consult-config.el")
(load "better-defaults.el")  ;; needs to see that helm/consult/whatever is loaded

;; (load "z-helpful.el")  ;; broken as of 2023-12-08 with Wrong type argument: 1,listp
(load "projectile-conf.el")
(load "javascript.el")
(load "ansi-color-conf.el")
(load "themes.el")
(load "obsidian.el")
(load "shell-conf.el")
(load "keyfind.el")
(load "files.el")
(load "elisp-conf.el")
