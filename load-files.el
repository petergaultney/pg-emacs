;; local files that need to be loaded
(load "funcs.el")
(load "smarter_move_beginning_of_line")

(load "better-defaults.el")
(load "window-half-scroll.el")
(load "compile-window-placement.el")
(load "mac-copy.el")
(load "find-file-in-project.el")
(load "z-helpful.el")

(load "coding-standards.el")
(load "unfill-paragraph.el")
;; (load "god-mode-config.el")
(load "indentation.el")
(load "javascript.el")
(load "typescript.el")

(require 'header2)

(load "Highlight-Indentation-for-Emacs/highlight-indentation.el")
;; (load "vtl")
(load "dos-eol")
(load "python-conf.el")

(require 'avy-de)
; (add-to-list 'custom-theme-load-path "~/.emacs.d/local_config/themes/")
										; (load-theme 'zenburn t)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist  '("\\.yml\\'" . yaml-mode))

(load "auto-correct.el")
(require 'auto-correct)
;; (load "lsp-config.el")
;; (load "ivy-config.el")
(load "helm-config.el")
(load "projectile-conf.el")
(require 'new-python)
(load "github-copilot.el")

(use-package sqlite-mode-extras
  :straight nil  ;; this avoids a weird error where straight tries to download something it can't find.
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
(require 'org-roam-config)
(load "org-custom")  ;; sometimes this used to fail so it's at the very end
