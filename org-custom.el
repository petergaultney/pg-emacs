(global-set-key (kbd "C-c o a") 'org-agenda-list)
(global-set-key (kbd "C-c o t") 'org-todo-list)

(require 'org)
(require 'org-install)

(defun insert-org-timestamp-now ()
  "Does what it says."
  (interactive)
  (require 'org-clock)
  (org-insert-time-stamp (org-current-time org-clock-rounding-minutes) 'with-hm))
(define-key global-map (kbd "C-c o n") 'insert-org-timestamp-now)
;; (eval-after-load "org-mode"
;;   '(define-key org-mode-map (kbd "C-c o n") 'org-time-stamp))

(defun org ()
  "Switch to my org dir."
  (interactive)
  (find-file "~/org"))
(defun notes ()
  "Switch to my notes dir."
  (interactive)
  (find-file "~/org/notes"))
;; http://members.optusnet.com.au/~charles57/GTD/orgmode.html#sec-2
(defun gtd ()
  "Open my GTD file"
  (interactive)
  (find-file "~/org/gtd.org"))
(defun work-notes ()
  "Switch to my work dir."
  (interactive)
  (find-file "~/org/xoi/xoi.org"))
(defun pork ()
  "Open my GTD file"
  (interactive)
  (find-file "~/org/gtd.org"))

(define-key global-map (kbd "C-c w") 'work-notes)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
