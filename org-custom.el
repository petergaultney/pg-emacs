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
