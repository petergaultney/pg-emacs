;; Enable vertico-multiform
(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'dired-directory) ; or face 'dired-directory
    file))

;; Sort directories before files
(defun sort-directories-first (files)
  (setq files (vertico-sort-history-length-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as dired-directory"
  (let ((sym (intern cmd)))
    (if (or (eq sym -major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
      (propertize cmd 'face 'dired-directory)
      cmd)))

;; add-to-list works if 'file isn't already in the alist
;; setq can be used but will overwrite all existing values
(add-to-list 'vertico-multiform-categories
             '(file
               ;; this is also defined in the wiki, uncomment if used
               (vertico-sort-function . sort-directories-first)
               (+vertico-transform-functions . +vertico-highlight-directory)))
;; (add-to-list 'vertico-multiform-commands
;;              '((execute-extended-command
;;                 reverse
;;                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))
