(setq lexical-binding t)

(defun my/consult-ripgrep ()
  (interactive)
  (run-at-time 0 nil #'consult-ripgrep
     (universal-argument)
    ))

;; (defun my/consult-ripgrep (arg)
;;   (interactive "P")
;;   (run-at-time 0 nil #'consult-ripgrep arg))
;; works now? not sure why the old one broke. i think i updated consult, though.
;; now this doesn't work. i dooooo nooooot get it

(defun consult--ripgrep-noignore-builder (input)
  "consult--ripgrep-builder with INPUT, but ignores .gitignore."
  (let ((consult-ripgrep-args
         (if (string-match-p "--no-ignore-vcs" consult-ripgrep-args)
             consult-ripgrep-args
           (concat consult-ripgrep-args " --no-ignore-vcs"))))
    (consult--ripgrep-make-builder input)))

(defun consult-ripgrep-noignore (&optional dir initial)
  "Do consult-ripgrep with DIR and INITIAL, but without ignoring."
  (interactive "P")
  (consult--grep "Ripgrep"
                 #'consult--ripgrep-noignore-builder
                 (if dir dir t)  ;; Here the directory prompt is called by default to avoid searching from the project root
                 initial))

(defun consult--ripgrep-case-sensitive-builder (input)
  "consult--ripgrep-builder with INPUT, but case sensitive."
  (let ((consult-ripgrep-args
         (string-replace "--smart-case" "" consult-ripgrep-args)))
    (consult--ripgrep-make-builder input)))

(defun consult-ripgrep-dumb-case (&optional dir initial)
  "Do consult-ripgrep with DIR and INITIAL, but case sensitive."
  (interactive "P")
  (consult--grep "Ripgrep"
                 #'consult--ripgrep-case-sensitive-builder
                 (if dir dir t)  ;; Here the directory prompt is called by default to avoid searching from the project root
                 initial))
