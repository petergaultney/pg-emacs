(tab-bar-mode)

(defun my-format-tab-name-with-number (tab)
  "Format tab name with a number prefix (1-9)."
  (let*
    (
      (tabs (tab-bar-tabs))
      (idx (cl-position tab tabs)) ; Find the 0-based index of the tab
      (name (funcall tab-bar-default-tab-name tab)))
    (if (and idx (< idx 9))
      (format "%d:%s" (1+ idx) name) ; Display 1-based index
      name)))

(setq tab-bar-tab-name-function #'my-format-tab-name-with-number)
(setq tab-bar-tab-name-function nil)


(defhydra
  hydra-tab
  (:timeout 3)
  "tab"
  ("n" tab-new "new")
  ("d" tab-close "close")
  ("1"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 0))
    "switch to 1")
  ("2"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 1))
    "switch to 2")
  ("3"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 2))
    "switch to 3")
  ("4"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 3))
    "switch to 4")
  ("5"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 4))
    "switch to 5")
  ("6"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 5))
    "switch to 6")
  ("7"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 6))
    "switch to 7")
  ("8"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 7))
    "switch to 8")
  ("9"
    (lambda ()
      (interactive)
      (tab-bar-select-tab 8))
    "switch to 9")
  ("e" tab-next "next")
  ("a" tab-previous "previous")
  ("r" tab-rename "rename")
  ("s" tab-switch "switch")
  ("q" nil "quit")
  ("g" nil "quit")
  ("<escape>" nil "quit" :exit t))

(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-t") 'hydra-tab/body)
