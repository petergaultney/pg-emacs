(use-package eca
  :ensure (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el"))
  :commands (eca eca-chat-new eca-rewrite eca-complete)
  :config
  (setq eca-chat-use-side-window t)
  (setq eca-chat-window-side 'right))
