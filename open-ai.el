(use-package markdown-mode
  :ensure t)

(defun read-claude-api-key ()
  "Read the contents of my Claude API key file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "~/.claude-api-key"))
    (string-trim (buffer-string))))

(use-package gptel
  :ensure (:host github :repo "karthink/gptel" )
  :config
  (setq gptel-default-mode 'markdown-mode)
  (gptel-make-anthropic "Claude" :stream t :key #'read-claude-api-key)
;;  (gptel-openai "ChatGPT" :stream t
  )
