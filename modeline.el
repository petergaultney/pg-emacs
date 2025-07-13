(use-package
  dim
  :ensure t
  :config
  (dim-minor-names
    '
    ((visual-line-mode " ↩")
      (abbrev-mode " ⌨")
      (auto-fill-function " ↵")
      (eldoc-mode "" eldoc)
      (color-identifiers-mode " 🍭")
	  (which-key-mode "")
      (whitespace-mode " _" whitespace)
      (paredit-mode " ()" paredit))))
