clone this repository into a directory within your .emacs.d, e.g., `~/.emacs.d/pg-emacs`

then add the following to your .emacs:

```
(add-to-list 'load-path "~/.emacs.d/pg-emacs")
(load "pg-emacs.el")
```

To also activate spacemacs side-by-side with pg emacs, further add the following:
```
(setq dotspacemacs-elpa-https nil)
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))
```

And then create a symlink to tell spacemacs that it's already configured: `ln -s ~/.emacs.d/pg-emacs/dotspacemacs.el ~/.spacemacs`
