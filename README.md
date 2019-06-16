clone this repository into a directory within your .emacs.d, e.g., `~/.emacs.d/dauphin-emacs`

then add the following to your .emacs:

```
(add-to-list 'load-path "~/.emacs.d/dauphin-emacs")
(load "dauphin-config.el")
```

To also activate spacemacs side-by-side with dauphin emacs, further add the following:
```
(setq dotspacemacs-elpa-https nil)
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))
```

And then create a symlink to tell spacemacs that it's already configured: `ln -s ~/.emacs.d/dauphin-emacs/dotspacemacs.el ~/.spacemacs`
