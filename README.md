check this repository out in a directory within your .emacs.d, e.g., "~/.emacs.d/local_config"

then add the following to your .emacs:

(add-to-list 'load-path "~/.emacs.d/local_config")
(load "peter_config.el")
