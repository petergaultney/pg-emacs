;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     javascript
     python
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ivy
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Droid Sans Mono for Powerline"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  )

(defun unset-evil-keys (keys)
  (interactive)
  (dolist (key keys)
    (print key debugbuff)
    (define-key evil-normal-state-map key nil)
    (define-key evil-motion-state-map key nil)
    (define-key evil-visual-state-map key nil)))

(defun set-evil-key (key func)
  (interactive)
  (define-key evil-normal-state-map key func)
  (define-key evil-motion-state-map key func)
  (define-key evil-visual-state-map key func))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (print "Setting up my user config..." debugbuff)

  (with-eval-after-load 'evil-maps
    ;; make NOIR my arrow keys
    (unset-evil-keys '("n" "o" "i" "r" "a" "e" ";" "f" "b" "d"))

    (set-evil-key "n" 'backward-char)
    (set-evil-key "o" 'forward-char)
    (set-evil-key "i" 'next-line)
    (set-evil-key "r" 'previous-line)

    ;; emacs movement everywhere
    (set-evil-key "a" 'mwim-beginning-of-code-or-line)
    (set-evil-key "e" 'end-of-line)

    ;; half-page scroll in all non-insert modes
    (set-evil-key "-" 'scroll-down-half)
    (set-evil-key "=" 'scroll-up-half)

    ;; cool new stuff
    (set-evil-key "f" 'forward-whitespace)
    (set-evil-key "b" 'backward-whitespace)
    (set-evil-key "t" 'avy-goto-char-timer)
    (set-evil-key "s" 'isearch-forward)

    ;; visual state stuff partially inherited from my Ergodox config
    (define-key evil-visual-state-map "w" 'kill-region)
    (define-key evil-visual-state-map "q" 'kill-ring-save)
    (define-key evil-visual-state-map ";" 'comment-dwim)

    ;; stuff that seems obvious to me
    (define-key evil-normal-state-map [tab] 'evil-indent)

   ;; my personal keybindings from Emacs
    ;; (define-key evil-normal-state-map "t" 'evil-insert)
    (define-key evil-normal-state-map "h" 'evil-insert-state)
    (define-key evil-normal-state-map (kbd "DEL") 'enter-insert-and-backward-delete-one)
    (define-key evil-normal-state-map (kbd "RET") 'evil-open-below)
    (define-key evil-normal-state-map "k" 'kill-line)
    (define-key evil-normal-state-map "d" 'delete-line)
    (define-key evil-normal-state-map "y" 'yank)
    (define-key evil-normal-state-map "p" 'evil-paste-pop)
    (define-key evil-normal-state-map "/" 'undo)
    (define-key evil-normal-state-map ";" 'comment-dwim)
    (define-key evil-motion-state-map "6" 'delete-indentation)

    ;; motion-specific things
    (define-key evil-motion-state-map "h" 'evil-normal-state))

  (with-eval-after-load 'magit-mode
    (print "loaded magit mode map" debugbuff)
    (define-key magit-mode-map "r" 'previous-line))

  ;; in ivy minibuffers, it's nice to be able to use my 'standard' movement keys
  ;; rather than the default C-n and C-p
  (with-eval-after-load 'counsel
    (print "loading after ivy minibuffer map" debugbuff)
    (define-key ivy-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "M-i") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "M-r") 'ivy-previous-line))

  ;; SPC i
  (spacemacs/declare-prefix "i" "my stuff")
  (spacemacs/set-leader-keys "in" 'evil-window-left)
  (spacemacs/set-leader-keys "io" 'evil-window-right)
  (spacemacs/set-leader-keys "i." 'end-of-buffer)
  (spacemacs/set-leader-keys "i," 'beginning-of-buffer)
  (spacemacs/set-leader-keys "im" 'evil-motion-state)
  ;; SPC c
  (spacemacs/declare-prefix "c" "Ctrl-C stuff")
  (spacemacs/set-leader-keys "c." 'increase-left-margin)
  (spacemacs/set-leader-keys "c," 'decrease-left-margin)
  (spacemacs/set-leader-keys "cn" 'windmove-left)
  (spacemacs/set-leader-keys "co" 'windmove-right)
  (spacemacs/set-leader-keys "ci" 'windmove-down)
  (spacemacs/set-leader-keys "cr" 'windmove-up)

  (setq-default evil-escape-key-sequence "gt")

  ;; evil-normal-state is preferred, so revert when idle
  (run-with-idle-timer 20 t 'evil-normal-state)

  ;; (setq evil-move-beyond-eol "t")
  (spacemacs/toggle-highlight-current-line-globally-off)
  (setq powerline-default-separator "utf-8")

  ;; more consistency in movement
  (global-set-key (kbd "C-n") 'backward-char)
  (global-set-key (kbd "C-o") 'forward-char)
  (global-set-key (kbd "C-r") 'previous-line)
  (global-set-key (kbd "M-n") 'backward-char)
  (global-set-key (kbd "M-o") 'forward-char)
  (global-set-key (kbd "M-i") 'next-line) ;; this relies on an iTerm2 mapping from Ctrl-I to Esc+-I
  (global-set-key (kbd "M-r") 'previous-line)
  (global-set-key [tab] 'evil-indent)

  ;; better avy keys
  (setq avy-keys '(?a ?s ?e ?t ?g ?y ?n ?i ?o ?h))

  (setq
   evil-normal-state-tag (propertize " <N> " 'face '((:background "red" :foreground "black")))
   evil-emacs-state-tag (propertize "  EMACS  " 'face '((:background "turquoise" :foreground "black")))
   evil-hybrid-state-tag (propertize " <H> " 'face '((:background "green" :foreground "black")))
   evil-replace-state-tag (propertize " REPLACE " 'face '((:background "dark orange" :foreground "black")))
   evil-motion-state-tag (propertize " <M> " 'face '((:background "khaki" :foreground "black")))
   evil-visual-state-tag (propertize " <V> " 'face '((:background "blue" :foreground "black")))
   evil-operator-state-tag (propertize " OPERATE " 'face '((:background "sandy brown" :foreground "black"))))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (orgit org-projectile org-category-capture org-present yapfify which-key wgrep use-package unfill smex smeargle pytest pyenv-mode py-isort pip-requirements pcre2el org-pomodoro org-mime org-fancy-priorities org-download mwim multiple-cursors mmm-mode markdown-toc magit-gitflow macrostep live-py-mode ivy-hydra hy-mode htmlize helm-make gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flycheck-flow flx exec-path-from-shell evil-visualstar evil-magit evil-escape elpy elisp-slime-nav diminish cython-mode counsel-projectile company-statistics company-anaconda bind-map auto-yasnippet auto-compile ace-window ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
