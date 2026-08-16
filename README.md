clone this repository into a directory within your .emacs.d, e.g., `~/.emacs.d/pg-emacs`

## External dependencies

These must be installed separately before Emacs will work correctly:

### Python (new-python.el)

```
uv tool install basedpyright   # LSP server: types, completions, go-to-def
uv tool install ruff            # formatter + import sorting (runs on save)
```

(`uv` itself can be installed via `brew install uv` or `curl -LsSf https://astral.sh/uv/install.sh | sh`)

add this to `~/.emacs.d/early-init.el`:

```
(setq package-enable-at-startup nil) ;; for elpaca
```

then add the following to your `~/.emacs` — this is the only per-machine
bootstrap, since `~/.emacs` lives outside this repo:

```
(load (expand-file-name "pg-emacs/pg-emacs.el" user-emacs-directory))
```

Note that this deliberately does **not** put the checkout on `load-path`.
Everything in here loads by resolved path via `pg-load` (defined at the top of
`pg-emacs.el`). While the directory was on `load-path`, every file in it competed
with Emacs' own libraries in a flat, global namespace of file basenames — and won,
since it sat ahead of the built-in lisp directories. That is how a local
`warnings.el` once shadowed `emacs-lisp/warnings.el` and stopped Emacs from
starting at all. Adding the directory back reintroduces that hazard for all ~1300
built-in library names that aren't preloaded.

`pg-load` loads each file at most once, so it also serves as `require` for files
in here: a file that depends on another calls `(pg-load "other.el")` at the point
of use, rather than relying on its position in `load-files.el`. Our own files
therefore don't need `provide`; the vendored third-party ones keep theirs.

To also activate spacemacs side-by-side with pg emacs, further add the following:
```
(setq dotspacemacs-elpa-https nil)
(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
(load-file (concat spacemacs-start-directory "init.el"))
```

And then create a symlink to tell spacemacs that it's already configured: `ln -s ~/.emacs.d/pg-emacs/dotspacemacs.el ~/.spacemacs`
