((log4e
   (:source #1="elpaca-menu-lock-file" :date (26696 16726 717680 0) :recipe
	 (:package "log4e" :repo "aki2o/log4e" :fetcher github :files
	   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		 "docs/*.texinfo"
		 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
		   "LICENSE" "README*" "*-pkg.el"))
	   :source "MELPA" :protocol https :inherit t :depth treeless :ref
	   "6d71462df9bf595d3861bfb328377346aceed422")))
  (codesearch
	(:source #1# :date (26696 16726 735867 0) :recipe
	  (:package "codesearch" :fetcher github :repo "abingham/emacs-codesearch" :files
		("codesearch.el" "listing-codesearch.el") :source "MELPA" :protocol https :inherit
		t :depth treeless :ref "c1989b5482106367a9b3565872dbee2028a61045")))
  (ripgrep
	(:source #1# :date (26696 16726 750076 0) :recipe
	  (:package "ripgrep" :repo "nlamirault/ripgrep.el" :fetcher github :files
		("ripgrep.el") :source "MELPA" :protocol https :inherit t :depth treeless :ref
		"b6bd5beb0c11348f1afd9486cbb451d0d2e3c45a")))
  (consult
	(:source #1# :date (26696 16726 761092 0) :recipe
	  (:package "consult" :repo "minad/consult" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"7146596b564fb0a52b5bff420f27454911f603c8")))
  (with-editor
	(:source #1# :date (26696 16726 770159 0) :recipe
	  (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"ca902ae02972bdd6919a902be2593d8cb6bd991b")))
  (magit-section
	(:source #1# :date (26696 16726 779418 0) :recipe
	  (:package "magit-section" :fetcher github :repo "magit/magit" :files
		("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el") :source
		"MELPA" :protocol https :inherit t :depth treeless :ref
		"925762e957c31e8bdf9e5630d2d99e98e4dc3abe")))
  (llama
	(:source #1# :date (26696 16726 786024 0) :recipe
	  (:package "llama" :fetcher github :repo "tarsius/llama" :files
		(:defaults ".dir-locals.el") :source "MELPA" :protocol https :inherit t :depth
		treeless :ref "9802c215a3eea748d9d7f81a1465850388006897")))
  (s
	(:source #1# :date (26696 16726 791969 0) :recipe
	  (:package "s" :fetcher github :repo "magnars/s.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"dda84d38fffdaf0c9b12837b504b402af910d01d")))
  (f
	(:source #1# :date (26696 16726 798158 0) :recipe
	  (:package "f" :fetcher github :repo "rejeep/f.el" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"931b6d0667fe03e7bf1c6c282d6d8d7006143c52")))
  (avy
	(:source #1# :date (26696 16726 803983 0) :recipe
	  (:package "avy" :repo "abo-abo/avy" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"933d1f36cca0f71e4acb5fac707e9ae26c536264")))
  (dash
	(:source #1# :date (26696 16726 810063 0) :recipe
	  (:package "dash" :fetcher github :repo "magnars/dash.el" :files
		("dash.el" "dash.texi") :source "MELPA" :protocol https :inherit t :depth treeless
		:ref "1de9dcb83eacfb162b6d9a118a4770b1281bcd84")))
  (parent-mode
	(:source #1# :date (26696 16726 816004 0) :recipe
	  (:package "parent-mode" :fetcher github :repo "Fanael/parent-mode" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"fbd49857ab2e4cd0c5611c0cc83f93711657b298")))
  (hjson-mode
	(:source #1# :date (26696 16726 821902 0) :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github :repo
		"hjson/hjson-emacs" :package "hjson-mode" :ref
		"45e62294337568de38a915d4daa251c012abe5a6")))
  (visual-fill-column
	(:source #1# :date (26696 16726 827977 0) :recipe
	  (:package "visual-fill-column" :fetcher codeberg :repo
		"joostkremers/visual-fill-column" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"e391b52922086ac38397a3325933900b6d90f9f0")))
  (adoc-mode
	(:source #1# :date (26696 16726 833811 0) :recipe
	  (:package "adoc-mode" :fetcher github :repo "bbatsov/adoc-mode" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"2c2eb8043623aa99d35aacbad2ee39188bf1bad3")))
  (xonsh-mode
	(:source #1# :date (26696 16726 839645 0) :recipe
	  (:package "xonsh-mode" :repo "seanfarley/xonsh-mode" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"7fa581524533a9b6b770426e4445e571a69e469d")))
  (lua-mode
	(:source #1# :date (26696 16726 845279 0) :recipe
	  (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
		(:defaults (:exclude "init-tryout.el")) :source "MELPA" :protocol https :inherit t
		:depth treeless :ref "d074e4134b1beae9ed4c9b512af741ca0d852ba3")))
  (spacemacs-theme
	(:source #1# :date (26696 16726 850860 0) :recipe
	  (:package "spacemacs-theme" :fetcher github :repo "nashamri/spacemacs-theme" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"6c74684c4d55713c8359bedf1936e429918a8c33")))
  (doom-themes
	(:source #1# :date (26696 16726 856366 0) :recipe
	  (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
		(:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el") :source "MELPA"
		:protocol https :inherit t :depth treeless :ref
		"88126db5e63d816533d0372cb99246b842cac74e")))
  (ef-themes
	(:source #1# :date (26696 16726 861944 0) :recipe
	  (:package "ef-themes" :repo "protesilaos/ef-themes" :files
		("*" (:exclude ".git" "COPYING" "doclicense.texi" "contrast-ratios.org")) :source
		"GNU ELPA" :protocol https :inherit t :depth treeless :host github :ref
		"3dd2f8fb425abdcbeb5e4b648fc48dbd508648eb")))
  (projectile-codesearch
	(:source #1# :date (26696 16726 867697 0) :recipe
	  (:package "projectile-codesearch" :fetcher github :repo "abingham/emacs-codesearch"
		:files ("projectile-codesearch.el") :source "MELPA" :protocol https :inherit t
		:depth treeless :ref "c1989b5482106367a9b3565872dbee2028a61045")))
  (projectile-ripgrep
	(:source #1# :date (26696 16726 872844 0) :recipe
	  (:package "projectile-ripgrep" :repo "nlamirault/ripgrep.el" :fetcher github :files
		("projectile-ripgrep.el") :source "MELPA" :protocol https :inherit t :depth
		treeless :host github :ref "b6bd5beb0c11348f1afd9486cbb451d0d2e3c45a")))
  (projectile
	(:source #1# :date (26696 16726 877519 0) :recipe
	  (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"e961fb976d8be6945c453fb740c90fcb3dca4bb2")))
  (orderless
	(:source #1# :date (26696 16726 882906 0) :recipe
	  (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"254f2412489bbbf62700f9d3d5f18e537841dcc3")))
  (wgrep
	(:source #1# :date (26696 16726 888730 0) :recipe
	  (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files
		("wgrep.el") :source "MELPA" :protocol https :inherit t :depth treeless :ref
		"49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")))
  (embark-consult
	(:source #1# :date (26696 16726 893925 0) :recipe
	  (:package "embark-consult" :repo "oantolin/embark" :fetcher github :files
		("embark-consult.el") :source "MELPA" :protocol https :inherit t :depth treeless
		:ref "2941f2ea36d61c1a84c3f79ebe47d604c9a92b5d")))
  (embark
	(:source #1# :date (26696 16726 899302 0) :recipe
	  (:package "embark" :repo "oantolin/embark" :fetcher github :files
		("embark.el" "embark-org.el" "embark.texi") :source "MELPA" :protocol https
		:inherit t :depth treeless :ref "2941f2ea36d61c1a84c3f79ebe47d604c9a92b5d")))
  (marginalia
	(:source #1# :date (26696 16726 904187 0) :recipe
	  (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"a527fb03b76a2bce1e360c6e73a095e06922c3f3")))
  (vertico
	(:source #1# :date (26696 16726 909721 0) :recipe
	  (:package "vertico" :repo "minad/vertico" :files
		(:defaults "extensions/vertico-*.el") :fetcher github :source "MELPA" :protocol
		https :inherit t :depth treeless :ref "751c9ad527f14750ef10543fa64b39cf778d6206")))
  (consult-projectile
	(:source #1# :date (26696 16726 914882 0) :recipe
	  (:package "consult-projectile" :fetcher gitlab :repo "OlMon/consult-projectile"
		:files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"400439c56d17bca7888f7d143d8a11f84900a406")))
  (org-fancy-priorities
	(:source #1# :date (26696 16726 920455 0) :recipe
	  (:package "org-fancy-priorities" :repo "harrybournis/org-fancy-priorities" :fetcher
		github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"7f677c6c14ecf05eab8e0efbfe7f1b00ae68eb1d")))
  (magit
	(:source #1# :date (26696 16726 926321 0) :recipe
	  (:package "magit" :fetcher github :repo "magit/magit" :files
		("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
		  ".dir-locals.el" (:exclude "lisp/magit-section.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :host github :ref
		"925762e957c31e8bdf9e5630d2d99e98e4dc3abe")))
  (transient
	(:source #1# :date (26696 16726 931250 0) :recipe
	  (:package "transient" :fetcher github :repo "magit/transient" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :host github :ref
		"f3f498aa155f88c7e2ab6d1d01d1361813059db8")))
  (eglot-booster
	(:source #1# :date (26696 16726 936570 0) :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github :repo
		"jdtsmith/eglot-booster" :package "eglot-booster" :ref
		"e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed")))
  (copilot
	(:source #1# :date (26696 16726 942410 0) :recipe
	  (:package "copilot" :fetcher github :repo "copilot-emacs/copilot.el" :files
		("dist" "*.el") :source "MELPA" :protocol https :inherit t :depth treeless :host
		github :ref "fe3f51b636dea1c9ac55a0d5dc5d7df02dcbaa48")))
  (ace-window
	(:source #1# :date (26696 16726 947714 0) :recipe
	  (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"77115afc1b0b9f633084cf7479c767988106c196")))
  (gptel-quick
	(:source #1# :date (26696 16726 953419 0) :recipe
	  (:source nil :protocol https :inherit t :depth treeless :host github :repo
		"karthink/gptel-quick" :package "gptel-quick" :ref
		"d7a3aedefdc656a136d5664f2dac165a1f6ebf0e")))
  (posframe
	(:source #1# :date (26696 16726 958413 0) :recipe
	  (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :host github :ref
		"81651536827c96bf5af5265ee7918ab70e1dd5b1")))
  (gptel
	(:source #1# :date (26696 16726 964123 0) :recipe
	  (:package "gptel" :repo "karthink/gptel" :fetcher github :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :host github :ref
		"0a7744610814da90a27cb8f6a090915f61bde4b8")))
  (markdown-mode
	(:source #1# :date (26696 16726 969366 0) :recipe
	  (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"7659bc470d096e7a53285fa246cbabcb07d66a33")))
  (elisp-autofmt
	(:source #1# :date (26696 16726 974863 0) :recipe
	  (:package "elisp-autofmt" :fetcher codeberg :repo "ideasman42/emacs-elisp-autofmt"
		:files (:defaults "elisp-autofmt.py" "elisp-autofmt.overrides.json") :source
		"MELPA" :protocol https :inherit t :depth treeless :ref
		"43a44dcbd17adf3b235de93e748321c7076f1c3c")))
  (color-identifiers-mode
	(:source #1# :date (26696 16726 989525 0) :recipe
	  (:package "color-identifiers-mode" :fetcher github :repo
		"ankurdave/color-identifiers-mode" :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"89343c624ae64f568b5305ceca3db48d65711863")))
  (highlight-numbers
	(:source #1# :date (26696 16726 995233 0) :recipe
	  (:package "highlight-numbers" :fetcher github :repo "Fanael/highlight-numbers"
		:old-names (number-font-lock-mode) :files
		("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
		  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
		  "docs/*.texinfo"
		  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
			"LICENSE" "README*" "*-pkg.el"))
		:source "MELPA" :protocol https :inherit t :depth treeless :ref
		"8b4744c7f46c72b1d3d599d4fb75ef8183dee307")))
  (elpaca-use-package
	(:source #1# :date (26696 16727 828 0) :recipe
	  (:package "elpaca-use-package" :wait t :repo
		"https://github.com/progfolio/elpaca.git" :files
		("extensions/elpaca-use-package.el") :main "extensions/elpaca-use-package.el"
		:build (:not elpaca--compile-info) :source "Elpaca extensions" :protocol https
		:inherit t :depth treeless :ref "722b36c7391cd7fd03400461fc74f26bee917287")))
  (elpaca
	(:source #1# :date (26696 16727 6449 0) :recipe
	  (:source nil :protocol https :inherit ignore :depth 1 :repo
		"https://github.com/progfolio/elpaca.git" :ref
		"722b36c7391cd7fd03400461fc74f26bee917287" :files
		(:defaults "elpaca-test.el" (:exclude "extensions")) :build
		(:not elpaca--activate-package) :package "elpaca"))))
