(use-package
  kubernetes-tramp
  :ensure t
  :config
  (push
    (cons
      "k8s"
      '
      ((tramp-login-program "kubectl")
        (tramp-login-args (("exec") ("-it") ("%h") ("--") ("/bin/bash")))
        (tramp-remote-shell "/bin/sh")
        (tramp-remote-shell-args ("-i") ("-c"))))
    tramp-methods))
