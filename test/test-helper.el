(require 'ert)
(when (require 'undercover nil t)
  (undercover "helm-git-grep.el"))

(require 'helm-git-grep)
