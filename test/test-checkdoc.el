(add-to-list 'load-path default-directory)
(defvar helm-git-grep-el "helm-git-grep.el")

;; test byte-comple
(mapc #'byte-compile-file `(,helm-git-grep-el))

;; test checkdoc
(with-current-buffer (find-file-noselect helm-git-grep-el)
    (let ((checkdoc-diagnostic-buffer "*warn*"))
      (checkdoc-current-buffer t)))
