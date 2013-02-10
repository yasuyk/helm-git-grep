(eval-when-compile (require 'cl))
(require 'vc-git)
(require 'anything-config)
(require 'anything-fix-multiline-process)
(provide 'anything-git-grep)

(defun anything-git-grep-find-git-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun anything-git-grep-find-git-submodule-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun anything-git-grep-process ()
  (anything-aif (anything-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process "git-grep-process" nil
               "git" "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (nbutlast
                (apply 'append
                       (mapcar
                        (lambda (x) (list "-e" x "--and"))
                        (split-string anything-pattern " +" t))))))
    '()))

(defun anything-git-submodule-grep-process ()
  (anything-aif (anything-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process
               "git-submodule-grep-process" nil
               (list
                "git" "--no-pager" "submodule" "--quiet" "foreach"
                (format "git grep --full-name -n --no-color %s | sed s!^!$path/!"
                        (mapconcat (lambda (x)
                                     (format "-e %s " (shell-quote-argument x)))
                                   (split-string anything-pattern " +" t)
                                   "--and ")))))
    '()))

(defvar anything-c-source-git-grep
  '((name . "Git Grep")
    (init . (lambda () (anything-attrset
                        'default-directory
                        (anything-git-grep-find-git-root))))
    (default-directory . nil)
    (candidates . anything-git-grep-process)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar anything-c-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (init . (lambda () (anything-attrset
                        'default-directory
                        (anything-git-grep-find-git-submodule-root))))
    (candidates . anything-git-submodule-grep-process)
    (default-directory . nil)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defun anything-git-grep ()
  "Anything git grep"
  (interactive)
  (anything-other-buffer '(anything-c-source-git-grep
                           anything-c-source-git-submodule-grep)
   "*anything git grep"))

(defun anything-git-grep-from-here ()
  "Anything git grep with current symbol using `anything'."
  (interactive)
  (anything :sources '(anything-c-source-git-grep
                       anything-c-source-git-submodule-grep)
            :input (thing-at-point 'symbol)))
