(eval-when-compile (require 'cl))
(require 'vc-git)
(require 'helm)
(require 'helm-fix-multiline-process)

(defun helm-git-grep-find-git-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun helm-git-grep-find-git-submodule-root ()
  (vc-git-root (or (buffer-file-name) default-directory)))

(defun helm-git-grep-process ()
  (helm-aif (helm-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process "git-grep-process" nil
               "git" "--no-pager" "grep" "--full-name" "-n" "--no-color"
               (nbutlast
                (apply 'append
                       (mapcar
                        (lambda (x) (list "-e" x "--and"))
                        (split-string helm-pattern " +" t))))))
    '()))

(defun helm-git-submodule-grep-process ()
  (helm-aif (helm-attr 'default-directory)
      (let ((default-directory it))
        (apply 'start-process
               "git-submodule-grep-process" nil
               (list
                "git" "--no-pager" "submodule" "--quiet" "foreach"
                (format "git grep --full-name -n --no-color %s | sed s!^!$path/!"
                        (mapconcat (lambda (x)
                                     (format "-e %s " (shell-quote-argument x)))
                                   (split-string helm-pattern " +" t)
                                   "--and ")))))
    '()))

(defvar helm-source-git-grep
  '((name . "Git Grep")
    (init . (lambda () (helm-attrset
                        'default-directory
                        (helm-git-grep-find-git-root))))
    (default-directory . nil)
    (candidates-process . helm-git-grep-process)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar helm-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (init . (lambda () (helm-attrset
                        'default-directory
                        (helm-git-grep-find-git-submodule-root))))
    (candidates-process . helm-git-submodule-grep-process)
    (default-directory . nil)
    (type . file-line)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)))

;;;###autoload
(defun helm-git-grep ()
  "Helm git grep"
  (interactive)
  (helm-other-buffer '(helm-source-git-grep
                           helm-source-git-submodule-grep)
   "*helm git grep"))

;;;###autoload
(defun helm-git-grep-from-here ()
  "Helm git grep with current symbol using `helm'."
  (interactive)
  (helm :sources '(helm-source-git-grep
                       helm-source-git-submodule-grep)
            :input (thing-at-point 'symbol)))

(provide 'helm-git-grep)
