;;; helm-git-grep.el --- helm for git grep

;; Copyright (C) 2013 mechairoi

;; Author: mechairoi
;; Maintainer: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.1
;; URL: https://github.com/yasuyk/helm-git-grep
;; Package-Requires: ((helm "1.0"))
;; Keywords: helm, git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add the following to your emacs init file:
;;
;; (require 'helm-git-grep) ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-c g") 'helm-git-grep)

;; Original version is anything-git-grep, and port to helm.
;; https://github.com/mechairoi/anything-git-grep

;;; Code:

(eval-when-compile (require 'cl))
(require 'vc-git)
(require 'helm)
(require 'helm-files)
(require 'helm-grep) ;; helm-grep-history and helm-grep-max-length-history
(require 'helm-elscreen) ;; helm-elscreen-find-file
(require 'helm-fix-multiline-process)

(declare-function elscreen-get-conf-list "ext:elscreen.el" (type))

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

(defun helm-git-grep-action (candidate &optional where mark)
  "Define a default action for `helm-git-grep' on CANDIDATE.
WHERE can be one of other-window, elscreen, other-frame."
  (let* ((lineno (nth 0 candidate))
         (fname (or (with-current-buffer
                        (if (eq major-mode 'helm-grep-mode)
                            (current-buffer)
                          helm-buffer)
                      (get-text-property (point-at-bol) 'help-echo))
                    (nth 2 candidate))))
    (case where
      (other-window (find-file-other-window fname))
      (elscreen     (helm-elscreen-find-file fname))
      (other-frame  (find-file-other-frame fname))
      (grep         (helm-grep-save-results-1))
      (t            (find-file fname)))
    (unless (or (eq where 'grep))
      (helm-goto-line lineno))
    (when mark
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))
    ;; Save history
    (unless (or helm-in-persistent-action
                (eq major-mode 'helm-grep-mode)
                (string= helm-pattern ""))
      (setq helm-grep-history
            (cons helm-pattern
                  (delete helm-pattern helm-grep-history)))
      (when (> (length helm-grep-history)
               helm-grep-max-length-history)
        (setq helm-grep-history
              (delete (car (last helm-grep-history))
                      helm-grep-history))))))

(defun helm-git-grep-other-window (candidate)
  "Jump to result in other window from helm git grep."
  (helm-git-grep-action candidate 'other-window))

(defun helm-git-grep-other-frame (candidate)
  "Jump to result in other frame from helm git grep."
  (helm-git-grep-action candidate 'other-frame))

(defun helm-git-grep-jump-elscreen (candidate)
  "Jump to result in elscreen from helm git grep."
  (require 'elscreen)
  (if (elscreen-get-conf-list 'screen-history)
      (helm-git-grep-action candidate 'elscreen)
    (error "elscreen is not running")))

(defvar helm-git-grep-actions
  (delq
   nil
   `(("Find File" . helm-git-grep-action)
     ("Find file other frame" . helm-git-grep-other-frame)
     ,(and (locate-library "elscreen")
           '("Find file in Elscreen"
             . helm-git-grep-jump-elscreen))
     ;; ("Save results in grep buffer" . helm-git-grep-save-results) TODO
     ("Find file other window" . helm-git-grep-other-window))))

(define-helm-type-attribute 'git-grep
  `((default-directory . nil)
    (candidate-number-limit . 300)
    (requires-pattern . 3)
    (volatile)
    (delayed)
    (filtered-candidate-transformer helm-filtered-candidate-transformer-file-line)
    (multiline)
    (action . ,helm-git-grep-actions)))

(defvar helm-source-git-grep
  '((name . "Git Grep")
    (init . (lambda () (helm-attrset
                        'default-directory
                        (helm-git-grep-find-git-root))))
    (candidates-process . helm-git-grep-process)
    (type . git-grep)))

(defvar helm-source-git-submodule-grep
  '((name . "Git Submodule Grep")
    (init . (lambda () (helm-attrset
                        'default-directory
                        (helm-git-grep-find-git-submodule-root))))
    (candidates-process . helm-git-submodule-grep-process)
    (type . git-grep)))

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

;;; helm-git-grep.el ends here
