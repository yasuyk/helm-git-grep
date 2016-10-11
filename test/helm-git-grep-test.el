;;; helm-git-grep-test.el --- helm-git-grep: unit test suite

;; Copyright (C) 2016 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; URL: https://github.com/yasuyk/helm-git-grep

;; This file is not part of GNU Emacs.

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

;; Unit test suite of helm-git-grep

;;; Code:


(eval-when-compile (require 'cl))

(require 'ert)
(require 'mocker)
(require 'f)

(defvar test/test-path
  (if load-file-name
      (f-dirname load-file-name)
    (f-expand default-directory)))

(defvar test/root-path
  (f-parent test/test-path))

(defvar test/fixture-path
  (f-join test/test-path "fixture/git"))

(defun should-equal? (a b)
    (should (equal a b)))
(defun should-not-equal? (a b)
    (should-not (equal a b)))

(ert-deftest test/helm-git-grep-base-directory ()
  (let ((expected "/tmp/git"))
    (let ((helm-git-grep-base-directory 'root))
      (mocker-let ((helm-git-grep-get-top-dir () ((:output expected))))
        (should-equal? (helm-git-grep-base-directory) expected)))
    (let ((helm-git-grep-base-directory 'current)
          (default-directory expected))
      (should-equal? (helm-git-grep-base-directory) expected))))

(ert-deftest test/helm-git-grep-get-top-dir ()
  (let ((default-directory (concat test/fixture-path "/Documentation/howto")))
    (f-equal? (helm-git-grep-get-top-dir) (concat test/fixture-path)))
  (mocker-let ((helm-git-grep-git-string
                (a b) ((:input '("rev-parse" "--show-cdup") :output nil))))
    (should-equal? (helm-git-grep-get-top-dir) nil)))

(ert-deftest test/helm-git-grep-showing-leading-and-trailing-lines-option ()
  (let ((helm-git-grep-showing-leading-and-trailing-lines t))
    (should-equal? (helm-git-grep-showing-leading-and-trailing-lines-option) "-1"))
  (let ((helm-git-grep-showing-leading-and-trailing-lines t)
        (helm-git-grep-showing-leading-and-trailing-lines-number 2))
    (should-equal? (helm-git-grep-showing-leading-and-trailing-lines-option) "-2"))
  (should-equal? (helm-git-grep-showing-leading-and-trailing-lines-option) nil)
  (should-equal? (helm-git-grep-showing-leading-and-trailing-lines-option t) ""))

(ert-deftest test/helm-git-grep-args ()
  (let ((helm-git-grep-ignore-case t)
        (helm-pattern "defun helm git")
        (helm-git-grep-pathspecs nil))
    (should-equal? (helm-git-grep-args)
                  '("--no-pager" "grep" "-n" "--no-color" "-i"
                    "-e" "defun" "--and" "-e" "helm" "--and" "-e" "git")))
  (let ((helm-git-grep-ignore-case nil)
        (helm-pattern "helm")
        (helm-git-grep-pathspecs nil))
     (should-equal? (helm-git-grep-args)
                   '("--no-pager" "grep" "-n" "--no-color" "-e" "helm")))
  (let ((helm-git-grep-ignore-case nil)
        (helm-pattern "helm")
        (helm-git-grep-pathspecs '("./*" ":!test/**")))
     (should-equal? (helm-git-grep-args)
                   '("--no-pager" "grep" "-n" "--no-color" "-e" "helm"
                     "--" "./*" ":!test/**")))
  (let ((helm-git-grep-ignore-case nil)
        (helm-pattern "helm")
        (helm-git-grep-pathspecs '("./*" ":!test/**"))
        (helm-git-grep-pathspec-available nil))
     (should-equal? (helm-git-grep-args)
                   '("--no-pager" "grep" "-n" "--no-color" "-e" "helm"))))

(ert-deftest test/helm-git-grep-highlight-match ()
  (let* ((helm-input "defun")
         (result (helm-git-grep-highlight-match "(defun abc())")))
    (should-equal? (get-text-property 0 'face result) nil)
    (cl-loop for x from 1 to (length helm-input)
             do (should-equal? (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from (1+ (length helm-input)) to  (length result)
             do (should-equal? (get-text-property x 'face result) nil)))
  (let* ((helm-input "begin match put")
         (result (helm-git-grep-highlight-match "(put-text-property (match-beginning 1) (match-end 1)")))
    (should-equal? (get-text-property 0 'face result) nil)
    (cl-loop for x from 1 to (length "put")
             do (should-equal? (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from 4 to (length "-text-property (")
             do (should-equal? (get-text-property x 'face result) nil))
    (cl-loop for x from 20 to (length "match")
             do (should-equal? (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from 25 to (length "-beginning 1) (")
             do (should-equal? (get-text-property x 'face result) nil))
    (cl-loop for x from 40 to (length "match")
             do (should-equal? (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from 40 to (length "-end 1)")
             do (should-equal? (get-text-property x 'face result) 'helm-git-grep-match))))

(ert-deftest test/helm-helm-git-grep-init()
  (let ((expected "/tmp/git"))
    (mocker-let ((helm-git-grep-base-directory () ((:output expected)))
                 (helm-attrset (n v) ((:input `(base-directory ,expected)))))
      (helm-git-grep-init))))

(ert-deftest test/helm-git-grep-persistent-action ()
  (let ((expected "helm"))
    (let ((current-prefix-arg t))
      (mocker-let ((helm-git-grep-action (candidate where mark) ((:input `(,expected nil mark))))
                   (helm-highlight-current-line () ((:output t))))
        (should (helm-git-grep-persistent-action expected))))
    (let ((current-prefix-arg nil))
      (mocker-let ((helm-git-grep-action (candidate) ((:input `(,expected))))
                   (helm-highlight-current-line () ((:output t))))
        (should (helm-git-grep-persistent-action expected))))))

(ert-deftest test/helm-git-grep-get-input-symbol ()
  (let ((expected " helm"))
    (with-temp-buffer
      (insert expected)
      (goto-char (1+ (point-min)))
      (should-equal? (helm-git-grep-get-input-symbol) "helm"))
    (let ((mark-active t))
      (mocker-let ((use-region-p () ((:output t)))
                   (helm-git-grep-get-region-substring () ((:output expected))))
        (should-equal? (helm-git-grep-get-input-symbol) expected)))))

(ert-deftest test/helm-git-grep-get-isearch-input-symbol ()
  ;; return isearch-string
  (let* ((expected "defun")
         (isearch-regexp expected)
         (isearch-string expected))
    (should-equal? (helm-git-grep-get-isearch-input-symbol) expected))
  (let* ((expected "\\^defun")
         (isearch-regexp nil)
         (isearch-string "^defun"))
    (should-equal? (helm-git-grep-get-isearch-input-symbol) expected)))

(ert-deftest test/helm-git-grep-rerun-with-input ()
  (mocker-let ((helm-run-after-exit (f) ((:input-matcher 'functionp :output t))))
    (should (helm-git-grep-rerun-with-input))))

(ert-deftest test/helm-git-grep-header-name ()
  (let ((helm-git-grep-doc-order-in-name-header
         '(pathspec basedir ignorecase))
        (helm-git-grep-pathspecs '("./*"))
        (helm-git-grep-ignore-case nil)
        (helm-git-grep-base-directory 'root))
    (should-equal? (helm-git-grep-header-name "Git Grep")
                  "Git Grep (C-c p: pathspec) (C-c b: base dir[root]) (C-c i: ignore case)"))
  (let ((helm-git-grep-doc-order-in-name-header
         '(ignorecase pathspec basedir))
        (helm-git-grep-ignore-case t)
        (helm-git-grep-base-directory 'current))
    (should-equal? (helm-git-grep-header-name "Git Grep")
                  "Git Grep (C-c i: ignore case[i]) (C-c b: base dir[current])")))



(ert-deftest test/helm-git-grep-toggle-ignore-case ()
  (let ((helm-git-grep-ignore-case t))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:output t))))
        (should (helm-git-grep-toggle-ignore-case))
        (should-equal? helm-git-grep-ignore-case nil)))
  (let ((helm-git-grep-ignore-case nil))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:output t))))
        (should (helm-git-grep-toggle-ignore-case))
        (should-equal? helm-git-grep-ignore-case t))))

(ert-deftest test/helm-git-grep-toggle-showing-trailing-leading-line ()
  (let ((helm-git-grep-showing-leading-and-trailing-lines t))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:output t))))
        (should (helm-git-grep-toggle-showing-trailing-leading-line))
        (should-equal? helm-git-grep-showing-leading-and-trailing-lines nil)))
  (let ((helm-git-grep-showing-leading-and-trailing-lines nil))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:output t))))
        (should (helm-git-grep-toggle-showing-trailing-leading-line))
        (should-equal? helm-git-grep-showing-leading-and-trailing-lines t))))

(ert-deftest test/helm-git-grep-toggle-base-directory ()
  (let ((helm-git-grep-base-directory 'root))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:output t))))
        (should (helm-git-grep-toggle-base-directory))
        (should-equal? helm-git-grep-base-directory 'current)))
  (let ((helm-git-grep-base-directory 'current))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:output t))))
        (should (helm-git-grep-toggle-base-directory))
        (should-equal? helm-git-grep-base-directory 'root))))

(ert-deftest test/helm-git-grep-pathspec-toggle-availability ()
  (let ((helm-git-grep-pathspecs nil))
    (mocker-let ((message (m) ((:input `(,helm-git-grep-pathspec-disabled-message)))))
      (helm-git-grep-pathspec-toggle-availability)))
  (let ((helm-git-grep-pathspecs '("./*" ":!test/**")))
    (let ((helm-git-grep-pathspec-available nil))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:max-occur 1))))
        (helm-git-grep-pathspec-toggle-availability)
        (should-equal? helm-git-grep-pathspec-available t)))
    (let ((helm-git-grep-pathspec-available t))
      (mocker-let ((helm-git-grep-rerun-with-input () ((:max-occur 1))))
        (helm-git-grep-pathspec-toggle-availability)
        (should-equal? helm-git-grep-pathspec-available nil)))))

(ert-deftest test/helm-git-grep ()
  (mocker-let ((helm-git-grep-1 () ((:output t))))
    (should (helm-git-grep))))

(ert-deftest test/helm-git-grep-at-point-symbol-is-nil ()
  (mocker-let ((helm-git-grep-get-input-symbol () ((:output nil)))
               (helm-git-grep-1 (input) ((:input '("") :output t))))
    (should (helm-git-grep-at-point))))

(ert-deftest test/helm-git-grep-at-point-do-deactivate-mark ()
  (let ((helm-git-grep-at-point-deactivate-mark t)
        (mark-active t))
    (mocker-let ((helm-git-grep-get-input-symbol () ((:output "helm")))
                 (helm-git-grep-1 (input) ((:input '("helm ") :output t)))
                 (deactivate-mark () ((:max-occur 1))))
      (should (helm-git-grep-at-point)))))

(ert-deftest test/helm-git-grep-from-isearch ()
    (mocker-let ((helm-git-grep-get-isearch-input-symbol () ((:output "helm")))
                 (helm-git-grep-1 (input) ((:input '("helm") :output t)))
                 (isearch-exit () ((:max-occur 1))))
      (should (helm-git-grep-from-isearch))))

(ert-deftest test/helm-git-grep-from-helm ()
  (let ((helm-input "helm"))
    (mocker-let ((helm-exit-and-execute-action (action) ((:input-matcher 'functionp :output t))))
      (should (helm-git-grep-from-helm)))))

(provide 'helm-git-grep-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-git-grep-test.el ends here
