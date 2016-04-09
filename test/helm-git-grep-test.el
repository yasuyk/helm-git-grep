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
(require 'helm-git-grep)
(require 'ert)

(defun should-equal (a b)
    (should (equal a b)))
(defun should-not-equal (a b)
    (should-not (equal a b)))

(ert-deftest ert--helm-git-grep-showing-leading-and-trailing-lines-option ()
  (let ((helm-git-grep-showing-leading-and-trailing-lines t))
    (should-equal (helm-git-grep-showing-leading-and-trailing-lines-option) "-1"))
  (let ((helm-git-grep-showing-leading-and-trailing-lines t)
        (helm-git-grep-showing-leading-and-trailing-lines-number 2))
    (should-equal (helm-git-grep-showing-leading-and-trailing-lines-option) "-2"))
  (should-equal (helm-git-grep-showing-leading-and-trailing-lines-option) nil)
  (should-equal (helm-git-grep-showing-leading-and-trailing-lines-option t) ""))

(ert-deftest ert--helm-git-grep-rgs ()
  (should-equal (helm-git-grep-args nil)
                '("--no-pager" "grep" "--full-name" "-n" "--no-color" "-i"))
  (let ((helm-git-grep-ignore-case nil))
     (should-equal (helm-git-grep-args nil)
                   '("--no-pager" "grep" "--full-name" "-n" "--no-color"))))

(ert-deftest ert--helm-git-grep-highlight-match ()
  (let* ((helm-input "defun")
         (result (helm-git-grep-highlight-match "(defun abc())")))
    (should-equal (get-text-property 0 'face result) nil)
    (cl-loop for x from 1 to (length helm-input)
             do (should-equal (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from (1+ (length helm-input)) to  (length result)
             do (should-equal (get-text-property x 'face result) nil)))
  (let* ((helm-input "begin match put")
         (result (helm-git-grep-highlight-match "(put-text-property (match-beginning 1) (match-end 1)")))
    (should-equal (get-text-property 0 'face result) nil)
    (cl-loop for x from 1 to (length "put")
             do (should-equal (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from 4 to (length "-text-property (")
             do (should-equal (get-text-property x 'face result) nil))
    (cl-loop for x from 20 to (length "match")
             do (should-equal (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from 25 to (length "-beginning 1) (")
             do (should-equal (get-text-property x 'face result) nil))
    (cl-loop for x from 40 to (length "match")
             do (should-equal (get-text-property x 'face result) 'helm-git-grep-match))
    (cl-loop for x from 40 to (length "-end 1)")
             do (should-equal (get-text-property x 'face result) 'helm-git-grep-match))))


(provide 'helm-git-grep-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; helm-git-grep-test.el ends here
