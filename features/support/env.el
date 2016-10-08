;;; env.el --- Environment use with ecukes for helm-git-grep

;; Copyright (C) 2016 Yasuyuki Oka

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

;;; Code:


(require 'f)

(defvar helm-git-grep-support-path
  (f-dirname load-file-name))

(defvar helm-git-grep-features-path
  (f-parent helm-git-grep-support-path))

(defvar helm-git-grep-root-path
  (f-parent helm-git-grep-features-path))

(defvar helm-git-grep-fixture-path
  (f-join helm-git-grep-root-path "test/fixture/git"))

(add-to-list 'load-path helm-git-grep-root-path)

(require 'helm-git-grep)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
 (define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm)
 (setq default-directory helm-git-grep-fixture-path)
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )

(provide 'env)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; env.el ends here
