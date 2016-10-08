;;; helm-git-grep-steps.el --- Steps used with ecukes for helm-git-grep

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

(Given "^I open file \"\\(.+\\)\"$" "Open given filename"
       (lambda (filename)
         (find-file filename)))

(Then "^I should be in buffer matching regexp \"\\(.+\\)\"$" "Match REGEXP against buffer-name"
      (lambda (expected)
        (let ((message "Expected to be in buffer '%s', but was in '%s'"))
          (string-match expected (buffer-name)))))

(Then "^the cursor should be at line \"\\(.+\\)\"$"
      "Checks that the cursor is at a specific line."
      (lambda (line)
        (let ((message "Expected cursor to be at line'%s', but was at '%s'"))
          (= (line-number-at-pos (point)) (string-to-number line)))))

(provide 'helm-git-grep-steps)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; helm-git-grep-steps.el ends here
