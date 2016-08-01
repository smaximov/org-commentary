;;; org-commentary-util.el --- supporting functions for org-commentary -*- lexical-binding: t -*-

;; Copyright (C) 2016 Sergei Maximov

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

;; See the commentary section in `org-commentary.el'.

;;; Code:

(require 'rx)
(require 'subr-x)

(defun org-commentary--comment-string (string)
  "Comment out each non-blank line of the STRING using Lisp-style comments."
  (if (string-blank-p string)
      string
    (with-temp-buffer
      ;; enforce Lisp-style comments
      (let ((comment-style 'plain)
            (comment-start ";")
            (comment-end "")
            (comment-padding " ")
            ;; number of comment characters to insert by `comment-region';
            ;; this value is doubled if `comment-style' is `plain';
            ;; so `comment-add' * 2 (two) characters will be inserted:
            (comment-add 1)
            (comment-empty-lines nil)
            (comment-quote-nested nil))
        (insert string)
        (comment-region (point-min) (point-max))
        (buffer-string)))))

(defconst org-commentary--drawer-regexp (rx (one-or-more (or word (char ?- ?_))))
  "Regexp to match a valid drawer name.")
(defconst org-commentary--drawer-keyword-regexp
  (rx-to-string `(seq line-start
                      "#+DRAWERS:"
                      (one-or-more blank)
                      (group (zero-or-more (regexp ,org-commentary--drawer-regexp)
                                           (zero-or-more blank)))
                      line-end))
  "Regexp to match `#+DRAWERS: DRAWERS-VALUES' line.

DRAWERS-VALUES is saved as the first match group upon successful match.")

(defun org-commentary--parse-custom-drawers ()
  "Return the list of custom drawers defined with the `#+DRAWERS' keyword.

Return nil if buffer doesn't define custom drawers."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let ((drawers nil)
            ;; keywords don't depend on case:
            (case-fold-search t)
            (word-separator-regexp (rx (one-or-more blank))))
        (while (re-search-forward org-commentary--drawer-keyword-regexp nil t)
          (setf drawers (append (split-string (match-string 1)
                                              word-separator-regexp t
                                              word-separator-regexp)
                                drawers)))
        drawers))))

(provide 'org-commentary-util)
;;; org-commentary-util.el ends here
