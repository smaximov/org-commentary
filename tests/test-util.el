;;; test-util.el --- test supporting functions for org-doc -*- lexical-binding: t -*-

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

;;; Code:

(require 'setup-tests)

(describe "Supporting functions"
  (describe "Comment string"
    (it "should comment out every non-blank line of a multiline string"
      (expect (org-doc--comment-string "\n\nSome text.\nThe other line.\n\n")
              :to-equal "\n\n;; Some text.\n;; The other line.\n\n"))

    (it "should leave empty/blank strings intact"
      (--each '("" " " "\t" "\n" " \n \n ")
        (expect (org-doc--comment-string it)
                :to-equal it))))

  (describe "Parse custom drawers"
    (it "should return nil if the buffer doesn't contain the `#DRAWERS' keyword"
      (with-temp-buffer
        (expect (org-doc--parse-custom-drawers) :to-equal nil)))

    (it "should return nil if the buffer has the `#DRAWERS' keyword with no value"
      (with-temp-buffer
        (insert "#+DRAWERS:\n\nSome text.")
        (expect (org-doc--parse-custom-drawers) :to-equal nil)))

    (it "should return the list of custom drawers if the buffer contains single `#DRAWERS' keyword"
      (with-temp-buffer
        (insert "#+DRAWERS: CUSTOM DRAWER\n\nSome text.")
        (expect (org-doc--parse-custom-drawers) :to-equal '("CUSTOM" "DRAWER"))))

    (it "should return the list of all drawers if the buffer contains multiple `#DRAWERS' keywords"
      (with-temp-buffer
        (insert "
#+DRAWERS: CUSTOM DRAWER
#+SOME_OTHER_KEYWORD: VALUE
#+DRAWERS:
#+DRAWERS: MORE DRAWERS

Some text.")
        (expect (org-doc--parse-custom-drawers)
                :to-equal '("MORE" "DRAWERS" "CUSTOM" "DRAWER"))))))

(provide 'test-util)
;;; test-util.el ends here
