;;; test-headers.el --- test functions for manipulation of elisp comment headers  -*- lexical-binding: t; -*-

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

(require 'setup-tests)

(describe "Match sections"
  (it "should match valid section headlines"
    (-each org-commentary--section-alist
      (-lambda ((section (starting-headlines . _)))
        (ignore _)
        (--each starting-headlines
          (expect (format "Preceding text\n;;;%s:  \nSection content" it)
                  :to-match (org-commentary--headline-regexp section))))))

  (it "should detect valid sections"
    (with-temp-buffer
      (let ((buffer-content "

;; Preceding text.

;;; Commentary:

;; Commentary section content.

;; It spans multiple lines.

;;; History:

;; Next section content.

")
            (section-content "

;; Commentary section content.

;; It spans multiple lines.

")
            (start nil)
            (end nil))
        (expect (lambda ()
                  (insert buffer-content)
                  (setf start (org-commentary--section-content-start 'commentary)
                        end (org-commentary--section-content-end 'commentary start)))
                :not :to-throw 'error)

        (expect (buffer-substring start end)
                :to-equal section-content))))

  (it "should signal errors on (unterminated|uncorrectly terminated) sections"
    (let ((buffer-content-duplicate-sections "

;; Preceding text.

;;; Change Log:

;; Change log content.

;;; History:

;; History is an alias to Change Log, so this's a no-no!

")

          (buffer-content-unterminated-section "

;; Preceding text.

;;; History:

;; Change log contents.

;; Code section starts without the ';;; Code:' headline:

    (defun loop () (loop))

       "))
      (--each (list buffer-content-duplicate-sections
                    buffer-content-unterminated-section)
        (expect (lambda ()
                  (with-temp-buffer
                    (insert it)
                    (org-commentary--section-content-end 'changelog
                                                         (org-commentary--section-content-start
                                                          'changelog))))
                :to-throw 'error)))))

(describe "Update headers"
  (it "should update buffer headers"
    (let ((buffer-content-before "

;; Preceding text.

;;; Commentary:

;; Commentary content before.

;;; Code:

")
          (replacement "Commentary content after.
It splits multiple lines.

It splits multiple paragraphs.")
          (buffer-content-after "

;; Preceding text.

;;; Commentary:

;; Commentary content after.
;; It splits multiple lines.

;; It splits multiple paragraphs.

;;; Code:

"))
      (with-temp-buffer
        (insert buffer-content-before)
        (org-commentary--update-comment-header 'commentary replacement)
        (expect (buffer-string)
                :to-equal buffer-content-after)))))

(provide 'test-headers)
;;; test-headers.el ends here
