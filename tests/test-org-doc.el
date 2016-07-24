;;; test-org-doc.el --- test public function and commands -*- lexical-binding: t -*-

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

(describe "Public API"
  (describe "Update"
    :var (org-file elisp-file org-contents elisp-before elisp-after)

    (before-each
      (setf org-file (make-temp-file "org-doc-"))
      (setf elisp-file (make-temp-file "org-doc-")))

    (after-each
      (delete-file org-file)
      (delete-file elisp-file))

    (it "should update a file with valid library headers"
      (setf org-contents "* Top-level heading

Top-level content.

** Second-level heading

Second-level content."
            elisp-before ";;; temp-file.el --- temporary elisp file

;; Copyright (C) 2016 Sergei Maximov

;; Some more comments.

;;; Change Log:

;;; Commentary:

;;; Code:

;;; temp-file.el ends here"
            elisp-after ";;; temp-file.el --- temporary elisp file

;; Copyright (C) 2016 Sergei Maximov

;; Some more comments.

;;; Change Log:

;; 1 Top-level heading
;; ===================

;;   Top-level content.


;; 1.1 Second-level heading
;; ~~~~~~~~~~~~~~~~~~~~~~~~

;;   Second-level content.

;;; Commentary:

;;; Code:

;;; temp-file.el ends here")

      (with-temp-file org-file (insert org-contents))
      (with-temp-file elisp-file (insert elisp-before))

      (org-doc:update "history" org-file elisp-file '(:with-toc nil))
      (expect (with-temp-buffer
                (insert-file-contents elisp-file)
                (buffer-string))
              :to-equal elisp-after))

    (it "should raise an error for a file with malformed library headers or missing sections"
      (setf org-contents "* Top-level heading

Top-level content.

** Second-level heading

Second-level content."
            elisp-before ";;; temp-file.el --- temporary elisp file

;; Copyright (C) 2016 Sergei Maximov

;; Some more comments.

;;; Change Log:

;;; Code:

;;; temp-file.el ends here")

      (with-temp-file org-file (insert org-contents))
      (with-temp-file elisp-file (insert elisp-before))

      (expect (lambda () (org-export:update org-file elisp-file))
              :to-throw 'error))))

(provide 'test-org-doc)
;;; test-org-doc.el ends here
