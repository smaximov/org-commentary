;;; test-export.el --- test Org mode exporting -*- lexical-binding: t -*-

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

(describe "Export"
  (let ((buffer-content "

#+DRAWERS: HIDDEN
#+DRAWERS: NOEXPORT
#+OPTIONS: d:(not \"HIDDEN\")

* Headline 1

:NOEXPORT:
I will be exported despite my name!
:END:

** Headline 1.1

:HIDDEN:
I will be ignored during the export!
:END:

** Headline 1.2            :noexport:

^^^^
||||
This subtree won't be exported!

")
        (export-result nil))
    (it "should trim leading and trailing whitespace"
      (with-temp-buffer
        (insert buffer-content)
        (setf export-result (org-doc::export-buffer-as-string))
        (expect (string-match-p "^[^[:space:]]" export-result)
                :to-be 0)
        (expect (string-match-p "^[^[:space:]]"
                                (if (version< emacs-version "25")
                                    (string-reverse export-result)
                                  ;; `string-reverse' is an alias for `reverse'
                                  ;; in Emacs 25 and is obsolete since 25.1.
                                  (reverse export-result)))
                :to-be 0)))

    (it "should generate the table of contents by default"
      (with-temp-buffer
          (insert buffer-content)
          (setf export-result (org-doc::export-buffer-as-string))
          (expect export-result :to-match "Table of Contents")))

    (describe "Export mode"
      (it "should export an Org document in the `utf8' mode by default"
        (with-temp-buffer
          (insert buffer-content)
          (setf export-result (org-doc::export-buffer-as-string))
          (expect export-result :to-match
                  (rx-to-string `(char ,@ (cdr (assoc 'utf-8 org-ascii-underline)))))))

      (it "should be possible to change the export mode using `org-doc:export-charset'"
        (with-temp-buffer
          (insert buffer-content)
          (let ((org-doc:export-charset 'latin1))
            (setf export-result (org-doc::export-buffer-as-string)))
          (expect export-result :to-match
                  (rx-to-string `(char ,@ (cdr (assoc 'latin1 org-ascii-underline))))))))

    (describe "Excluding"
      (it "should exclude the content of specific drawers"
        (with-temp-buffer
          (insert buffer-content)
          (setf export-result (org-doc::export-buffer-as-string))
          (expect export-result :to-match "I will be exported despite my name!")
          (expect export-result :not :to-match "I will be ignored during the export!")))

      (it "should exclude the entries tagged by a specific tag"
        ;; FIXME default tag
        (with-temp-buffer
          (insert buffer-content)
          (setf export-result (org-doc::export-buffer-as-string))
          (expect export-result :not :to-match "This subtree won't be exported!")))

      (it "should be possible to exclude the table of contents from exporting"
        (with-temp-buffer
          (insert "#+OPTIONS: toc:nil\n* Headline\n** Headline")
          (setf export-result (org-doc::export-buffer-as-string))
          (expect export-result :not :to-match "Table of Contents"))))))

(provide 'test-export)
;;; test-export.el ends here
