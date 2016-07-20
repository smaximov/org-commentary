;;; org-doc-headers.el --- functions to manipulate comment headers of elisp files -*- lexical-binding: t; -*-

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

;; See the commentary section in `org-doc.el'.

;;; Code:

(require 'dash)
(require 'rx)

(require 'org-doc-util)

(defun org-doc::section-headline-regexp (section-regexp)
  "Create the regexp to search for a section's headline.

SECTION-REGEXP is a regexp matching section's name."
  (rx-to-string `(seq line-start
                      ";;;"
                      (zero-or-more blank)
                      (regexp ,section-regexp)
                      (zero-or-more blank)
                      ":"
                      (zero-or-more blank)
                      line-end)))

(defconst org-doc::section-alist
  '((commentary (("Commentary") . ("Change Log" "History" "Code")))
    (changelog (("Change Log" "History") . ("Commentary" "Code"))))
  "Section symbol -> (starting-headlines . terminating-headlines).")

(defconst org-doc::section-regexp-alist
  (-map (-lambda ((section (starting-headlines . terminating-headlines)))
          (cons section
                (cons (org-doc::section-headline-regexp (rx-to-string `(or ,@starting-headlines)))
                      (org-doc::section-headline-regexp (rx-to-string `(or ,@terminating-headlines))))))
        org-doc::section-alist)
  "Section symbol -> (headline-regexp . terminating-regexp).")

(defconst org-doc::sections '(commentary changelog)
  "Valid section symbols.")

(defconst org-doc::section-names '("commentary" "changelog" "history")
  "Valid section names.")

(defconst org-doc::section-name-alist
  '(("commentary" . commentary)
    ("changelog" . changelog)
    ("history" . changelog))
  "Association list mapping `org-doc::section-names' to `org-doc::sections'.")

(defun org-doc::valid-section-name? (section-name)
  "Return nil if SECTION-NAME is not a member of `org-doc::section-names'."
  (member section-name org-doc::section-names))

(defun org-doc::section-symbol (section-name)
  "Return the symbol corresponging to SECTION-NAME."
  (cdr (assoc section-name org-doc::section-name-alist)))

(defun org-doc::validate-section! (section)
  "Check if SECTION is one of '(changelog commentary)', signal an error otherwise."
  (unless (memq section org-doc::sections)
    (error "Unkown section `%s'.  Valid sections are `changelog' and `commentary'" section)))

(defun org-doc::headline-regexp (section)
  "Return headline regexp for SECTION."
  (org-doc::validate-section! section)
  (cadr (assoc section org-doc::section-regexp-alist)))

(defun org-doc::terminate-regexp (section)
  "Return terminating regexp for SECTION."
  (org-doc::validate-section! section)
  (cddr (assoc section org-doc::section-regexp-alist)))

(defun org-doc::section-content-start (section &optional start)
  "Return the position at the start of SECTION content.

START is the buffer position where the search is started.
If START is nil, it defaults to (point-min)."
  (let ((case-fold-search nil)
        (headline-regexp (org-doc::headline-regexp section)))
    (save-excursion
      (save-match-data
        (goto-char (or start (point-min)))
        (or (re-search-forward headline-regexp nil t)
            (error "Section `%s' is not found" section))))))

(defun org-doc::section-content-end (section start)
  "Return the position at the end of SECTION content.

START is the buffer position where the search is started.
The value START should be obtained by invoking
`org-doc::section-content-start'."
  (let ((case-fold-search nil)
        (headline-regexp (org-doc::headline-regexp section))
        (terminate-regexp (org-doc::terminate-regexp section)))
    (save-excursion
      (save-match-data
        (goto-char start)
        (when (re-search-forward headline-regexp nil t)
          (error "Section `%s' has duplicate headlines" section))
        (unless (re-search-forward terminate-regexp nil t)
          (error "Section `%s' is unterminated" section))
        (match-beginning 0)))))

(defun org-doc::update-comment-header (section content)
  "Replace elips file header section denoted by SECTION with CONTENT.

CONTENT is commented out before inserting."
  (save-excursion
   (let* ((start (org-doc::section-content-start section))
          (end (org-doc::section-content-end section start)))
     (kill-region start end)
     (goto-char start)
     (insert "\n\n" (org-doc::comment-string content) "\n\n"))))

(provide 'org-doc-headers)
;;; org-doc-headers.el ends here
