;;; org-doc-custom.el --- customization options for `org-doc' -*- lexical-binding: t; -*-

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

(defgroup org-doc nil
  "Sync elisp file headers and Org mode docs"
  :group 'org)

(defcustom org-doc:export-charset 'utf-8
  "The charset allowed to represent various elements and objects.

This value mirrors `org-ascii-charset'.

Possible values are:
`ascii'    Only use plain ASCII characters
`latin1'   Include Latin-1 characters
`utf-8'    Use all UTF-8 characters"
  :tag "Export Charset"
  :group 'org-doc
  :type '(choice
          (const :tag "ASCII" ascii)
          (const :tag "Latin-1" latin1)
          (const :tag "UTF-8" utf-8)))

(defun org-doc:custom-options-plist ()
  "Return customization options as a property list."
  (list :ascii-charset org-doc:export-charset))

(provide 'org-doc-custom)
;;; org-doc-custom.el ends here
