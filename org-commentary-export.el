;;; org-commentary-export.el --- custom Org mode export backend and helper functions -*- lexical-binding: t; -*-

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

;; This file defines a custom Org mode export backend (derived from `ascii').
;; The main reason to introduce a new backend was to get rid of extra elements
;; present in the `ascii' backend I find superfuous for comment headers of elisp files
;; (e.g., hardcoded document title block in `org-ascii-template').

;; `org-commentary' emphasizes the control of which parts of a Org document you want to export.
;; It uses drawers and tags to define what needs to be excluded from the resulting document.
;; While tags exclusion worked fine in the existing Org mode code, exporting of drawer
;; was quite broken in Org mode versions prior to 8.3. Org mode recognized only few built-in
;; drawers (PROPERTIES, CLOCK, LOGBOOK, RESULTS).  Custom drawers needed to be registered
;; globally in `org-drawers' or on per-file basis using the `#+DRAWERS' keyword.
;; Note: `org-drawers' was deprecated in 8.3 then removed.

;; But the problem in earlier versions of Org mode was that the support for custom drawers
;; defined with the `#+DRAWERS' keyword didn't extend to exporting.  Org mode just
;; didn't parse custom drawers defined in an Org file.  So to work around this issue, `org-commentary'
;; parses `#+DRAWERS' manually to find out which custom drawers are defined in an Org file
;; and augments the `org-drawers' variable with those custom drawers before starting export.

;;; Code:

(require 'ox)

(require 'org-commentary-util)

(defun org-commentary--template (contents info)
  "Return complete document string after conversion.

CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.

This functions corresponds to `org-ascii-template' of the `ascii'
backend."
  (concat
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (concat
        ;; FIXME: org-ascii--build-toc is "private".
        ;; FIXME: should we write `org-commentary--build-toc'?
        (org-ascii--build-toc info (and (wholenump depth) depth))
        "\n\n\n")))
   contents))

(org-export-define-derived-backend 'org-commentary--ascii 'ascii
  :translate-alist '((template . org-commentary--template)))

;; Silence Emacs complaining about the undefined `org-drawers' variable.
(defvar org-drawers)

(defun org-commentary--buffer-drawers ()
  "Return the list of Org mode drawers which the current buffer is heard of.

This function is provided for compatibility with Org mode versions prior to 8.3.
On later versions the return value is always nil"
  (when (version< org-version "8.3")
    (append org-drawers (org-commentary--parse-custom-drawers))))

(defun org-commentary-export-buffer-as-string (&optional ext-plist)
  "Export the Org document opened in the current buffer as a string.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

The result is stripped from leading and trailing whitespace."
  (let ((org-drawers (org-commentary--buffer-drawers)))
    (string-trim (org-export-as 'org-commentary--ascii nil nil nil ext-plist))))

(defun org-commentary-export-file-as-string (file &optional ext-plist)
  "Export the Org file FILE as a string.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

The result is stripped from leading and trailing whitespace."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (org-commentary-export-buffer-as-string ext-plist)))

(provide 'org-commentary-export)
;;; org-commentary-export.el ends here
