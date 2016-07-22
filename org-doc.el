;;; org-doc.el --- convert Org mode content to elisp comment header -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sergei Maximov

;; Author: Sergei Maximov <s.b.maximov@gmail.com>
;; Created: 20 Jul 2016
;; Version: 0.1.0
;; Package-Requires: ((dash "2.0") (emacs "24.4") (org "8.0"))
;; Keywords: convenience, docs, tools

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

;; Table of Contents
;; ─────────────────

;; 1 org-doc
;; .. 1.1 Installation
;; .. 1.2 Usage
;; ..... 1.2.1 Inside Emacs
;; ..... 1.2.2 From the command line
;; .. 1.3 Similar projects


;; 1 org-doc
;; ═════════

;;   `org-doc' — generate conventional [library headers] from Org mode
;;   files.


;;   [library headers]
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html


;; 1.1 Installation
;; ────────────────

;;   FIXME: to be written.


;; 1.2 Usage
;; ─────────

;;   tl;dr (assuming you're using [Cask] for project management)

;;   1. Add `org-doc' to the development dependencies of your library:

;;      ┌────
;;      │ (development
;;      │  (depends-on "org-doc"))
;;      └────

;;      Fetch dependencies:

;;      ┌────
;;      │ $ cask install
;;      └────

;;   2. Put the [library header] boilerplate in your ELisp file.

;;   3. Generate /Commentary/ section of the library headers:

;;      ┌────
;;      │ $ cask emacs --batch -l org-doc-cli -f org-doc -- \
;;      │     README.org your-package.el
;;      └────

;;   4. [Optional] Generate /Change Log/ section of the library headers:

;;      ┌────
;;      │ $ cask emacs --batch -l org-doc-cli -f org-doc -- \
;;      │     --section changelog CHANGELOG.org your-package.el
;;      └────

;;   5. Commit.

;;   Run `cask emacs --batch -l org-doc-cli -f org-doc -- --help' to see
;;   available options.


;;   [Cask] https://github.com/cask/cask

;;   [library header]
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html


;; 1.2.1 Inside Emacs
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   First you need to load `org-doc':

;;   ┌────
;;   │ (require 'org-doc)
;;   └────

;;   Then you can interactively call `org-doc:update-file-header'.


;; 1.2.2 From the command line
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   FIXME: to be written.


;; 1.3 Similar projects
;; ────────────────────

;;   • [org2elcomment] (available on MELPA).


;;   [org2elcomment] https://github.com/cute-jumper/org2elcomment

;;; Code:

(require 'org-doc-custom)
(require 'org-doc-export)
(require 'org-doc-headers)
(require 'org-doc-util)

(defconst org-doc:version "0.1.0")

(defun org-doc:update-file-header (section-name org elisp)
  "Update the file header using the content of an Org document.

SECTION-NAME is a string denoting which section of the header to update.
Valid values are defined in `org-doc::section-names'.
ORG is a name of Org document which contents will be exported.
ELISP is a name of the Emacs Lisp file which comment header will be updated.

Function returns the converted content of the ORG file."
  (interactive
   (list (completing-read "Section [commentary, changelog, or history]: "
                          org-doc::section-names)
         (read-file-name "Org document: " nil nil 'confirm)
         (read-file-name "ELisp file: " nil nil 'confirm)))

  (unless (org-doc::valid-section-name? section-name)
    (error "Invalid section name: `%s'" section-name))

  (let* ((export-result
          (org-doc:export-file-as-string org))

         ;; the buffer associated with the `elisp' file;
         ;; nil if no buffers visit that file:
         (elisp-buffer-visited? (get-file-buffer elisp))

         ;; create a new buffer if necessary
         (elisp-buffer (or elisp-buffer-visited?
                           (find-file-noselect (expand-file-name elisp)))))

    (unwind-protect
        (with-current-buffer elisp-buffer
          (org-doc::update-comment-header (org-doc::section-symbol section-name)
                                          export-result)
          (basic-save-buffer (called-interactively-p))
          export-result)

      ;; kill the buffer associated with the `elisp' file
      ;; if we created it manually.
      (unless elisp-buffer-visited?
        (kill-buffer elisp-buffer)))))

(provide 'org-doc)
;;; org-doc.el ends here
