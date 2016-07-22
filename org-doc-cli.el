;;; org-doc-cli.el --- command-line interface for `org-doc' -*- lexical-binding: t; -*-

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

(require 'subr-x)

(require 'org-doc)

(defconst org-doc::usage
  (format "org-doc %s
Update comment headers of elisp files using Org mode documents.

Usage:
    cask emacs --batch -l org-doc-cli -f org-doc -- [OPTION]... ORG-FILE ELISP-FILE

Flags:
    -h, --help       display this message
    -v, --version    display version information

Options:
    -s, --section [commentary]    specify which comment section to update
                                  [values: commentary, changelog, history]"
          org-doc:version))

(define-error 'org-doc::cli-argument-error
  "Invalid command-line argument" 'user-error)
(define-error 'org-doc::duplicate-argument
  "Duplicate option or flag" 'org-doc::cli-argument-error)
(define-error 'org-doc::invalid-option-value
  "Invalid option value" 'org-doc::cli-argument-error)
(define-error 'org-doc::unknown-argument
  "Unkown option or flag" 'org-doc::cli-argument-error)
(define-error 'org-doc::missing-value
  "Missing option value" 'org-doc::cli-argument-error)
(define-error 'org-doc::positional-arg-count-mismatch
  "Too few or too many positional arguments" 'org-doc::cli-argument-error)

(define-error 'org-doc::usage "")
(define-error 'org-doc::version "")

(defun org-doc::parse-args (args)
  "Parse command line arguments ARGS.

Result is a property list
    (:org ORG-FILE
     :elisp ELISP-FILE
     :section SECTION)."
  (setf args (-remove-item "--" args))
  (when (null args)
    (signal 'org-doc::usage nil))
  (let ((positional-args-left 2)
        org elisp section)
    (while args
      (let ((argi (pop args))
            value)
        (when (string-match "^\\(--[^=]+\\)=" argi)
          (setf value (substring argi (match-end 0))
                argi (match-string 1 argi))
          (when (string-blank-p value)
            (signal 'org-doc::missing-value
                    (format "options `%s' requires a value" argi))))
        (cond ((member argi '("-h" "--help"))
               (signal 'org-doc::usage nil))
              ((member argi '("-v" "--version"))
               (signal 'org-doc::version nil))
              ((member argi '("-s" "--section"))
               (when section
                 (signal 'org-doc::duplicate-argument
                         "option `--section' defined multiple times"))
               (unless (setf value (or value (pop args)))
                 (signal 'org-doc::missing-value
                         "option `--section' requires a value"))
               (unless (org-doc::valid-section-name? value)
                 (signal 'org-doc::invalid-option-value
                         (format "invalid value for option `--section': expected one of %s; `%s' given"
                                 org-doc::section-names value)))
               (setf section value))
              ((string-prefix-p "-" argi)
               (signal 'org-doc::unknown-argument
                       (format "unknown option or flag: `%s'" argi)))

              ;; treat rest values as positional arguments
              ((<= positional-args-left 0)
               (signal 'org-doc::positional-arg-count-mismatch
                       "extra positional arguments"))
              (t
               (if (= positional-args-left 2)
                   (setf org argi)
                 (setf elisp argi))
               (setf positional-args-left (1- positional-args-left))))))
    (when (> positional-args-left 0)
      (signal 'org-doc::positional-arg-count-mismatch
              (format "%s positional argument(s) missing (%s)"
                      positional-args-left
                      (if (= positional-args-left 2)
                          "`ORG-FILE' and `ELISP-FILE'"
                        "`ELISP-FILE"))))
    (list :org org
          :elisp elisp
          :section (or section "commentary"))))

(defun org-doc::usage (&optional exit-code)
  "Display `org-doc' usage information and exit.

EXIT-CODE is an integer used as the exit status (defaults to 0)."
  (message "%s" org-doc::usage)
  (kill-emacs (or exit-code 0)))

(defun org-doc ()
  "Parse command line arguments and update elisp comment headers accordingly."
  (unwind-protect
      (condition-case error
          (let* ((args (org-doc::parse-args argv))
                 (export-result (org-doc:update-file-header (plist-get args :section)
                                                            (plist-get args :org)
                                                            (plist-get args :elisp))))
            (message "%s" export-result))
        (org-doc::cli-argument-error
         (message "cli-error: %s." (cdr error))
         (org-doc::usage 1))
        (org-doc::usage
         (org-doc::usage))
        (org-doc::version
         (message "org-doc %s" org-doc:version))
        (error
         (message "%s" (string-join (mapcar (lambda (elt)
                                              (format "%s" elt))
                                            error)
                                    ": "))))
    (kill-emacs)))

(provide 'org-doc-cli)
;;; org-doc-cli.el ends here
