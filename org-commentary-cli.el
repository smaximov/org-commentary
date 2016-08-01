;;; org-commentary-cli.el --- command-line interface for `org-commentary' -*- lexical-binding: t; -*-

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

(require 'dash)
(require 'org)
(require 'subr-x)

(require 'org-commentary)

(defconst org-commentary--cli-args-spec
  ;; ((NAME...) DEFAULT KEY OPTION-P VALIDATE HANDLER DATUM...)
  `((("-h" "--help") nil nil nil nil org-commentary--usage)
    (("-v" "--version") nil nil nil nil org-commentary--version)
    (("-n" "--dry-run") nil :dry-run)
    (("-q" "--silent") nil :silent)
    (("-s" "--section") "commentary" :section t ,org-commentary--section-names)
    (("-c" "--charset") ascii :ascii-charset t (("ascii" . ascii)
                                                ("utf-8" . utf-8)
                                                ("latin1" . latin1))))
  "Specification of command line arguments.

Each element has the form
    ((NAME...) DEFAULT KEY OPTION-P VALIDATOR HANDLER DATUM...)
where
    - NAME is the argument name string (e.g., \"-h\", \"--help\");
    - DEFAULT is the default value of the argument, if defined;
    - KEY (optional) is a keyword which defines the key under which
      the argument's value will be stored in the parsed arguments plist;
    - OPTION-P is a boolean indicating whether the argument
      is an option (takes value) or a flag;
    - VALIDATOR (optional) is either
          - a function to validate and transform the value;
          - a list of valid values;
          - an association list to map valid values to transformed values;
    - HANDLER (optional) is either a function to invoke or a signal to fire
      when the argument is encountered;
    - DATUM (optional) extra arguments to pass when invoking HANDLER.")

(defconst org-commentary--cli-args-alist
  (-mapcat (-lambda ((names _ . rest))
             (-map (lambda (name)
                     `(,name ,@rest))
                   names))
           org-commentary--cli-args-spec))

(defconst org-commentary--cli-args-defaults-plist
  (let (defaults-plist)
    (-each org-commentary--cli-args-spec
      (-lambda ((_ default key . _))
        (when (and default key)
          (setf defaults-plist
                (plist-put defaults-plist key default)))))
    defaults-plist))

(defconst org-commentary--usage
  (format "org-commentary %s
Update comment headers of elisp files using Org mode documents.

Usage:
    cask exec org-commentary [OPTION]... ORG-FILE ELISP-FILE

Usage (without Cask):
    emacs -Q --batch --eval '(package-initialize)' -l org-commentary-cli -f \\
        org-commentary -- [OPTION]... ORG-FILE ELISP-FILE

Flags:
    -h, --help       display this message
    -v, --version    display version information
    -n, --dry-run    don't update ELISP-FILE, just display the ORG-FILE export result
    -q, --silent     don't display the ORG-FILE export result

Options:
    -s, --section [commentary]    specify which comment section to update
                                  [values: commentary, changelog, history]
    -c, --charset [ascii]         the charset allowed to represent various
                                  elements and objects during export
                                  [values: ascii, utf-8, latin1]"
          org-commentary-version))

(define-error 'org-commentary--cli-argument-error
  "Invalid command-line argument" 'user-error)
(define-error 'org-commentary--duplicate-argument
  "Duplicate option or flag" 'org-commentary--cli-argument-error)
(define-error 'org-commentary--invalid-option-value
  "Invalid option value" 'org-commentary--cli-argument-error)
(define-error 'org-commentary--unknown-argument
  "Unkown option or flag" 'org-commentary--cli-argument-error)
(define-error 'org-commentary--missing-value
  "Missing option value" 'org-commentary--cli-argument-error)
(define-error 'org-commentary--positional-arg-count-mismatch
  "Too few or too many positional arguments" 'org-commentary--cli-argument-error)

(define-error 'org-commentary--usage "")
(define-error 'org-commentary--version "")

(defun org-commentary--handle (handler value datum)
  "Invoke HANDLER with VALUE and extra data DATUM.

HANDLER is either a function or a name of a signal defined
with `define-error'.  If HANDLER is none of the above, raise an error."
  (cond ((and (symbolp handler)
              (get handler 'error-conditions))
         (signal handler datum))
        ((functionp handler)
         (apply handler value datum))
        (t
         (error "Invalid handler: %S" handler))))

(defun org-commentary--validate (value validator)
  "Validate (and transform) VALUE using VALIDATOR.

VALIDATOR should be either
    - a function to return a transformed value
      or nil if the value is invalid;
    - a list of of valid values;
    - an association list (VALUE . TRANSFORMED-VALUE) where
      VALUE is a valid value and TRANSFORMED-VALUE is
      the value it should be mapped to.

The function returns the transformed value or nil
if VALUE is invalid."
  (cond ((functionp validator)
         (funcall validator value))
        ((listp validator)
         (-when-let (pair (assoc-string value validator))
           (if (consp pair)
               (cdr pair)
             pair)))
        (t
         (error "Invalid validator: %S" validator))))

(defun org-commentary--parse-args (args)
  "Parse command line arguments ARGS.

Result is a property list
    (:org ORG-FILE
     :elisp ELISP-FILE
     :section SECTION
     :silent SILENT-P
     :dry-run DRY-RUN-P)."
  (setf args (-remove-item "--" args))
  (when (null args)
    (signal 'org-commentary--usage nil))
  (let (positional args-plist)
    (while args
      (let ((argi (pop args))
            value)
        ;; normalize long options
        (when (string-match "^\\(--[^=]+\\)=" argi)
          (setf value (substring argi (match-end 0))
                argi (match-string 1 argi))

          (unless (or (null value)
                      (string-empty-p value))
            (push value args)))

        (-if-let (arg-spec (assoc argi org-commentary--cli-args-alist))
            ;; handle known arguments
            (let ((key (nth 1 arg-spec))
                  (option? (nth 2 arg-spec))
                  (validator (nth 3 arg-spec))
                  (handler (nth 4 arg-spec))
                  (datum (nthcdr 5 arg-spec)))

              ;; handle duplicate arguments
              (when (plist-get args-plist key)
                (signal 'org-commentary--duplicate-argument
                        (format "option `%s' defined multiple times"
                                argi)))

              ;; handle options
              (when option?
                (setf value (pop args))

                ;; missing values
                (unless value
                  (signal 'org-commentary--missing-value
                          (format "option `%s' requires a value"
                                  argi)))

                ;; validation
                (when validator
                  (setf value
                        (or (org-commentary--validate value validator)
                            (signal 'org-commentary--invalid-option-value
                                    (format "invalid value for option `%s': %s"
                                            argi value))))))

              (when key
                (setf args-plist
                      (plist-put args-plist key (if option? value t))))

              (when handler
                (org-commentary--handle handler (if option? value t) datum)))
          ;; handle unknown arguments
          (if (string-prefix-p "-" argi)
              ;; unknown options and flags
              (signal 'org-commentary--unknown-argument
                      (format "unknown option or flag: `%s'" argi))

            ;; treat rest values as positional arguments
            (push argi positional)))))

    (when (/= 2 (length positional))
      (signal 'org-commentary--positional-arg-count-mismatch
              (format "expected exactly 2 positional arguments; %s given"
                      (length positional))))
    (setf positional (reverse positional))

    (setf args-plist (plist-put args-plist :org (nth 0 positional)))
    (setf args-plist (plist-put args-plist :elisp (nth 1 positional)))

    (org-combine-plists org-commentary--cli-args-defaults-plist
                        args-plist)))

(defun org-commentary--usage (&optional exit-code)
  "Display `org-commentary' usage information and exit.

EXIT-CODE is an integer used as the exit status (defaults to 0)."
  (message "%s" org-commentary--usage)
  (kill-emacs (or exit-code 0)))

(defun org-commentary ()
  "Parse command line arguments and update elisp library headers accordingly."
  (unwind-protect
      (condition-case error
          (let* ((args (org-commentary--parse-args argv))

                 ;; custom options
                 (org (plist-get args :org))
                 (elisp (plist-get args :elisp))
                 (section (plist-get args :section))
                 (silent (plist-get args :silent))
                 (dry-run (plist-get args :dry-run))

                 ;; result
                 export-result)

            ;; remove custom options
            (--each '(:org :elisp :section :dry-run :silent)
              (setf args (plist-put args it nil)))

            ;; generate result
            (setf export-result
                  (if dry-run
                      (org-commentary-export-file-as-string org args)
                    (org-commentary-update section org elisp args)))

            (unless silent
              (message "%s" export-result)))

        ;; handle errors
        (org-commentary--cli-argument-error
         (message "cli-error: %s." (cdr error))
         (org-commentary--usage 1))
        (org-commentary--usage
         (org-commentary--usage))
        (org-commentary--version
         (message "org-commentary %s" org-commentary-version))
        (error
         (message "%s" (string-join (mapcar (lambda (elt)
                                              (format "%s" elt))
                                            error)
                                    ": "))))
    (kill-emacs)))

(provide 'org-commentary-cli)
;;; org-commentary-cli.el ends here
