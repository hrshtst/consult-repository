;;; consult-repository --- Consult huge number of repos -*- lexical-binding: t -*-

;; Author: Hiroshi Atsuta <atsuta@ieee.org>
;; Maintainer: Hiroshi Atsuta <atsuta@ieee.org>
;; Created: 28 Jan 2022
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Keywords: extensions
;; Package-Requires: ((emacs "27.1") (consult "0.14"))
;; Homepage: https://github.com/hrshtst/consult-repository

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Consult interface to find or grep within a repository selected from
;; several sources.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'consult)

;;;; User configuration

(defgroup consult-repository nil
  "Consult huge number of repos."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :prefix "consult-repository-"
  :link '(url-link "https://github.com/hrshtst/consult-repository"))

;;;;; User options

(defcustom consult-repository--find-command
  (cond ((fboundp 'affe-find) #'affe-find)
        ((fboundp 'consult-find) #'consult-find))
  "Function to search for regexp with find."
  :type 'function)

(defcustom consult-repository--grep-command
  (cond ((fboundp 'affe-grep) #'affe-grep)
        ((fboundp 'consult-grep) #'consult-grep))
  "Function to search for regexp with grep."
  :type 'function)

(defcustom consult-repository-sources
  '(consult-repository--source-ghq
    consult-repository--source-straight)
  "Sources used by `consult-repository--pick'."
  :type '(repeat symbol))

;;;; Utility functions
;;;;; Strings

(defun consult-repository--split (string)
  "Split the STRING on newlines, returning a list.
Remove any blank lines at the beginning or end."
  (let ((parts (split-string string "\n")))
    ;; Remove blank lines from beginnig.
    (while (equal (car parts) "")
      (setq parts (cdr parts)))
    (setq parts (nreverse parts))
    ;; Remove blank lines from end.
    (while (equal (car parts) "")
      (setq parts (cdr parts)))
    (nreverse parts)))

;;;;; External processes

(defun consult-repository--process-call (program &rest args)
  "Run PROGRAM synchronously with ARGS.
Return output to STDOUT from the PROGRAM as a string. If the
process exits with a non-zero status, throw an error. If the
process is unable to start, return an elisp error object."
  (condition-case e
      (with-temp-buffer
        (unless (zerop
                 (apply #'call-process program nil t nil args))
          (error (format
                  "The process %s ends with a non-zero status"
                  program)))
        (let ((s (buffer-string)))
          (unless (string-empty-p s) s)))
    (error e)))

;;;; List functions

(defun consult-repository--ghq-repos ()
  "Return a list of repositories managed by ghq."
  (mapcar #'abbreviate-file-name
          (consult-repository--split
           (consult-repository--process-call
            "ghq" "list" "--full-path"))))

(defun consult-repository--straight-repos ()
  "Return a list of repositories managed by straight.el."
  (mapcar #'abbreviate-file-name
          (cl-remove-if
           (lambda (file)
             (string-match-p
              "\\(?:\\(?:\\.\\(?:\\.\\|git\\)?\\)$\\)" file))
           (directory-files (straight--repos-dir) 'full))))

;;;; Internal variables

(defvar consult-repository--history nil)

(defvar consult-repository--source-ghq
  `(:name "Ghq repos"
    :narrow ?g
    :category file
    :face consult-file
    :history file-name-history
    :enabled ,(lambda () (executable-find "ghq"))
    :items ,#'consult-repository--ghq-repos)
  "Ghq repository source for `consult-repository--pick'.")

(defvar consult-repository--source-straight
  `(:name "Straight repos"
    :narrow ?s
    :category file
    :face consult-file
    :history file-name-history
    :enabled ,(lambda () (fboundp 'straight-use-package))
    :items ,#'consult-repository--straight-repos)
  "Straight repository source for `consult-repository--pick'.")

(defun consult-repository--pick (&optional prompt)
  "Return path to the selected directory.
PROMPT is the prompt string shown in the minibuffer."
  (when-let (repo (consult--multi consult-repository-sources
                                  :prompt (or prompt "Switch repo: ")
                                  :history 'consult-repository--history
                                  :sort nil))
    (expand-file-name (car repo))))

;;;; Interactive functions

;;;###autoload
(defun consult-repository-grep (&optional initial)
  "Search for regexp with grep in selected repository.

The command pops up a `consult' interface to select a repository
path defined in `consult-repository-sources', then searches for
regexp with grep in the selected repository with an optional
INITIAL input. The grep command to execute is determined by the
`consult-repository--grep-command', which is set to
`consult-grep' or `affe-grep' by default."
  (interactive "P")
  (let* ((repo (consult-repository--pick "In repository: "))
         (default-directory repo))
    (funcall consult-repository--grep-command repo initial)))

;;;###autoload
(defun consult-repository-find (&optional initial)
  "Search for regexp with find in selected repository.

The command pops up a `consult' interface to select a repository
path defined in `consult-repository-sources', then searches for
regexp with find in the selected repository with an optional
INITIAL input. The find command to execute is determined by the
`consult-repository--find-command', which is set to
`consult-find' or `affe-find' by default."
  (interactive "P")
  (let* ((repo (consult-repository--pick "In repository: "))
         (default-directory repo))
    (funcall consult-repository--find-command repo initial)))

(provide 'consult-repository)
;;; consult-repository.el ends here
