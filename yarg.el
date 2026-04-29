;;; yarg.el --- ripgrep interface              -*- lexical-binding: t; -*-

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: tools, processes
;; Version: 0.1

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

;; A minimal Emacs interface to ripgrep (rg), built on compilation-mode.
;;
;; Usage:
;;   M-x yarg           Search symbol at point from project root immediately.
;;   C-u M-x yarg       Same, but edit the rg command first.
;;   C-u C-u M-x yarg   Choose a directory and edit the rg command.
;;
;; The only user option is `yarg-rg-switches'.

;;; Code:

(require 'compile)
(require 'ansi-color)
(require 'thingatpt)
(require 'project)

(defgroup yarg nil
  "Run ripgrep and display results."
  :group 'tools
  :group 'processes)

(defcustom yarg-rg-switches
  "-n --no-heading --color always --smart-case --hidden --glob=!.git -M 1500"
  "Switches passed to rg before the -e <pattern> argument.
Removing -n, --no-heading and --color will break highlighting and/or
error navigation."
  :type 'string
  :group 'yarg)

;;; Compilation plumbing

(defun yarg--filter ()
  "Apply ANSI color sequences from rg output, tagging colored spans."
  (save-excursion
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (when face
               (ansi-color-apply-overlay-face beg end face)
               (put-text-property beg end 'yarg-color t)))))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(defun yarg--column-start ()
  (or (let* ((beg (match-end 0))
             (end (save-excursion (goto-char beg) (line-end-position)))
             (mbeg (text-property-any beg end 'yarg-color t)))
        (when mbeg (- mbeg beg)))
      (when (match-string 4)
        (1- (string-to-number (match-string 4))))))

(defun yarg--column-end ()
  (let* ((beg (match-end 0))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (mbeg (text-property-any beg end 'yarg-color t))
         (mend (and mbeg (next-single-property-change mbeg 'yarg-color nil end))))
    (when mend (- mend beg))))

(defconst yarg-error-regexp-alist
  ;; rg --no-heading produces: file:line:col:content
  ;; The optional group 4 is the column number.
  `(("^\\(.+?\\)\\(:\\)\\([1-9][0-9]*\\)\\2\
\\(?:\\(?:\\(?4:[1-9][0-9]*\\)\\2\\)\\|[^0-9\n]\\|[0-9][^0-9\n]\\|\\.\\.\\.\\)"
     1 3 (yarg--column-start . yarg--column-end) nil 1
     (4 compilation-column-face nil t))
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Compilation error-regexp-alist for rg --no-heading output.")

(define-compilation-mode yarg-mode "Yarg"
  "Compilation mode for ripgrep output."
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-face 'compilation-info)
  (add-hook 'compilation-filter-hook #'yarg--filter nil t))

;;; Core logic

(defvar yarg-history nil "Minibuffer history for `yarg'.")

(defun yarg--project-root ()
  (if-let* ((proj (project-current))) (project-root proj) default-directory))

(defun yarg--build-command (pattern)
  (format "rg %s -e %s" yarg-rg-switches (shell-quote-argument pattern)))

(defun yarg--read-command (initial-pattern directory)
  (let* ((dir-name (file-name-nondirectory (directory-file-name directory)))
         (prompt (format "Yarg [%s]: " dir-name))
         (prefix (format "rg %s -e " yarg-rg-switches))
         (init (if initial-pattern
                   (concat prefix (shell-quote-argument initial-pattern))
                 (concat prefix "''")))
         ;; Place cursor at end when a pattern is pre-filled, between
         ;; the quotes when starting empty.
         (pos (if initial-pattern (length init) (1+ (length prefix)))))
    (read-from-minibuffer prompt (cons init pos) nil nil 'yarg-history)))

;;;###autoload
(defun yarg (arg)
  "Run ripgrep from the project root, collecting output in `yarg-mode'.

With no prefix ARG: search for the symbol at point immediately.
With one \\[universal-argument]: pre-fill the symbol at point but allow
  editing the rg command before running.
With two \\[universal-argument]'s: choose the search directory, then
  edit the rg command.

The search pattern is quoted and passed after rg's -e flag so it
is always treated as a literal string unless the user edits it."
  (interactive "P")
  (let* ((numeric (prefix-numeric-value arg))
         (symbol (thing-at-point 'symbol t))
         (choose-dir (>= numeric 16))
         (edit-p (or (>= numeric 4) (not symbol)))
         (directory (if choose-dir
                        (read-directory-name "Search in: " nil nil t)
                      (yarg--project-root)))
         (command (if (and symbol (not edit-p))
                      (yarg--build-command symbol)
                    (yarg--read-command symbol directory))))
    (let ((default-directory (expand-file-name directory)))
      (compilation-start command 'yarg-mode))))

(provide 'yarg)
;;; yarg.el ends here
