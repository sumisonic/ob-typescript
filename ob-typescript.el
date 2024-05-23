;;; ob-typescript.el --- org-babel functions for typescript evaluation

;; Copyright (C) 2015 KURASHIKI Satoru

;; Author: KURASHIKI Satoru
;; Keywords: literate programming, reproducible research, typescript
;; Homepage: https://github.com/lurdan/ob-typescript
;; Version: 0.1
;; Package-Requires: ((emacs "24") (org "8.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Exec typescript in org-babel code blocks.

;;; Requirements:
;; You need to install node.js and typescript to use this extension.

;;; Code:
(require 'ob)
;;(require 'ob-ref)
;;(require 'ob-comint)
;;(require 'ob-eval)

;;(require 'typescript)

(add-to-list 'org-babel-tangle-lang-exts '("typescript" . "ts"))

(defvar org-babel-typescript-tsconfig-path nil
  "Path to the tsconfig.json file used for TypeScript compilation.")

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:typescript '((:cmdline . "--noImplicitAny")))

(defvar org-babel-command:typescript "tsc"
  "Command run by ob-typescript to launch tsc compiler")

(defun org-babel-variable-assignments:typescript (params)
  "Return list of typescript statements assigning the block's variables."
  (mapcar (lambda (pair) (format "let %s=%s;"
                                 (car pair) (org-babel-typescript-var-to-typescript (cdr pair))))
          (org-babel--get-vars params)))

(defun org-babel-typescript-var-to-typescript (var)
  "Convert an elisp var into a string of typescript source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-typescript-var-to-typescript var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun get-tsc-version ()
  "Get typescript compiler version"
  (let ((ver-str (org-babel-eval (concat org-babel-command:typescript " --version") "")))
    (string-match "[[:digit:]]+" ver-str)
    (string-to-number (match-string 0 ver-str))))

(defun org-babel-execute:typescript (body params)
  "Execute a block of Typescript code with org-babel. This function is
called by `org-babel-execute-src-block'"
  (let* ((project-root (if org-babel-typescript-tsconfig-path
                           (file-name-directory org-babel-typescript-tsconfig-path)
                         ""))
         (tmp-src-file (org-babel-temp-file "ts-src-" ".ts"))
         (output-dir (concat project-root "/dist"))
         (src-file-path (concat project-root "/src/" (file-name-nondirectory tmp-src-file)))
         (tmp-out-file (concat output-dir "/" (file-name-nondirectory (file-name-sans-extension tmp-src-file)) ".js"))
         (cmdline (cdr (assoc :cmdline params)))
         (cmdline (if cmdline (concat " " cmdline) ""))
         (tsconfig-arg (if org-babel-typescript-tsconfig-path
                           (format "--project %s" org-babel-typescript-tsconfig-path)
                         ""))
         (jsexec (if (assoc :wrap params) ""
                   (concat " && node " (org-babel-process-file-name tmp-out-file))))
         (results (progn
                    ;; Ensure the output directory exists
                    (make-directory output-dir t)
                    ;; Ensure the src directory exists
                    (make-directory (concat project-root "/src") t)
                    ;; Copy the temp source file to the src directory
                    (copy-file tmp-src-file src-file-path t)
                    ;; Compile the TypeScript code using tsc with the project configuration
                    (org-babel-eval (format "cd %s && %s %s" project-root org-babel-command:typescript tsconfig-arg) "")
                    ;; Execute the compiled JavaScript file
                    (org-babel-eval (format "node %s" tmp-out-file) ""))))
    results))

(provide 'ob-typescript)

;;; ob-typescript.el ends here
