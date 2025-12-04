;;; agental-tool.el --- agental tool                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'agental)
(require 'agental-context)

(defcustom agental-tool-read-tools (list #'agental-tool-glob-tool #'agental-tool-ls-tool #'agental-tool-read-file-tool)
  ""
  :type '(list symbol)
  :group 'agental)

(defcustom agental-tool-edit-tools (list #'agental-tool-edit-file-tool)
  ""
  :type '(list symbol)
  :group 'agental)

;;; glob-tool

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(require 'gptel)

(defun agental-tool--read-ignore-list (dir filename)
  "Read ignore patterns from FILENAME inside DIR.
return ignore patterns as a list of strings.
Empty lines and comments (#...) are ignored."
  (let ((ignore-file (expand-file-name filename dir)))
    (when (file-exists-p ignore-file)
      (split-string
       (with-temp-buffer
         (insert-file-contents ignore-file)
         (replace-regexp-in-string "^#.*" "" (buffer-string)))
       "\n" t "[[:space:]]+"))))

;;; glob tool
(defun agental-tool--glob-sort-files (files)
  "Sort FILES by modification time (newest first)."
  (sort files
        (lambda (a b)
          (> (float-time (file-attribute-modification-time (file-attributes a)))
             (float-time (file-attribute-modification-time (file-attributes b)))))))


(defun agental-tool--apply-ignore-rules (files dir respect-git respect-ai)
  "Filter FILES in DIR.
Removing paths that match .gitignore or .aiignore patterns in DIR. If
RESPECT-GIT or RESPECT-AI are nil, the corresponding ignore file is not
considered."
  (let* ((git-ignore (and respect-git (agental-tool--read-ignore-list dir ".gitignore")))
         (ai-ignore  (and respect-ai (agental-tool--read-ignore-list dir ".aiignore")))
         (ignores (append git-ignore ai-ignore)))
    (seq-remove
     (lambda (f)
       (seq-some (lambda (pat)
                   (string-match-p (wildcard-to-regexp pat)
                                   (file-relative-name f dir)))
                 ignores))
     files)))

(cl-defun agental-tool-glob-tool (pattern dir-path &optional (case-sensitive nil) (respect-git-ignore t) (respect-ai-ignore t))
  "Find files matching PATTERN using `file-expand-wildcards'.

Arguments:
  PATTERN (string):The glob pattern to match, e.g. \"src/**/*.el\" or \"*.org\".
  DIR-PATH (string, optional): Directory in which to search.
  CASE-SENSITIVE (boolean, optional): If non-nil, matching is case-sensitive.
  RESPECT-GIT-IGNORE (boolean, optional): If non-nil, obey .gitignore rules.
  RESPECT-AI-IGNORE (boolean, optional): If non-nil, obey .aiignore rules.

Returns a list of absolute file paths, sorted by modification time (newest
first). If no files are found, returns nil and displays a message."
  (let* ((dir dir-path)
         (default-directory (file-name-as-directory dir))
         ;; Adjust pattern case if case-insensitive search
         (pattern (if (and (not case-sensitive)
                           (string-match-p "[A-Z]" pattern))
                      (downcase pattern)
                    pattern))
         (files (file-expand-wildcards pattern t))
         (filtered (agental-tool--apply-ignore-rules
                    files dir
                    (or respect-git-ignore t)
                    (or respect-ai-ignore t)))
         (sorted (agental-tool--glob-sort-files filtered)))
    (if (null sorted)
        (format "No files found matching: %s in %s" dir pattern)
      (format "Found %d files matching '%s' (sorted by modification time):\n%s"
              (length sorted)
              pattern
              (string-join sorted "\n")))))

;; Register the tool with GPTel
(gptel-make-tool
 :name "find_files"
 :function #'agental-tool-glob-tool
 :description
 "Find files matching a given glob pattern using Emacs' file-expand-wildcards.
Returns a list of files sorted by modification time (newest first). Supports ignore rules and recursive patterns."
 :args (list
        '(:name "pattern"
                :type string
                :description "The glob pattern to match, e.g. '**/*.el' or '*.org'")
        '(:name "dir-path"
                :type string
                :description "Optional directory path to search within")
        '(:name "case-sensitive"
                :type boolean
                :description "Whether the search is case-sensitive (default: false)"
                :optional t)
        '(:name "respect-git-ignore"
                :type boolean
                :description "Whether to obey .gitignore (default: true)"
                :optional t)
        '(:name "respect-ai-ignore"
                :type boolean
                :description "Whether to obey .aiignore (default: true)"
                :optional t))
 :category "agental")

;;; ls tools
(defun agental-tool--should-ignore (filename patterns)
  "Return non-nil if FILENAME matches any glob PATTERNS.
Each pattern may include '*' and '?' wildcards."
  (when (and patterns (listp patterns))
    (seq-some
     (lambda (pattern)
       (let* ((regex-pattern
               (replace-regexp-in-string
                "\\?" "."
                (replace-regexp-in-string
                 "\\*" ".*"
                 (replace-regexp-in-string
                  "[.+^${}()|[\\]\\]" "\\\\\\&"
                  pattern))))
              (regex (concat "^" regex-pattern "$")))
         (string-match-p regex filename)))
     patterns)))

(defun agental-tool--ls-collect-files (dir ignore respect-git respect-ai)
  "Collect and filter files in DIR based on IGNORE patterns and ignore files.
Respects .gitignore and .aiignore when requested.

RESPECT-GIT (boolean, optional): If non-nil, obey .gitignore rules.
RESPECT-AI (boolean, optional): If non-nil, obey .aiignore rules."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (entries (directory-files dir t "^[^.]" t))
         (git-ignore (and respect-git (agental-tool--read-ignore-list dir ".gitignore")))
         (ai-ignore  (and respect-ai (agental-tool--read-ignore-list dir ".aiignore")))
         (ignore-list (append ignore git-ignore ai-ignore))
         results)
    (dolist (f entries)
      (unless (agental-tool--should-ignore (file-name-nondirectory f) ignore-list)
        (when (file-exists-p f)
          (let* ((attrs (file-attributes f))
                 (is-dir (car attrs))
                 (size (if is-dir 0 (nth 7 attrs)))
                 (mtime (nth 5 attrs)))
            (push (list
                   (cons 'name (file-name-nondirectory f))
                   (cons 'path f)
                   (cons 'isDirectory is-dir)
                   (cons 'size size)
                   (cons 'modifiedTime mtime))
                  results)))))
    ;; sort: directories first, then alphabetically
    (setq results
          (sort results
                (lambda (a b)
                  (let ((ad (alist-get 'isDirectory a))
                        (bd (alist-get 'isDirectory b)))
                    (if (eq ad bd)
                        (string< (alist-get 'name a) (alist-get 'name b))
                      ad)))))
    results))

(defun agental-tool-ls-tool (dir-path &optional ignore respect-git-ignore respect-ai-ignore)
  "List directory contents for DIR-PATH, respecting ignore patterns.

Arguments:
  DIR-PATH (string): The directory path to list.
  IGNORE (list of string): List of glob patterns to ignore.
  RESPECT-GIT-IGNORE (boolean): Whether to respect .gitignore (default: t).
  RESPECT-AI-IGNORE (boolean): Whether to respect .aiignore (default: t).

Returns a list of alists describing files, each with keys:
  - name: filename
  - path: absolute path
  - isDirectory: non-nil if it is a directory
  - size: file size in bytes
  - modifiedTime: last modification time."
  (condition-case err
      (let* ((dir (or dir-path default-directory)))
        (unless (file-directory-p dir)
          (error "Path is not a directory: %s" dir))
        (let ((entries (agental-tool--ls-collect-files
                        dir ignore
                        (or respect-git-ignore t)
                        (or respect-ai-ignore t))))
          (if (null entries)
              (format "Directory %s is empty." dir)
            (format "Listed %d entries in %s (sorted by modification time):\n%s"
                    (length entries)
                    dir
                    (string-join (mapcar (lambda (item)
                                           (alist-get 'name item))
                                         entries)
                                 "\n")))))
    (error
     (format "Error listing directory: %s" (error-message-string err)))))

;; Register with GPTel
(gptel-make-tool
 :name "list_directory"
 :function #'agental-tool-ls-tool
 :description
 "List the names of files and subdirectories directly within a specified directory path.
Supports glob-style ignore patterns and optionally respects .gitignore and .aiignore files."
 :args (list
        '(:name "dir-path"
                :type string
                :description "The path to the directory to list")
        '(:name "ignore"
                :type array
                :description "List of glob patterns to ignore"
                :optional t)
        '(:name "respect-git-ignore"
                :type boolean
                :description "Whether to respect .gitignore, Only available in git repositories. (default: true)"
                :optional t)
        '(:name "respect-ai-ignore"
                :type boolean
                :description "Whether to respect .aiignore (default: true)"
                :optional t))
 :category "agental")

;;; read file tools

;; (defcustom agental-tool-max-read-length 1048576
;;   "Maximum number of characters to read from a file.

;; Non-nil means read at most this many characters from any file. If nil, read the
;; entire file regardless of its size. This variable is used by the file reading
;; tools in this package to limit memory usage when processing large files."
;;   :type 'number)

(defcustom agental-tool-max-read-length 100
  "Maximum number of line to read from a file.

Non-nil means read at most this many characters from any file. If nil, read the
entire file regardless of its size. This variable is used by the file reading
tools in this package to limit memory usage when processing large files."
  :type 'number
  :group 'agental-tool)

(defun agental-tool--read-string (content &optional offset limit show-line-numbers)
  "Read string CONTENT with optional line-number OFFSET and LIMIT.

- CONTENT is the full string content to read from.

- OFFSET, if provided, specifies the line number to start reading
  from (1-based). For negative values, starts at that many lines before
  the end of the file.

- LIMIT, if provided, specifies the number of lines to read from the
  start position. For negative values, the actual limit is computed as
  (total_lines + limit). For example, with a 100-line file: limit=10
  reads 10 lines, limit=-10 reads 90 lines (100 + (-10)).

- SHOW-LINE-NUMBERS, if non-nil, formats output in cat -n style with
  line numbers. Lines are formatted as \"[spaces for alignment][line
  number][tab][line content]\".

If neither OFFSET nor LIMIT is provided, returns the full content.

If only OFFSET is provided, returns content from that line to the end.

If only LIMIT is provided, returns the first LIMIT lines.

If both are provided, returns LIMIT lines starting from OFFSET.

Returns the processed content as a string."
  (if (and (not offset) (not limit) (not show-line-numbers))
      ;; Return full content if no parameters provided.
      content
    (let* ((lines (split-string content "\n"))
           (num-lines (length lines))
           (start-idx
            (if offset
                (cond
                 ;; Negative offset: start at that many lines before the end.
                 ((< offset 0)
                  (max 0 (+ num-lines offset)))
                 ;; Zero or positive offset: 1-based indexing (treat 0 as 1).
                 (t
                  (max 0 (min (1- (max 1 offset)) num-lines))))
              0))
           (actual-limit
            (when limit
              (cond
               ;; Negative limit: equivalent to total lines - (negative limit value).
               ((< limit 0)
                (max 0 (+ num-lines limit)))
               ;; Zero or positive limit: use as-is.
               (t
                limit))))
           (end-idx
            (if actual-limit
                (min num-lines (+ start-idx actual-limit))
              num-lines))
           (selected-lines (seq-subseq lines start-idx end-idx)))
      (cond
       (show-line-numbers
        ;; Format with line numbers (cat -n style).
        (let* ((actual-start-line (1+ start-idx))
               ;; `cat -n` includes the trailing newline if present, but doesn't number it. We
               ;; handle this as a special case. If:
               ;;
               ;; - the last line is empty
               ;; - we're looking at the actual last line of the content (i.e. not a blank line in
               ;;   the middle)
               ;; - there's more than one line (since `cat -n` on a completely blank file does show
               ;;   line number 1
               ;;
               ;; then exclude the last line from the list of lines to be numbered, and add it back
               ;; at the end.
               (should-exclude-final-empty
                (and (= end-idx num-lines) ; processing to end.
                     (> (length selected-lines) 1) ; we have more than one line.
                     (string-empty-p (car (last selected-lines))))) ; last line is empty.
               (lines-to-number
                (if should-exclude-final-empty
                    (butlast selected-lines)
                  selected-lines))
               (max-line-num (+ actual-start-line (length lines-to-number) -1))
               (line-num-width
                (if (> (length lines-to-number) 0)
                    (length (number-to-string max-line-num))
                  1))
               (formatted-lines
                (cl-loop
                 for
                 line
                 in
                 lines-to-number
                 for
                 line-num
                 from
                 actual-start-line
                 collect
                 (format (concat "%" (number-to-string line-num-width) "d\t%s") line-num line)))
               (result (string-join formatted-lines "\n")))
          ;; Add the final trailing newline if we excluded the final empty line
          (if should-exclude-final-empty
              (concat result "\n")
            result)))
       ;; Regular format without line numbers.
       (t
        (string-join selected-lines "\n"))))))

(defun agental-tool--read-file (path &optional offset limit show-line-numbers)
  "Read the contents of a file specified by PATH.

PATH is the path to the file, relative to the workspace root.

OFFSET, if provided, specifies the line number to start reading
from (1-based).

LIMIT, if provided, specifies the number of lines to read.

SHOW-LINE-NUMBERS, if non-nil, formats output in cat -n style with line numbers.

Returns the file contents as a string, with optional
offset/limit/show-line-numbers processing. For symlinks, returns the target
path instead of following the link. Signals an error if the file is not
found in the workspace."
  (let* ((full-path (file-truename path)))
    (if (file-exists-p full-path)
        ;; Check if this is a symlink first (only for existing files).
        (if (file-symlink-p full-path)
            ;; For symlinks, return the target path instead of following the link.
            (let ((target (file-symlink-p full-path)))
              (format "Symlink target: %s" target))

          ;; Normal/non-symlink handling.
          ;; Some LLMs (for example qwen3-coder at time of writing) seem to have trouble invoking
          ;; tools with integer inputs - they'll always pass e.g. '1.0' instead of '1'. Therefore we
          ;; need to support float inputs, which in general we handle by rounding to the nearest
          ;; integer.
          (let* ((content (with-temp-buffer (insert-file-contents full-path) (buffer-string)))
                 (parsed-offset
                  (if offset
                      (round offset)
                    0))
                 (parsed-limit
                  (if limit
                      (min (round limit) agental-tool-max-read-length)
                    agental-tool-max-read-length))
                 (processed-content
                  (agental-tool--read-string content parsed-offset parsed-limit show-line-numbers)))
            ;; Check if the processed content exceeds the maximum read length.
            (if (> parsed-limit agental-tool-max-read-length)
                ;; truncated file.
                (concat
                 "IMPORTANT: The file content has been truncated.\n"
                 (format "Status: Showing lines %d-%d  of %d total lines.\n" parsed-offset (+ parsed-limit parsed-offset) parsed-limit)
                 (format "Action: To read more of the file, you can use the 'offset' and 'limit' parameters in a subsequent 'read_file_in_workspace' call. For example, to read the next section of the file, use offset: %s.\n"
                         (+ offset parsed-limit))
                 (substring processed-content 0 agental-tool-max-read-length)
                 "<truncated...>")
              processed-content)))
      (error "File not exists"))))

(defun agental-tool-read-file-tool (path &optional offset limit show-line-numbers)
  "Read the contents of a file specified by PATH.

PATH is the path to the file, relative to the workspace root.

OFFSET, if provided, specifies the line number to start reading
from (1-based).

LIMIT, if provided, specifies the number of lines to read.

SHOW-LINE-NUMBERS, if non-nil, formats output in cat -n style with line numbers.

Returns the file contents as a string, with optional
offset/limit/show-line-numbers processing. For symlinks, returns the target
path instead of following the link. Signals an error if the file is not
found in the workspace."
  (let* ((project-metadata (and agental--context
                                (agental-context-metadata-project-metadata agental--context)))
         (project-dir (cdr project-metadata))
         (full-path (if (and project-dir (not (file-name-absolute-p path)))
                        (expand-file-name path project-dir)
                      (file-truename path))))
    (condition-case err
        (agental-tool--read-file full-path offset limit show-line-numbers)
      (error
       (error-message-string err)))))

(gptel-make-tool
 :name "read_file_in_workspace"
 :function #'agental-tool-read-file-tool
 :description
 (concat
  "Reads and returns the content of a specified file. If the file is large, the content will be truncated."
  "The tool's response will clearly indicate if truncation has occurred and will provide details on how to read more of the file using the 'offset' and 'limit' parameters."
  "Handles text files. For text files, it can read specific line ranges.")
 :confirm nil
 :include nil
 :args
 `((:name "path" :type string :description "Path to the file, relative to workspace root")
   (:name
    "offset"
    :type number
    :optional t
    :description
    ,(concat
      "Optional: For text files, the 0-based line number to start reading from."
      "Requires 'limit' to be set. Use for paginating through large files."))
   (:name
    "limit"
    :type number
    :optional t
    :description
    ,(concat
      "Optional: For text files, maximum number of lines to read."
      " Use with 'offset' to paginate through large files."
      " If omitted, reads the entire file (if feasible, up to a default limit)."))
   (:name
    "show_line_numbers"
    :type boolean
    :optional t
    :description
    ,(concat
      "Include line numbers in output (cat -n style: each line prefixed with right-aligned "
      "line number and a tab character)")))
 :category "agental")

;;; grep tool
(defun agental-tools-ripgrep-search (pattern path file-regexp case-insensitive lines-after lines-before)
  "Use ripgrep search in PATH with PATTERN.

PATTERN is the regular expression to search for.
PATH is the directory or file to search.
FILE-REGEXP is a regular expression to filter files.
CASE-INSENSITIVE, if non-nil, performs a case-insensitive search.
LINES-AFTER specifies number of lines to show after each match.
LINES-BEFORE specifies number of lines to show before each match.

Returns the ripgrep command as a string."
  (let ((args))
    ;; Add case-insensitive flag
    (when case-insensitive
      (push "-i" args))
    ;; Add context lines
    (when lines-before
      (push (format "--before-context=%d" lines-before) args))
    (when lines-after
      (push (format "--after-context=%d" lines-after) args))
    ;; Add file type filter
    (when file-regexp
      (push (format "--type-add=%s" file-regexp) args)
      (push "--type=custom" args))
    ;; Add pattern
    (push pattern args)
    ;; Add search path
    (when path
      (push path args))
    ;; Reverse to get correct order and join
    (string-join (append (list "rg" "--color=never" "--line-number")
                         (reverse args))
                 " ")))


(defun agental-tool-search-tool (pattern &optional path file-regexp case-insensitive lines-after lines-before)
  "Search for PATTERN within the workspace using grep-like functionality.

PATTERN is the regular expression to search for (required).

PATH is the directory or file to search, relative to workspace root.
Defaults to workspace root if not provided.

FILE-REGEXP is a regular expression to filter files by path relative to
search path.

CASE-INSENSITIVE, if non-nil, performs a case-insensitive search (default: nil).

LINES-AFTER specifies number of lines to show after each match.

LINES-BEFORE specifies number of lines to show before each
match.

Returns formatted search results as a string. Considers workspace
context including any pending changes, creations, or deletions."
  (let* ((project-metadata (and agental--context
                                (agental-context-metadata-project-metadata agental--context)))
         (project-dir (cdr project-metadata))
         (search-path (if path
                          (if (file-name-absolute-p path)
                              (file-truename path)
                            (expand-file-name path project-dir))
                        project-dir)))
    (if (and search-path (file-exists-p search-path))
        (condition-case err
            (let* ((rg-command (agental-tools-ripgrep-search pattern
                                                             search-path
                                                             file-regexp
                                                             case-insensitive
                                                             lines-after
                                                             lines-before))
                   (output (with-temp-buffer
                             (when (zerop (call-process-shell-command rg-command nil t nil))
                               (let ((total-lines (count-lines (point-min) (point-max))))
                                 (concat (format "Match %d lines\n" total-lines)
                                         (save-excursion
                                           (goto-char (point-min))
                                           (let ((end-pos (save-excursion
                                                            (forward-line 99)
                                                            (point))))
                                             (buffer-substring-no-properties (point-min) end-pos)))))))))
              (if output
                  output
                "No matches found."))
          (error
           (error-message-string err)))
      (format "Path does not exist: %s" search-path))))

(gptel-make-tool
 :name "search_in_workspace"
 :function #'agental-tool-search-tool
 :description
 (concat
  "Search for patterns within the workspace using ripgrep\n")
 :confirm nil
 :include nil
 :args
 `((:name
    "pattern"
    :type string
    :description "Regular expression pattern to search for (required)")
   (:name
    "path"
    :type string
    :optional t
    :description "Directory or file to search, relative to workspace root (defaults to workspace root)")
   (:name
    "file_regexp"
    :type string
    :optional t
    :description
    ,(concat
      "Filter files by regular expression matched against file path relative to search path. "
      "Examples: '\\.py$' for Python files, 'src/.*\\.js$' for JS files in src/, "
      "'test.*\\.py$' for test files"))
   (:name
    "case_insensitive"
    :type boolean
    :optional t
    :description "If true, perform case-insensitive search (default: false)")
   (:name
    "lines_after"
    :type number
    :optional t
    :description "Number of lines to show after each match.")
   (:name
    "lines_before"
    :type number
    :optional t
    :description "Number of lines to show before each match.")))

;;; edit tools
(defun agental-tool--generate-patch-diff (path orig-content new-content)
  "Generate a unified diff patch comparing ORIG-CONTENT and NEW-CONTENT for PATH.

PATH is the file path for which to generate the diff.  ORIG-CONTENT is
the original file content, which may be nil for new files.  NEW-CONTENT
is the modified file content, which may be nil for deleted files.

Return a unified diff string showing the changes between ORIG-CONTENT
and NEW-CONTENT, or nil if the file has not changed.  The diff uses
git-style headers and labels to properly handle file creations and
deletions."
  (let* (;; Get the path relative to the base directory.
         (rel-path (file-truename path))
         ;; Check if file has actually changed.
         (file-changed-p (not (equal orig-content new-content)))
         result)

    ;; Only generate diff if the file has actually changed.
    (when file-changed-p
      (let ((temp-orig (make-temp-file "gptel-diff-orig"))
            (temp-new (make-temp-file "gptel-diff-new")))

        ;; Write original content (or empty file for new files).
        (with-temp-buffer
          (when orig-content
            (insert orig-content))
          (write-region (point-min) (point-max) temp-orig nil 'silent))

        ;; Write new content or create empty file for deletions.
        (with-temp-buffer
          (when new-content
            (insert new-content))
          (write-region (point-min) (point-max) temp-new nil 'silent))

        ;; Generate diff and append to result.
        (with-temp-buffer
          ;; Add the standard git diff header, which allows diff-mode to create new files.
          (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))

          ;; Use diff to generate a unified patch with the correct file path.
          (when (or orig-content new-content)
            (call-process "diff"
                          nil t nil "-u" "--label"
                          (if orig-content
                              (concat "a/" rel-path)
                            ;; Use /dev/null to denote file creations.
                            "/dev/null")
                          "--label"
                          (if new-content
                              (concat "b/" rel-path)
                            ;; Use /dev/null to denote file deletions.
                            "/dev/null")
                          temp-orig temp-new))

          ;; Append the diff to the result.
          (setq result (concat result (buffer-string))))

        ;; Clean up the temp files.
        (delete-file temp-orig)
        (delete-file temp-new)))
    result))


(defun agental-tool--edit-string (content old-string new-string &optional replace-all)
  "In CONTENT string, replace OLD-STRING with NEW-STRING.

If REPLACE-ALL is non-nil, replace all occurrences. Otherwise, error if
multiple matches exist and replace only single occurrences.

Return the new content string if the replacement was successful, or signal
an error if it was not."
  (let ((case-fold-search nil))
    ;; Error if old-string and new-string are identical
    (when (string-equal old-string new-string)
      (error "No changes to make: old_string and new_string are exactly the same"))
    ;; Handle empty old-string specially.
    (if (string-empty-p old-string)
        (if (string-empty-p content)
            ;; If content is empty, return the new string.
            new-string
          ;; If content is not empty and old-string is empty, throw an error.
          (error "Cannot replace empty string in non-empty content"))
      ;; Normal case: old-string is not empty.
      (let* ((start 0)
             (matches 0)
             (match-positions '()))
        ;; Count matches and collect positions.
        (while (setq start (string-search old-string content start))
          (setq matches (1+ matches))
          (push start match-positions)
          (setq start (+ start (length old-string))))

        (cond
         ((= matches 0)
          (error "String to replace not found in file"))
         ((and (> matches 1) (not replace-all))
          (error
           (concat
            "Found %d matches of the string to replace, but replace_all is false. "
            "To replace all occurrences, set replace_all to true. To replace only one "
            "occurrence, please provide more context to uniquely identify the instance")
           matches))
         (t
          ;; Perform replacement(s)
          (if replace-all
              ;; Replace all occurrences (work backwards to preserve positions)
              (let ((result content))
                (dolist (pos (sort match-positions '>))
                  (setq result
                        (concat
                         (substring result 0 pos)
                         new-string
                         (substring result (+ pos (length old-string))))))
                result)
            ;; Replace single occurrence
            (let ((match-pos (car (reverse match-positions))))
              (concat
               (substring content 0 match-pos)
               new-string
               (substring content (+ match-pos (length old-string))))))))))))


(defun agental-tool--edit-file (path edits)
  "Edit file in PATH with new content.

PATH is the path to the file.

EDITS is a vector of edit operations, each containing :old_string and
:new_string. For compatibility with LLMs that don't support array arguments, a
JSON string representing an array is also accepted.

All edits are applied in sequence to the same file. Each edit requires exact
whitespace matching. If any edit fails, the entire operation fails.

Returns (content . new-content) on success. Signals an error if the file is not
found or if the edit operation fails."
  (unless (vectorp edits)
    (if (stringp edits)
        ;; Try to decode JSON string to vector.
        (condition-case nil
            (let ((decoded (json-parse-string edits :array-type 'vector :object-type 'plist)))
              (if (vectorp decoded)
                  (setq edits decoded)
                (error
                 "The 'edits' parameter must be an array, but the decoded JSON is not an array")))
          (error
           (error
            "The 'edits' parameter must be an array of objects, or a valid JSON string representing an array")))
      ;; Not a vector or string - invalid input.
      (error "The 'edits' parameter must be an array of objects, not %s" (type-of edits))))
  (let ((full-path (file-truename path)))
    (if (file-exists-p full-path)
        (when-let* ((content (with-temp-buffer (insert-file-contents full-path) (buffer-string)))
                    (new-content content))
          (cl-loop
           for edit across edits do
           (let ((old-text (plist-get edit :old_text))
                 (new-text (plist-get edit :new_text))
                 (replace-all (plist-get edit :replace_all)))

             (unless (and old-text new-text)
               (error
                "Each edit must contain old_text and new_text properties"))
             ;; Handle :json-false inputs for replace-all parameter.
             (setq replace-all
                   (and replace-all (not (eq replace-all :json-false))))
             (setq new-content
                   (agental-tool--edit-string new-content old-text new-text
                                              replace-all))))
          (cons content new-content))
      (error "The '%s' file not find" path))))

(defun agental-tool-edit-file-tool (path edits)
  "Edit file in PATH with new content.

PATH is the path to the file.

EDITS is a vector of edit operations, each containing :old_string and
:new_string. For compatibility with LLMs that don't support array
arguments, a JSON string representing an array is also accepted.

All edits are applied in sequence to the same file. Each edit requires
exact whitespace matching. If any edit fails, the entire operation
fails.

Returns nil on success. Signals an error if the file is not found or if the edit
operation fails."
  (condition-case err
      (let* ((project-metadata (and agental--context
                                    (agental-context-metadata-project-metadata agental--context)))
             (project-dir (cdr project-metadata))
             (full-path (if (and project-dir (not (file-name-absolute-p path)))
                            (expand-file-name path project-dir)
                          (file-truename path))))
        (pcase-let* ((`(,content . ,new-content) (agental-tool--edit-file full-path edits)))
          ;; generate diff buffer
          (let ((diff-buffer (get-buffer-create (format "*Diff for %s*" path)))
                (diff-text (agental-tool--generate-patch-diff path content new-content)))
            (if diff-text
                (progn
                  (with-current-buffer diff-buffer
                    (let ((was-read-only buffer-read-only))
                      ;; Temporarily disable read-only mode to update the buffer.
                      (when was-read-only
                        (read-only-mode -1))
                      (erase-buffer)
                      (insert diff-text)
                      (when was-read-only
                        (read-only-mode 1)))

                    (diff-mode)

                    ;; Move to the beginning of the buffer.
                    (goto-char (point-min)))

                  (display-buffer diff-buffer
                                  '(display-buffer-reuse-window
                                    (body-function . select-window)))

                  "Generate diff success.")
              "Generate diff buffer error, new content same with orign content"))))
    (error
     (error-message-string err))))

(gptel-make-tool
 :name "edit_file_in_workspace"
 :function #'agental-tool-edit-file-tool
 :description
 (concat
  "Make multiple exact string replacements in a single file. "
  "Edits are applied sequentially in array order to the same file. "
  "Each edit requires exact whitespace matching. Do NOT include line numbers in old_text or new_text. "
  "If any edit fails, no changes are made. Returns null on success.")
 :confirm nil
 :include nil
 :args
 `((:name "path" :type string :description "Path to the file in workspace")
   (:name
    "edits"
    :type array
    :description "Array of edit operations to apply in sequence"
    :items
    (:type
     object
     :properties
     (:old_text
      (:type
       string
       :description
       ,(concat
         "Exact text to find and replace. Must match precisely including whitespace "
         "and newlines. Do NOT include line numbers."))
      :new_text (:type string :description "Text to replace the old_text with")
      :replace_all
      (:type
       boolean
       :description "If true, replace all occurrences. If false (default), error if multiple matches exist"))
     :required ["old_text" "new_text"])))
 :category "agental")

(provide 'agental-tool)
;;; agental-tool.el ends here
