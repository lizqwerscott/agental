;;; agental-context.el --- agental context           -*- lexical-binding: t; -*-

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

(require 'project)
(require 'json)

(require 'gptel)
(require 'gptel-context)

;;; cursor
(require 'which-func)

(defcustom agental-context-context-window-size 2000
  "Maximum number of characters to capture around point as the context window.
This value determines how much surrounding text the system will read when
extracting contextual information."
  :type 'number
  :group 'agental)

(defun agental-context--cursor-context-range (n-chars)
  "Get content with BUFFER cursor.
N-CHARS is max size."
  (when (> n-chars 0)
    (let* ((n n-chars)
           (pt (point))
           (buf-min (point-min))
           (buf-max (point-max))
           (start-pos (max buf-min (- pt n)))
           (end-pos   (min buf-max (+ pt n))))
      (cons start-pos end-pos))))

(defun agental-context--symbol ()
  "Get cursor thing."
  (when-let* ((thing (or (thing-at-point 'url)
                         (thing-at-point 'existing-filename)
                         (thing-at-point 'filename)
                         (thing-at-point 'symbol)))
              (thing (substring-no-properties thing)))
    thing))

(declare-function breadcrumb-imenu-crumbs "breadcrumb")

(defun agental-context--function ()
  "Get cursor function."
  (if (not (featurep 'breadcrumb))
      (which-function)
    (when-let* ((imenu-crumbs (breadcrumb-imenu-crumbs))
                (func (substring-no-properties imenu-crumbs)))
      func)))

(defun agental-context-cursor-prompt ()
  "Get cursor prompt."
  (let* ((thing (agental-context--symbol))
         (function (agental-context--function))
         (name (buffer-name (current-buffer)))
         (path (buffer-file-name (current-buffer)))
         (prompts (list (if thing
                            (format "Cursor point (%s)" thing)
                          "")
                        (if function
                            (format "Cursor function or Heading (%s)" function)
                          "")
                        (format "Cursor buffer name (%s)" name)
                        (if path
                            (format "Cursor file path (%s)" (file-truename path))
                          ""))))
    (when prompts
      (format "[%s]"
              (string-join prompts "|")))))

(defun agental-context-buffer-content ()
  "Return a list describing the buffer content at cursor position or region.

If a region is active, return a list containing the current buffer and a
:bounds property with the region boundaries.  Otherwise, return a list
containing the current buffer and a :bounds property with the boundaries
of text surrounding the cursor, as determined by
`agental-context--cursor-context-range'.

The returned list has the form (BUFFER :bounds ((START . END))), where
BUFFER is the current buffer object, START is the beginning position, and
END is the ending position."
  (when-let* ((bounds (if (use-region-p)
                          (cons (region-beginning)
                                (region-end))
                        (agental-context--cursor-context-range agental-context-context-window-size))))
    (list (current-buffer) :bounds (list bounds))))

(defun agental-context-project-content ()
  "Get current project metadata."
  (when-let* ((project (project-current))
              (root-dir (project-root project))
              (name (project-name project)))
    (cons name root-dir)))

(defun agental-context-workspace-content ()
  "Return a compact workspace context string (JSON metadata + code snippet)."
  (let* ((project-metadata (agental-context-project-content))

         (buf (current-buffer))
         (name (buffer-name buf))
         (path (buffer-file-name buf))
         (rel-path (when path
                     (file-relative-name path
                                         (or (cdr project-metadata)
                                             default-directory))))
         (lang major-mode)
         (line (line-number-at-pos))
         (thing (agental-context--symbol))
         (function-or-heading (agental-context--function))

         (cursor-buffer-context (agental-context-buffer-content))
         (buffer (car cursor-buffer-context))
         (bounds (car (plist-get (cdr cursor-buffer-context) :bounds)))
         (buffer-content (with-current-buffer buffer
                           (buffer-substring-no-properties (car bounds) (cdr bounds))))

         (meta (json-encode `(("project" . ,(or (car project-metadata) "unknown"))
                              ("file" . ,(or rel-path name))
                              ("abs_path" . ,(or path ""))
                              ("lang" . ,lang)
                              ("cursor_line" . ,line)
                              ("function_or_heading" . ,(or function-or-heading ""))
                              ("symbol" . ,(or thing ""))))))
    (with-temp-buffer
      (insert (format "[METADATA] %s\n" meta))
      (insert "=======================================================\n")
      (insert "WORKSPACE CONTEXT:\n")
      (insert (format "Buffer: %s\n" name))
      (when path (insert (format "Abs path: %s\n" (file-truename path))))
      (insert (format "Cursor line: %s\n" line))
      (insert (format "Function or Heading: %s\n\n" (or function-or-heading "")))
      (when (not (string= "" buffer-content))
        (insert (format "Cursor surrounding snippet(±%d chars):\n" (/ agental-context-context-window-size 2)))
        (insert buffer-content)
        (insert "\n"))
      (insert "=======================================================\n")
      (buffer-string))))

(defun agental-context--transform-add-context (content callback fsm)
  "A gptel prompt transformer to add context from the current workspace.

CONTENT is need add content.

CALLBACK and FSM are as described in the
`gptel-prompt-transform-functions' documentation.

Adds the CONTENT to the prompt,
in the same place as the default gptel context as specified by
`gptel-use-context'."
  (when-let* (
              ;; plist containing information about the upcoming request.
              (info (gptel-fsm-info fsm))
              ;; Buffer where the request is being sent.
              (buffer (plist-get info :buffer))
              (_ (buffer-live-p buffer))
              (workspace-string content))
    (gptel-context--wrap-in-buffer workspace-string))
  (funcall callback))

(provide 'agental-context)
;;; agental-context.el ends here
