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

;;; cursor
(require 'which-func)

(defun agental-context--get-surrounding-chars-pos-with-cursor (&optional n-chars)
  "Get content with BUFFER cursor.
N-CHARS is max size."
  (let* ((n (or n-chars 500))
         (pt (point))
         (buf-min (point-min))
         (buf-max (point-max))
         (start-pos (max buf-min (- pt n)))
         (end-pos   (min buf-max (+ pt n))))
    (cons start-pos end-pos)))

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
         (prompts (list (if thing
                            (format "Cursor point (%s)" thing)
                          "")
                        (if function
                            (format "Cursor function or Heading (%s)" function)
                          ""))))
    (when prompts
      (format "[%s]"
              (string-join prompts "|")))))

(defun agental-context-buffer-content ()
  "Get cursor content."
  (when-let* ((bounds (if (use-region-p)
                          (cons (region-beginning)
                                (region-end))
                        (agental-context--get-surrounding-chars-pos-with-cursor))))
    (list (current-buffer) :bounds (list bounds))))

(provide 'agental-context)
;;; agental-context.el ends here
