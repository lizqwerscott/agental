;;; agental.el --- agental                           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Version: 0.0.1
;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9") (yaml "1.2.0"))
;; Keywords: tools
;; URL: https://github.com/lizqwerscott/agental

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

(require 'gptel)

(require 'agental-context)

(defgroup agental nil
  "Agental."
  :group 'agental)

(defun agental--send (workspace-context)
  "Submit this prompt to the current LLM backend.

WORKSPACE-CONTEXT is now workspace content.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response."
  (let ((fsm (gptel-make-fsm :handlers gptel-send--handlers))
        (gptel-use-context 'user))
    (gptel-request nil
      :stream gptel-stream
      :transforms (append gptel-prompt-transform-functions
                          (list
                           (lambda (callback fsm)
                             (agental-context--transform-add-context workspace-context callback fsm))))
      :fsm fsm)
    (message "Querying %s..."
             (thread-first (gptel-fsm-info fsm)
                           (plist-get :backend)
                           (or gptel-backend)
                           (gptel-backend-name))))
  (gptel--update-status " Waiting..." 'warning))


(defun agental--create-buffer (buffer-name prompt context)
  "Create or switch to a GPT session buffer and initialize it.

BUFFER-NAME is the name of the target buffer.
PROMPT is the prompt text to insert during initialization.
CONTEXT is the context.

This function performs the following operations:
1. Create or switch to the buffer with the given name
2. Set the major mode according to =gptel-default-mode'
3. Validate and sanitize model configuration using =gptel--sanitize-model'
4. Insert the prompt text at the end of the buffer
5. Display the buffer using =gptel-display-buffer-action'
6. Enable =gptel-mode' if not already active
7. Send the initial request via =gptel-send'

If the buffer is empty, the prompt is prefixed with \"*** \". If the buffer
already contains content, the prompt is appended at the end."
  (with-current-buffer (get-buffer-create buffer-name)
    (cond                               ;Set major mode
     ((eq major-mode gptel-default-mode))
     ((eq gptel-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall gptel-default-mode)))
    (gptel--sanitize-model :backend (default-value 'gptel-backend)
                           :model (default-value 'gptel-model)
                           :shoosh nil)
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (if (bobp)
        (insert (concat "*** " prompt))
      (goto-char (point-max))
      (insert prompt))
    (display-buffer (current-buffer) gptel-display-buffer-action)
    (unless gptel-mode (gptel-mode 1))
    (agental--send context)))

;;;###autoload
(defun agental-global-chat ()
  "Global chat."
  (interactive)
  (let* ((completion-extra-properties
          `(:annotation-function
            ,(lambda (comp)
               (and-let* ((desc
                           (plist-get (gptel-get-preset (intern-soft comp))
                                      :description)))
                 (concat (propertize " " 'display '(space :align-to 32))
                         (if (string-match "\\(\n\\)" desc)
                             (substring desc 0 (match-beginning 1))
                           desc))))))
         (preset (completing-read "Select main preset: " gptel--known-presets nil t))
         (cursor-context (agental-context-cursor-prompt))
         (message (read-string (format "%s\nInput: @%s "
                                       (propertize cursor-context
                                                   'face 'font-lock-comment-face)
                                       preset)
                               nil nil nil t))
         (gptel-display-buffer-action '(display-buffer-reuse-window
                                        (body-function . select-window)))
         (buffer-name "*global-chat*")
         (prompt (format "@%s %s"
                         preset
                         message))
         (workspace-context (agental-context-workspace-content)))
    (agental--create-buffer buffer-name prompt workspace-context)))

;;;###autoload
(defun agental-project-chat ()
  "Global chat in project."
  (interactive)
  (when-let* ((project (project-current))
              (name (project-name project))
              (root-dir (file-truename (project-root project)))
              (completion-extra-properties
               `(:annotation-function
                 ,(lambda (comp)
                    (and-let* ((desc
                                (plist-get (gptel-get-preset (intern-soft comp))
                                           :description)))
                      (concat (propertize " " 'display '(space :align-to 32))
                              (if (string-match "\\(\n\\)" desc)
                                  (substring desc 0 (match-beginning 1))
                                desc))))))
              (preset (completing-read "Select Project preset: " gptel--known-presets nil t))
              (cursor-context (agental-context-cursor-prompt))
              (message (read-string (format "%s\nInput: @%s "
                                            (propertize cursor-context
                                                        'face 'font-lock-comment-face)
                                            preset) nil nil t))
              (gptel-display-buffer-action '(display-buffer-reuse-window
                                             (body-function . select-window)))
              (buffer-name (format "*%s-chat %s*" name root-dir))
              (prompt (format "@%s %s%s"
                              preset
                              message
                              (concat " " cursor-context)))
              (workspace-context (agental-context-workspace-content)))
    (agental--create-buffer buffer-name prompt workspace-context)))

(provide 'agental)
;;; agental.el ends here
