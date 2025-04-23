;;; log2file.el --- Simple file-format-aware capture system -*- lexical-binding: t; -*-

;; Author: Mike
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: markdown, org, convenience, capture
;; URL: https://github.com/pdxmph/log2file

;;; Commentary:
;;
;; A lightweight capture system for Org and Markdown files.
;; Creates a transient buffer for writing a timestamped post and saves it to a
;; target file without showing the rest of the file during capture.

;;; Code:

(defgroup log2file nil
  "Multi-format capture system."
  :group 'convenience
  :prefix "log2file-")

(defcustom log2file-targets nil
  "List of files to use as capture targets."
  :type '(repeat file))

(defcustom log2file-dir nil
  "Directory from which to use all *.org or *.md files as capture targets."
  :type 'directory)

(defvar log2file--destination nil
  "Internal variable to store the destination file during a capture session.")

(define-minor-mode log2file-mode
  "Minor mode for log2file buffers."
  :lighter " >> Capture"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'log2file-finalize)
            (define-key map (kbd "C-c C-k") #'log2file-abort)
            map))

(defun log2file--format-type (file)
  "Return symbol `org` or `markdown` based on file extension."
  (cond
   ((string-match-p "\\.org\\'" file) 'org)
   ((string-match-p "\\.md\\'" file) 'markdown)
   (t (user-error "Unsupported file type: %s" file))))

(defun log2file--format-entry (file title)
  "Return a formatted entry string based on file type."
  (let ((timestamp (format-time-string "%Y-%m-%d %a %H:%M")))
    (pcase (log2file--format-type file)
      ('org (format "* [%s] %s\n\n" timestamp title))
      ('markdown (format "## [%s] %s\n\n" timestamp title)))))

(defun log2file-add-current-file ()
  "Add the current file to `log2file-targets`."
  (interactive)
  (let ((path (buffer-file-name)))
    (unless path
      (user-error "This buffer is not visiting a file"))
    (unless (member path log2file-targets)
      (customize-save-variable
       'log2file-targets
       (add-to-list 'log2file-targets path))
      (message "Added %s to log2file-targets" path))))

(defun log2file-remove-current-file ()
  "Remove the current file from `log2file-targets`."
  (interactive)
  (let ((path (buffer-file-name)))
    (unless path
      (user-error "This buffer is not visiting a file"))
    (when (member path log2file-targets)
      (customize-save-variable
       'log2file-targets
       (setq log2file-targets (delete path log2file-targets)))
      (message "Removed %s from log2file-targets" path))))

(defun log2file ()
  "Start a capture session for Org or Markdown files."
  (interactive)
  (let* ((targets (cond
                   (log2file-targets log2file-targets)
                   (log2file-dir (directory-files log2file-dir t "\\.\(org\|md\)$"))
                   (t (user-error "No log2file targets or directory defined."))))
         (target (completing-read "Capture to: " targets nil t))
         (title (read-string "Log entry title: "))
         (entry-header (log2file--format-entry target title))
         (capture-buffer (generate-new-buffer "*log2file*")))
    (split-window-below -12)
    (other-window 1)
    (switch-to-buffer capture-buffer)
    (insert entry-header)
    (funcall (if (eq (log2file--format-type target) 'org) #'org-mode #'markdown-mode))
    (log2file-mode 1)
    (setq log2file--destination target)
    (message "Write your log. C-c C-c to save, C-c C-k to cancel.")))

(defun log2file-finalize ()
  "Save the capture buffer's contents to the target file."
  (interactive)
  (let ((content (string-trim-right (buffer-string)))
        (dest log2file--destination)
        (type (log2file--format-type log2file--destination)))
    (with-current-buffer (find-file-noselect dest)
      (goto-char (point-min))
      ;; Ensure top-of-file title
      (pcase type
        ('markdown
         (unless (looking-at "^# ")
           (insert (format "# %s\n\n" (file-name-base dest)))))
        ('org
         (unless (re-search-forward "^#\\+TITLE:" nil t)
           (goto-char (point-min))
           (insert (format "#+TITLE: %s\n\n" (file-name-base dest))))))

      (goto-char (point-min))
      (if (re-search-forward (if (eq type 'org) "^\\* " "^## ") nil t)
          (progn
            (beginning-of-line)
            (message "Inserting above first heading at position %d" (point))
            (while (and (not (bobp))
                        (save-excursion (forward-line -1) (looking-at "^\\s-*$")))
              (forward-line -1)
              (delete-blank-lines))
            (insert "\n" content "\n\n"))
        (goto-char (point-max))
        (message "No heading found; appending to end at position %d" (point))
        (insert "\n" content "\n\n"))
      (save-buffer))
    (log2file--cleanup)
    (message "Capture saved to %s." log2file--destination)))

(defun log2file-abort ()
  "Cancel the current capture session."
  (interactive)
  (when (y-or-n-p "Abort this capture? ")
    (log2file--cleanup)
    (message "Capture aborted.")))

(defun log2file--cleanup ()
  "Close and kill the capture buffer."
  (let ((buf (current-buffer)))
    (kill-buffer buf)
    (delete-window)))

(provide 'log2file)
;;; log2file.el ends here
