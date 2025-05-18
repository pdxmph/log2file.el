;;; log2file-transient.el --- Transient-based capture system -*- lexical-binding: t -*-
;; Author: Mike
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))
;; Keywords: markdown, convenience, capture
;; URL: https://github.com/pdxmph/log2file

;;; Commentary:
;; A lightweight capture system for Markdown files.
;; Creates a transient buffer for writing a timestamped post and saves it to a
;; target file without showing the rest of the file during capture.
;;
;; Modified to display target file titles instead of filenames in the prompt.

;;; Code:
(require 'transient)
(require 'subr-x) ;; For string-trim

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

(defvar log2file--selected-file nil
  "Currently selected target file.")

(defvar log2file--entry-title nil
  "Title for the current entry.")

(defvar log2file--destination nil
  "Internal variable storing the destination file during a capture session.")

;; Helper function to extract title from file
(defun log2file--get-title (file)
  "Extract title from FILE, checking for frontmatter or headers."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 4096)
    (goto-char (point-min))
    (let (title)
      ;; Check for YAML frontmatter
      (when (looking-at "---")
        (forward-line 1)
        (let ((frontmatter-end (save-excursion
                                 (search-forward "---" nil t))))
          (when frontmatter-end
            (when (re-search-forward "^title:[ \t]*\\(.+\\)" frontmatter-end t)
              (setq title (string-trim (match-string 1)))))))

      ;; If no title in frontmatter, look for H1 header
      (unless title
        (goto-char (point-min))
        (when (re-search-forward "^# \\(.+\\)" nil t)
          (setq title (match-string 1))))

      (or title (file-name-base file)))))

;; Function to get available target files
(defun log2file--get-target-files ()
  "Return list of available target files."
  (cond
   (log2file-targets log2file-targets)
   (log2file-dir
    (directory-files-recursively log2file-dir "\\.md\\'"))
   (t nil)))

;; Function to select target file
(defun log2file-select-file ()
  "Select a target file for capture."
  (interactive)
  (let* ((files (log2file--get-target-files))
         (choices (mapcar (lambda (f)
                            (cons (log2file--get-title f) f))
                          files))
         (selected (completing-read "Select target: " choices nil t)))
    (setq log2file--selected-file (cdr (assoc selected choices)))
    (message "Selected: %s" (log2file--get-title log2file--selected-file))
    (transient-setup 'log2file-menu)))

;; Function to set entry title
(defun log2file-set-title ()
  "Set title for the capture entry."
  (interactive)
  (setq log2file--entry-title (read-string "Entry title: "))
  (message "Title set: %s" log2file--entry-title)
  (transient-setup 'log2file-menu))

;; Function to format an entry
(defun log2file--format-entry (file title)
  "Format entry for FILE with TITLE."
  (let ((ts (format-time-string "%Y-%m-%d %a %H:%M")))
    (format "## [%s] %s\n\n" ts title)))

;; Function to execute capture
(defun log2file-execute-capture ()
  "Execute the capture operation."
  (interactive)

  ;; Validate we have what we need
  (unless log2file--selected-file
    (user-error "No target file selected"))
  (unless log2file--entry-title
    (user-error "No title set for entry"))

  ;; Create capture buffer
  (let* ((file log2file--selected-file)
         (title log2file--entry-title)
         (entry (log2file--format-entry file title))
         (buf (generate-new-buffer "*log2file*")))

    (split-window-below -12)
    (other-window 1)
    (switch-to-buffer buf)
    (insert entry)
    (markdown-mode)
    (log2file-mode 1)
    (setq log2file--destination file)
    (message "C-c C-c to save, C-c C-k to cancel.")))

;; Define the transient menu
(transient-define-prefix log2file-menu ()
  "Log2File capture menu."
  :info-manual "(log2file)"

  ["Setup"
   ("f" "Select file" log2file-select-file
    :description
    (lambda ()
      (format "Select file [%s]"
              (if log2file--selected-file
                  (log2file--get-title log2file--selected-file)
                "none selected"))))
   ("t" "Set title" log2file-set-title
    :description
    (lambda ()
      (format "Set title [%s]"
              (or log2file--entry-title "none set"))))]

  ["Actions"
   ("c" "Capture now" log2file-execute-capture :if
    (lambda () (and log2file--selected-file log2file--entry-title)))])

;; The minor mode for editing
(define-minor-mode log2file-mode
  "Minor mode for log2file capture buffers."
  :lighter " >> Capture"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'log2file-finalize)
    (define-key map (kbd "C-c C-k") #'log2file-abort)
    map))

(defun log2file--find-markdown-insert-position (file)
  "Find insert position in a Markdown FILE.
Returns the position to insert new entries."
  (let ((title-pos nil)
        (found-title nil))
    ;; Check for YAML frontmatter
    (goto-char (point-min))
    (if (looking-at "---")
        (progn
          (forward-line 1) ;; Skip first ---
          (if (search-forward "---" nil t) ;; Find closing ---
              (progn
                (forward-line 1) ;; Move to line after closing ---
                (setq title-pos (point))
                (setq found-title t))))
      ;; If no frontmatter, check for level 1 heading
      (goto-char (point-min))
      (if (re-search-forward "^# " nil t)
          (progn
            (end-of-line)
            (forward-line 1)
            (setq title-pos (point))
            (setq found-title t))
        ;; No title found, add one
        (goto-char (point-min))
        (insert (format "# %s\n\n" (file-name-base file)))
        (setq title-pos (point))
        (setq found-title t)))
    (cons found-title title-pos)))

(defun log2file-finalize ()
  "Save and close the capture session."
  (interactive)
  (let ((cnt (string-trim-right (buffer-string)))
        (file log2file--destination)
        (capture-buffer (current-buffer)))

    ;; Insert content into the destination file
    (with-current-buffer (find-file-noselect file)
      (let* ((position-info (log2file--find-markdown-insert-position file)))
        ;; Now insert the entry at the right position
        (if (car position-info)
            (progn
              (goto-char (cdr position-info))
              ;; Ensure there's a blank line before our new entry if there's content
              (unless (looking-at "^$")
                (insert "\n"))
              (insert cnt "\n\n")
              (save-buffer))
          (message "Something went wrong finding title position"))))

    ;; Return to the capture buffer to clean up
    (with-current-buffer capture-buffer
      (log2file--cleanup))

    (message "Saved to %s" file)))

(defun log2file-abort ()
  "Abort the current capture session without saving."
  (interactive)
  (log2file--cleanup)
  (message "Capture aborted."))

(defun log2file--cleanup ()
  "Close window and kill buffer."
  (when (and (window-live-p (selected-window))
             (not (one-window-p)))
    (delete-window))
  (kill-buffer (current-buffer)))

;;;###autoload
(defun log2file ()
  "Start log2file capture process using transient UI."
  (interactive)
  (transient-setup 'log2file-menu))

(provide 'log2file-transient)
;;; log2file-transient.el ends here
