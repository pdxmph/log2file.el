;;; log2file.el --- Simple file-format-aware capture system -*- lexical-binding: t -*-

;; Author: Mike
;; Version: 0.7
;; Package-Requires: ((emacs "27.1"))
;; Keywords: markdown, org, convenience, capture
;; URL: https://github.com/pdxmph/log2file

;;; Commentary:
;; A lightweight capture system for Org and Markdown files.
;; Creates a transient buffer for writing a timestamped post and saves it to a
;; target file without showing the rest of the file during capture.
;;
;; Modified to display target file titles instead of filenames in the prompt.

;;; Code:
(require 'subr-x)  ;; For string-trim


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

(defcustom log2file-cli-directory "~/notes/topics/"
  "Default directory for CLI log2file commands.
This is prepended to CLI-provided filenames."
  :type 'directory
  :group 'log2file)

(defcustom log2file-split-size 12
  "Number of lines high for the capture window split."
  :type 'integer
  :group 'log2file)

(defvar log2file--destination nil
  "Internal variable storing the destination file during a capture session.")

;;;###autoload
(define-minor-mode log2file-mode
  "Minor mode for log2file capture buffers."
  :lighter " >> Capture"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'log2file-finalize)
    (define-key map (kbd "C-c C-k") #'log2file-abort)
    map))

;;;###autoload
(defun log2file-abort ()
  "Abort the current capture session without saving."
  (interactive)
  (log2file--cleanup)
  (message "Capture aborted."))

(defun log2file--format-type (file)
  "Return symbol 'org or 'markdown based on FILE extension."
  (cond ((string-match-p "\\.org\\'" file)      'org)
        ((string-match-p "\\.md\\'"  file)      'markdown)
        (t (user-error "Unsupported file type: %s" file))))

(defun log2file--format-entry (file title)
  "Return a formatted entry string for FILE with TITLE."
  (let ((ts (format-time-string "%Y-%m-%d %a %H:%M")))
    (pcase (log2file--format-type file)
      ('org      (format "* [%s] %s " ts title))
      ('markdown (format "## [%s] %s " ts title)))))

(defun log2file--get-title (file)
  "Extract and return the TITLE from FILE.
Checks for Org #+TITLE:, Markdown # header, or YAML frontmatter title:.
Falls back to the file's basename if no title is found."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 4096)
    (goto-char (point-min))
    (let ((type (log2file--format-type file))
          title)
      (pcase type
        ('org
         (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.+\\)" nil t)
           (setq title (match-string 1))))
        ('markdown
         ;; Check for YAML frontmatter
         (goto-char (point-min))
         (when (looking-at "---")
           (forward-line 1)
           (let ((frontmatter-end (save-excursion
                                    (search-forward "---" nil t))))
             (when frontmatter-end
               (when (re-search-forward "^title:[ \t]*\\(.+\\)" frontmatter-end t)
                 (setq title (string-trim (match-string 1)))))))
         ;; If no title found in frontmatter, look for H1
         (unless title
           (goto-char (point-min))
           (when (re-search-forward "^# \\(.+\\)" nil t)
             (setq title (match-string 1)))))
        (_ nil))
      (or title (file-name-base file)))))

;;;###autoload
(defun log2file-add-current-file ()
  "Add the current file to `log2file-targets`."
  (interactive)
  (let ((p (buffer-file-name)))
    (unless p (user-error "Not visiting a file"))
    (unless (member p log2file-targets)
      (customize-save-variable 'log2file-targets (add-to-list 'log2file-targets p))
      (message "Added %s" p))))

;;;###autoload
(defun log2file-remove-current-file ()
  "Remove the current file from `log2file-targets`."
  (interactive)
  (let ((p (buffer-file-name)))
    (unless p (user-error "Not visiting a file"))
    (when (member p log2file-targets)
      (customize-save-variable 'log2file-targets (delete p log2file-targets))
      (message "Removed %s" p))))

;;;###autoload
(defun log2file ()
  "Start a capture session for Org or Markdown files, selecting target by title."
  (interactive)
  (let* ((files
          (cond
           (log2file-targets log2file-targets)
           (log2file-dir
            (directory-files-recursively log2file-dir "\\.\\(?:org\\|md\\)\\'"))
           (t (user-error "No targets or directory defined"))))
         (choices
          (mapcar (lambda (f)
                    (cons (log2file--get-title f) f))
                  files))
         (dest
          (cdr (assoc
                (completing-read "Capture to (by title): " choices nil t)
                choices)))
         (ttl   (read-string "Title: "))
         (entry (log2file--format-entry dest ttl))
         (buf   (generate-new-buffer "*log2file*")))
    (split-window-below (- log2file-split-size))
    (other-window 1)
    (switch-to-buffer buf)
    (insert entry)
    (funcall (if (eq (log2file--format-type dest) 'org)
                 #'org-mode
               #'markdown-mode))
    (log2file-mode 1)
    (setq log2file--destination dest)
    (message "C-c C-c to save, C-c C-k to cancel.")))

;; Helper function for finding insert position in Markdown files
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

;; Helper function for finding insert position in Org files
(defun log2file--find-org-insert-position (file)
  "Find insert position in an Org FILE.
Returns the position to insert new entries."
  (let ((title-pos nil)
        (found-title nil))
    ;; Check for #+TITLE
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TITLE:" nil t)
        (progn
          (end-of-line)
          (forward-line 1)
          (setq title-pos (point))
          (setq found-title t))
      ;; If no #+TITLE, check for L1 heading
      (goto-char (point-min))
      (if (re-search-forward "^\\* " nil t)
          (progn
            (end-of-line)
            (forward-line 1)
            (setq title-pos (point))
            (setq found-title t))
        ;; No title found, add one
        (goto-char (point-min))
        (insert (format "#+TITLE: %s\n\n" (file-name-base file)))
        (setq title-pos (point))
        (setq found-title t)))
    (cons found-title title-pos)))

;;;###autoload
(defun log2file-finalize ()
  "Save and close the capture session."
  (interactive)
  (let ((cnt (string-trim-right (buffer-string)))
        (file log2file--destination)
        (capture-buffer (current-buffer)))

    ;; Insert content into the destination file
    (with-current-buffer (find-file-noselect file)
      (let* ((file-type (log2file--format-type file))
             (position-info
              (pcase file-type
                ('markdown (log2file--find-markdown-insert-position file))
                ('org (log2file--find-org-insert-position file))
                (_ (cons nil nil)))))

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

(defun log2file--cleanup ()
  "Close window and kill buffer."
  (when (and (window-live-p (selected-window))
             (not (one-window-p)))
    (delete-window))
  (kill-buffer (current-buffer)))

;;;###autoload
(defun log2file-cli-capture (fn ttl)
  "CLI: capture TTL to FN (expanded in `log2file-cli-directory`)."
  (let* ((file  (expand-file-name fn log2file-cli-directory))
         (entry (log2file--format-entry file ttl)))
    (unless (file-directory-p (file-name-directory file))
      (make-directory (file-name-directory file) :parents))
    (unless (file-exists-p file)
      (with-temp-buffer (write-file file)))
    (log2file--insert-entry file entry)
    (message "Logged to %s" file)))

;;;###autoload
(defun log2file-cli-edit (fn ttl)
  "CLI: open capture buffer for FN with TTL."
  (let* ((file (expand-file-name fn log2file-cli-directory))
         (head (log2file--format-entry file ttl))
         (buf  (generate-new-buffer "*log2file*")))
    (switch-to-buffer buf)
    (insert head)
    (funcall (if (eq (log2file--format-type file) 'org)
                 #'org-mode
               #'markdown-mode))
    (log2file-mode 1)
    (setq log2file--destination file)
    (message "Editing %s" file)))

;; Legacy function to maintain compatibility with other function calls
(defun log2file--insert-entry (file entry)
  "Insert ENTRY into FILE after frontmatter/title but before other headings.
This is a compatibility function that uses the new helper functions."
  (with-current-buffer (find-file-noselect file)
    (let* ((file-type (log2file--format-type file))
           (position-info
            (pcase file-type
              ('markdown (log2file--find-markdown-insert-position file))
              ('org (log2file--find-org-insert-position file))
              (_ (cons nil nil)))))

      ;; Now insert the entry at the right position
      (if (car position-info)
          (progn
            (goto-char (cdr position-info))
            ;; Ensure there's a blank line before our new entry if there's content
            (unless (looking-at "^$")
              (insert "\n"))
            (insert entry "\n\n")
            (save-buffer))
        (message "Something went wrong finding title position")))))

(provide 'log2file)
;;; log2file.el ends here
