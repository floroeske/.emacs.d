(defun custom/insert-new-line ()
  "Insert new line without breaking the current one."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun custom/toggle-line-comment ()
  "Comment or uncomment current line or the ones covered by a marked region."
  (interactive)
  (let
   ((beg (car (custom/get-region-positions)))
    (end (cdr (custom/get-region-positions))))
   (comment-or-uncomment-region beg end)))

(defun custom/get-region-positions ()
  "Return a dotted-pair (BEG . END) with regions's beginning and ending positions."
  (interactive)
  (save-excursion
    (let (beg end)
      (if (and mark-active (> (point) (mark)))
          (exchange-point-and-mark))
      (setq beg (line-beginning-position))
      (if mark-active
          (exchange-point-and-mark))
      (setq end (line-end-position))
      (cons beg end))))

(defun custom/smart-move-beginning-of-line ()
  "Move to beginning of line or to beginning of indentation depending on POINT."
  (interactive)
  (if (= (point) (line-beginning-position))
      (back-to-indentation)
    (move-beginning-of-line nil)))

(defun custom/duplicate-current-line-or-region (arg)
  "Duplicates the current line or those covered by region ARG times."
  (interactive "p")
  (let (beg end exit-point)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark)))
  (setq beg (line-beginning-position))
  (if mark-active
      (exchange-point-and-mark))
  (setq end (line-end-position))
  (setq exit-point end)
  (let ((region (buffer-substring-no-properties beg end)))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char exit-point)
    (next-line)
    (back-to-indentation)))

(defun custom/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun custom/hsplit-last-buffer ()
  "Horizontally split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun custom/vsplit-last-buffer ()
  "Vertically split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun custom/swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun custom/switch-fullscreen nil
  "Switch to fullscreen."
  (interactive)
  (let* ((modes '(nil fullboth fullwidth fullheight))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))

(defun custom/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun custom/rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun custom/camelcase-to-snakecase ()
  "Switch thisName to this-name."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(defun custom/camelcase-region (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

(defun custom/camelcase-word-or-region ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (custom/camelcase-region pos1 pos2)))

(defun custom/snakecase-region (start end)
  "Changes region from camelCase to snake_case"
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

(defun custom/snakecase-word-or-region ()
  "Changes word or region from camelCase to snake_case"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (custom/snakecase-region pos1 pos2)))

(defun custom/toggle-camelcase-snakecase ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun custom/replace-word-in-buffer ()
  "print current word."
  (interactive)
  (save-excursion
    (setq column (current-column))
    (setq line (current-line))
    (setq old-string (thing-at-point 'word))
    (setq old-string (read-string "Replace string: " old-string))
    (setq new-string (read-string "With string: " old-string))
    (goto-line (point-min))
    (query-replace old-string new-string nil)
    (goto-line line)
    (forward-char column)))

(defun custom/qrc (replace-str)
  (interactive "sDo query-replace current word with: ")
  (forward-word)
  (let ((end (point)))
    (backward-word)
    (kill-ring-save (point) end)
    (query-replace (current-kill 0) replace-str)  nil (point-min) (point-max)))

(defun custom/replace-in-buffer ()
  "Replace text in whole buffer. The suggested OLD text is either the current region,
  or the next word (as mark-word would select it). The suggested text for the
  replacement is the same as the OLD text."
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setq curr-word (buffer-substring-no-properties (mark) (point)))
    (setq old-string (read-string "OLD string:\n" curr-word))
    (setq new-string (read-string "NEW string:\n" old-string))
    (query-replace old-string new-string nil (point-min) (point-max))))

(defun custom/toggle-initials ()
  "Toggle first character case."
  (interactive)
  (save-excursion
    (backward-to-word 1)
    (forward-to-word 1)
    (setq old-string (thing-at-point 'word))
    (kill-word)
    (setq new-string (upcase-initials old-string))
    (insert new-string)))

(defmacro custom/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "Execution took %.06f" (float-time (time-since time)))))

(defun custom/create-list (prefix postfix start end)
  "Create a numbered list of strings"
  (interactive "sPrefix: \nsPostfix: \nnStart: \nnEnd: ")
  (dolist (ind (number-sequence start end))
    (insert (format "%s%d%s\n" prefix ind postfix))))

(defun custom/shell-add-path (new-path)
  "Append path to PATH variable"
  (interactive "DNew path: ")
  (setenv "PATH"
          (concat (getenv "PATH") ":" new-path)))

(defun custom/shell-source (filename)
  "Update environment variables from a shell source file."
  (interactive "FSource file: ")

  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer

    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))

    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))

      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))

  (message "Sourcing environment from `%s'... done." filename))

(defun custom/new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (let (buffer-name)
    (setq buffer-name (cl-loop for num from 0
                            for name = (format "*scratch-%03i*" num)
                            while (get-buffer name)
                            finally return name))
    (switch-to-buffer buffer-name)
    (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.\n")
    (insert ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n")
    (insert "\n")
    (lisp-interaction-mode)))

(defun custom/new-note ()
  "open up a guaranteed new note buffer"
  (interactive)
  (let (buffer-name)
    (setq buffer-name (cl-loop for num from 0
                            for name = (format "*note-%03i*" num)
                            while (get-buffer name)
                            finally return name))
    (switch-to-buffer buffer-name)
    (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
    (insert "\n")
    (insert "\n")
    (goto-char (point-max))
    (org-mode)))

(defun custom/server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun custom/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun custom/run-command (command)
  "Dmenu/rofi-like run external commands. Press C-M-i or Tab to auto complete"
  (interactive
   (list (read-shell-command "run: ")))
  (start-process-shell-command command nil command))

(defun custom/shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))

(defun custom/esh-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (eshell-bol))
           (end-pos (point-at-eol))
           (input (buffer-substring-no-properties start-pos end-pos)))
      (let* ((command (ivy-read "Command: "
                                (delete-dups
                                 (when (> (ring-size eshell-history-ring) 0)
                                   (ring-elements eshell-history-ring)))
                                :preselect input
                                :action #'ivy-completion-in-region-action))
             (cursor-move (length command)))
        (kill-region (+ start-pos cursor-move) (+ end-pos cursor-move))
        )))
  ;; move cursor to eol
  (end-of-line)
  )

(defun custom/occur-word-or-region ()
  "Occur for word or region"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (occur (buffer-substring-no-properties  pos1 pos2))))

(defun custom/vertical-splitting ()
  "Adjust splitting for vertical screen"
  (interactive)
  (setq split-height-threshold 200)
  (setq split-width-threshold nil))

(defun custom/horizontal-splitting ()
  "Adjust splitting for horizontal screen"
  (interactive)
  (setq split-height-threshold nil)
  (setq split-width-threshold 200))

(defun insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))
(defun datetime ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun filename-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
