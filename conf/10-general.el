(setq
  ;; If a frame alredy opened, use it!
  display-buffer-reuse-frames t

  ;; large file settings
  gc-cons-threshold 50000000
  large-file-warning-threshold 100000000)

;; Backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-inhibited t)

;; replace marked text when type
(delete-selection-mode 1)

;; Custom file for UI configurations
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; move cursor by camelCase
(subword-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Reload file without asking when changed externally
(global-auto-revert-mode t)

;; Keep a list of recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 200)

;; make indentation commands use space only
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; dired configurations
(put 'dired-find-file-other-buffer 'disabled t)
(setq dired-listing-switches "-alh")

;; whitespace display
(global-whitespace-mode)
(setq whitespace-global-modes
      '(not magit-mode git-commit-mode))
(setq whitespace-style '(face trailing tabs))

;; allow remembering risky variables
(defun risky-local-variable-p (sym &optional _ignored) nil)
;; Then it'll only re-confirm when you edit one.
;; You can also do (advice-add 'risky-local-variable-p :override #'ignore), whi

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
