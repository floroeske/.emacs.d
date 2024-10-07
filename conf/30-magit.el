(use-package magit
  :ensure t
  :pin melpa)

;; (setq magit-diff-highlight-indentation nil)
;; (setq magit-diff-highlight-trailing nil)
;; (setq magit-diff-paint-whitespace nil)
;; (setq magit-diff-highlight-hunk-body nil)
;; (setq magit-diff-refine-hunk nil)
(setq magit-refresh-status-buffer nil)

(setq magit-status-headers-hook '(magit-insert-error-header
                                  magit-insert-diff-filter-header
                                  magit-insert-head-branch-header
                                  magit-insert-upstream-branch-header
                                  magit-insert-push-branch-header
                                  magit-insert-tags-header
                                  magit-insert-user-header
                                  magit-insert-remote-header))
