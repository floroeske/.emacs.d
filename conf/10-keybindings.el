(global-set-key (kbd "C-<return>") 'custom/insert-new-line)
(global-set-key (kbd "C-a") 'custom/smart-move-beginning-of-line)
(global-set-key (kbd "C-c d") 'custom/duplicate-current-line-or-region)
(global-set-key (kbd "C-x =") 'custom/swap-buffers-in-windows)
(global-set-key (kbd "C-x C-k") 'custom/delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'custom/rename-current-buffer-file)
(global-set-key (kbd "C-c /") 'custom/toggle-line-comment)
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c i") 'indent-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(global-set-key [(control f4)] 'highlight-symbol-remove-all)

(global-set-key [M-up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [M-down] (lambda () (interactive) (scroll-up 1)))

(global-set-key [C-tab] 'company-complete)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c C--") 'custom/camelcase-word-or-region)
(global-set-key (kbd "C-c C-_") 'custom/snakecase-word-or-region)
