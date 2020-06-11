(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq default-frame-alist '((font . "Monospace 10")))

(setq
  ;; better startup
  inhibit-splash-screen t
  inhibit-startup-message t
  ;; show column number at bottom bar
  column-number-mode t
  ;; disable anoying beep
  ring-bell-function 'ignore
  ;; improve rendering performance
  redisplay-dont-pause t
)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (load-theme 'flo-one t)

;; When running emacs as daemon.
;;
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (load-theme 'flo-one t))))
;;   (load-theme 'flo-one t))
