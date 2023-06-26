(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq
  ;; better startup
  inhibit-splash-screen t
  inhibit-startup-message t
  ;; show column number at bottom bar
  column-number-mode t
  ;; disable anoying beep
  ring-bell-function 'ignore
  ;; improve rendering performance
  redisplay-dont-pause t)

;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

(use-package modus-themes
  :ensure t)

(load-theme 'modus-operandi t)

;; When running emacs as daemon.
;;
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (load-theme 'flo-one t))))
;;   (load-theme 'flo-one t))


;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))
;; (setq doom-modeline-height 16)

;; enable toolbar icons (only necessary on kde?)
(setq tool-bar-style 'image)
