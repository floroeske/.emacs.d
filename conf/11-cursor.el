(set-cursor-color "#111111")
(setq-default cursor-type 'box)

;; change to bar when overwrite
(add-hook 'overwrite-mode-hook
          (lambda ()
            (setq cursor-type (if overwrite-mode 'hollow 'box))))

