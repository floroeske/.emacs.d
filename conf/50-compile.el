;; https://emacs.stackexchange.com/questions/691/how-can-i-customize-the-compile-command
;; create a file in the project root with the default compile command
;;
;; .dir-locals.el
;; ((nil . ((compile-command . "make package install"))))
;; or
;; ((nil . ((compile-command . (concat "cd " (file-name-directory (locate-dominating-file buffer-file-name ".dir-locals.el")) " && catkin_make")))))
;; or
;; ((nil . ((compile-command . (let (dl-filename dl-filepath dl-path)
;;                               (setq dl-filename ".dir-locals.el")
;;                               (setq dl-filepath (locate-dominating-file buffer-file-name dl-filename))
;;                               (setq dl-path (file-name-directory dl-filepath))
;;                               (concat "cd " dl-path " && catkin_make"))))))


;; emacs compile buffer auto close?
;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                    buffer)))

;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; custom message after compilation is finished
;; (defun my-compilation-finish-function (buffer desc)
;;   (message "End of compilation %f" compilation-time))
;; (add-hook 'compilation-finish-functions 'my-compilation-finish-function)

;; Cucumber's ANSI colors messing up emacs compilation buffer
;; https://stackoverflow.com/questions/3072648/cucumbers-ansi-colors-messing-up-emacs-compilation-buffer
;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (let ((inhibit-read-only t))
;;     (ansi-color-apply-on-region (point-min) (point-max))))

;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)



;; (ignore-errors
;;   (require 'ansi-color)
;;   (defun my-colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (ansi-color-apply-on-region compilation-filter-start (point-max))))
;;   (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ignore-errors
;;   (require 'ansi-color)
;;   (setq ansi-color-names-vector
;;         [,alum-7 ,red-3 ,cham-3 ,butter-3, blue-3 ,plum-3 ,blue-1 ,alum-1])
;;   (setq ansi-color-map (ansi-color-make-color-map))
 
;;   (defun colorize-compilation-buffer ()
;;     (let ((inhibit-read-only t))
;;       (ansi-color-apply-on-region (point-min) (point-max))))
;;   (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))


;; (setq comint-buffer-maximum-size 2000)
;; (add-hook 'compilation-filter-hook 'comint-truncate-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; auto scroll
;; https://stackoverflow.com/questions/4657142/how-do-i-encourage-emacs-to-follow-the-compilation-buffer
(setq compilation-scroll-output t)


(defun my-compile ()
  "Kill compilation buffer and create new one"
  (interactive)
  (progn
    (if (get-buffer "*compilation*") ; If old compile window exists
        (progn
          ;; (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
          (kill-buffer "*compilation*") ; and kill the buffers
          (print "I killed the compilation buffer!")
          )
      )
    (call-interactively 'compile)
    ;; (defvar compilation-time (measure-time (call-interactively 'compile)))
    ;; (enlarge-window 20)
    )
  )

(defun my-next-error () 
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )
  
(defun my-previous-error () 
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    )
  )

(setq compile-command "make")

(global-set-key (kbd "C-n") 'my-next-error)
(global-set-key (kbd "C-p") 'my-previous-error)
(global-set-key (kbd "C-<f12>") 'my-compile)
(global-set-key (kbd "C-<f11>") 'kill-compilation)
(global-set-key "\C-x\C-m" 'my-compile)

;; disable email key
(global-unset-key (kbd "C-x m"))


