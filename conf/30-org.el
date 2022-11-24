;; The newest org-mode release, version 9.0.0,
;; apparently lets src-blocks inherit org-block face,
;; while the org-block-background face becomes obsolete.
;; So now just by setting org-block face you should be
;; able to have fixed-width fonts for all codes.

(use-package org
  :ensure t
  :init
  ;; (defface org-block-begin-line
  ;;   '((t (:underline "#A7A6AA" :foreground "#cccccc" :background "#EAEAFF")))
  ;;   "Face used for the line delimiting the begin of source blocks.")

  ;; (defface org-block
  ;;   '((t (:background "#EAEAFF")))
  ;;   "Face used for the source block background.")

  ;; (defface org-block-end-line
  ;;   '((t (:overline "#A7A6AA" :foreground "#cccccc" :background "#EAEAFF")))
  ;;   "Face used for the line delimiting the end of source blocks."))

  (defface org-block-begin-line
    '((t (:foreground "#666666" :background "#f0f0f0")))
    "Face used for the line delimiting the begin of source blocks.")

  (defface org-block-end-line
    '((t (:foreground "#666666" :background "#f0f0f0")))
    "Face used for the line delimiting the end of source blocks."))


(defun org-wrap ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

;; Enable visible-mode after loading org mode to show all content
;; (add-hook 'org-mode-hook 'visible-mode)
