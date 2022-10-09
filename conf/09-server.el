;; (unless (and (fboundp 'server-running-p)
;;              (server-running-p))
;;   (server-start))


;; emacsclient filename:linenumber:position
(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "looks for filenames like file:line or file:line:position and reparses name in such manner that position in file"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) "")))
                               )
                            fn))) files))
  )
