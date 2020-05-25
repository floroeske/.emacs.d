(defalias 'gk-urls-external-browser 'browse-url-xdg-open)

(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse
with external browser."
  (apply
   (if (y-or-n-p "Browse with EWW? ")
       'eww-browse-url
     'gk-urls-external-browser)
   args))

(setq browse-url-browser-function #'gk-browse-url)
