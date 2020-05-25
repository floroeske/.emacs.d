(use-package eglot
  :ensure t)

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
