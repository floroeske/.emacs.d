(use-package rg
  :ensure t)

(rg-enable-default-bindings)

(setq rg-custom-type-aliases
  '(("cpp" .    "*.cpp *.cxx *.cc *.hpp *.h")
    ("c" .      "*.c *.h")
    ("py" .     "*.py")
    ("org" .    "*.org")
    ("log" .    "*.txt *.log")
    ))
