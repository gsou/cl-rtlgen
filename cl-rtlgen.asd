(asdf:defsystem #:cl-rtlgen
  :description "Use Common Lisp to generate RTL files"
  :version "0.1"
  :author "Skami"
  :license "MIT"
  :components ((:file "packages")
               (:file "cl-rtlgen" :depends-on ("packages"))))
