(asdf:defsystem abc-histo
  :version "0"
  :description "abc lists data extraction"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "BSD-style"
  :depends-on (cl-ppcre)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "abc-histo")))
