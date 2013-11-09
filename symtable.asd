;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :symtable
  :depends-on (:alexandria :iterate :cl-containers)
  :components
  ((:file "packages")
   (:file "symtable" :depends-on ("packages"))))
