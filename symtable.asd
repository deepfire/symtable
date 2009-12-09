;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :symtable
  :depends-on (alexandria iterate semi-precious)
  :components
  ((:file "packages")
   (:file "symtable" :depends-on ("packages"))))
