;;; -*- Mode: Lisp -*-

(defpackage :symtable.system
  (:use :cl :asdf))

(in-package :symtable.system)

(defsystem :symtable
  :depends-on (alexandria iterate semi-precious)
  :components
  ((:file "packages")
   (:file "symtable" :depends-on ("packages"))))
