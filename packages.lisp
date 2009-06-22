(defpackage #:symtable
  (:nicknames :sym)
  (:use :common-lisp :alexandria :iterate :octree-1d)
  (:export
   #:make-symtable #:symtable
   #:add #:name #:addr #:next-name #:next-addr
   #:do-table-symbols
   #:load-system-map))
