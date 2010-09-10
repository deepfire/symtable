(defpackage #:symtable
  (:nicknames :sym)
  (:use :common-lisp :alexandria :iterate :intree)
  (:export
   #:make-symtable #:symtable
   #:add #:name #:addr* #:addr #:next-name* #:next-name #:next-addr
   #:do-table-symbols
   #:load-system-map))
