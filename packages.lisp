(defpackage #:symtable
  (:nicknames :sym)
  (:use :common-lisp :alexandria :iterate)
  (:export
   #:make-symtable #:symtable
   #:add #:name #:addr* #:addr #:next-name* #:next-name #:next-addr
   #:do-table-symbols
   #:load-system-map))
