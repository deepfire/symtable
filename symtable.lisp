;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SYMTABLE; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2007-2008 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :sym)

(defstruct (symtable (:constructor %make-symtable)
		     (:print-object (lambda (obj stream)
				      (format stream "~@<#<SYMTABLE~; #x~8,'0X+#x~X, ~D symbols~;>~:@>"
					      (symtable-extent-start obj)
					      (symtable-extent-len obj)
					      (symtable-nsyms obj)))))
  store
  extent-start
  extent-len
  name#
  nsyms)

(defun make-symtable (start len)
  (%make-symtable :nsyms 0
		  :extent-start start
		  :extent-len len
		  :name# (make-hash-table)
		  :store (oct-1d:make-tree :start start :length len)))

(defun add (addr val symtable)
  (setf (gethash val (symtable-name# symtable)) addr)
  (incf (symtable-nsyms symtable))
  (oct-1d:insert addr val (symtable-store symtable)))

(defun name (symtable addr)
  (oct-1d:tree-left addr (symtable-store symtable)))

(defun addr (symtable name)
  (gethash name (symtable-name# symtable)))

(defun next-name (name symtable)
  (oct-1d:tree-right (addr symtable name) (symtable-store symtable)))

(defun next-addr (addr symtable)
  (oct-1d:tree-right addr (symtable-store symtable)))

(defun load-system-map (filename &key (name :kernel) (nickname :k) reuse-package)
  (when (and (or (find-package name) (find-package nickname)) (null reuse-package))
    (error "Can't make a symbol table named ~S: similarly-named package already exists." name))
  (let* ((*read-base* 16) (syms nil)
	 (package (or (when reuse-package
			(when-let ((package (find-package name)))
			  (unless (eq package (find-package nickname))
			    (error "While attempting to reuse package: package name ~S and nickname ~S refer to different packages." name nickname))
			  package))
		      (make-package name :nicknames (list nickname))))
	 (minimax (with-open-file (stream filename)
		    (iter (for addr = (read stream nil nil nil))
			  (for type = (read stream nil nil nil))
			  (for sym = (let ((*package* package))
				       (read stream nil nil nil)))
			  (while (and addr sym))
			  (push (cons addr sym) syms)
			  (maximize addr into maxaddr)
			  (minimize addr into minaddr)
			  (finally (return (cons minaddr maxaddr))))))
	 (table (make-symtable (car minimax) (- (cdr minimax) (car minimax)))))
    (iter (for (addr . sym) in syms)
          (when (symbolp sym)
            (export sym package)
            (add addr sym table)))
    table))

(defmacro do-table-symbols ((symbol address) table &body body)
  "Execute BODY with SYMBOL and ADDRESS bound, in turn, to each entry in TABLE."
  `(iter (for (,symbol ,address) in-hashtable (symtable-name# ,table))
         ,@body))