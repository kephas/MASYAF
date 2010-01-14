(defpackage :thierry-technologies.com/2010/01/masyaf-system
  (:use :common-lisp :asdf))

(in-package :thierry-technologies.com/2010/01/masyaf-system)

(defsystem "masyaf"
  :description "Multi-agent system, game-agnostic framework for logical game solving"
  :version "0.1.0"
  :author "Pierre Thierry <pierre.thierry@thierry-technologies.com>"
  :licence "GPL"
  :depends-on ("cl-utilities")
  :components ((:file "package")
	       (:file "macros")
	       (:file "information")
	       (:file "graph"))
  :serial t)
