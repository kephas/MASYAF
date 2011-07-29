(defpackage :thierry-technologies.com/2010/01/masyaf-system
  (:use :common-lisp :asdf))

(in-package :thierry-technologies.com/2010/01/masyaf-system)

(defsystem "masyaf"
  :description "Multi-Agent SYstem, game-Agnostic Framework for logical game solving"
  :version "0.1.0"
  :author "Pierre Thierry <pierre.thierry@thierry-technologies.com>"
  :licence "GPL"
  :depends-on ("cl-utilities")
  :components ((:file "package")
	       (:file "macros" :depends-on ("package"))
	       (:file "clone" :depends-on ("package"))
	       (:file "information" :depends-on ("package"))
	       (:file "graph" :depends-on ("package" "macros"))
	       (:file "gamestate" :depends-on ("information" "graph"))
	       (:file "display" :depends-on ("graph" "macros"))
	       (:file "agents" :depends-on ("package"))
	       (:file "scheduler" :depends-on ("agents" "gamestate"))
	       (:file "games/trivial" :depends-on ("gamestate" "macros"))
	       (:file "games/bridges" :depends-on ("gamestate" "macros"))))
