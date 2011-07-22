(defpackage :thierry-technologies.com/2010/01/masyaf
  (:use :common-lisp :asdf :cl-utilities)
  (:nicknames :masyaf)
  (:export 
   ;; macros
   #:cons-bind
   #:let-conses
   #:with-functions
   #:named-let
   #:cif
   ;; cloning
   #:clone
   #:shared-clone
   ;; informations
   #:information-symbol
   #:information-arguments
   #:*joker*
   #:_
   #:information-search
   #:information-add
   ;; space
   #:spatial
   #:space
   #:in-space?
   #:%in-space?
   #:origin
   #:unit-vector
   #:cartesian-space
   #:cartesian-hyperoctant
   #:space-dimensions
   #:vector
   #:vect-coords
   #:numbers ; used in spacing
   #:make-vector
   #:multiply
   #:next-point-in-grid
   #:%next-point
   #:complete-grid
   #:do-grid
   #:translate
   #:%translate
   #:make-translation
   #:neighbours
   #:unit-circle
   #:%neighbours
   #:manhattan-distance
   #:chebyshev-distance
   #:make-path
   #:except-first
   ;; gamestate
   #:gamestate-with-information
   #:gamestate-info-base
   #:gamestate-new-info
   #:gamestate-add-info
   #:gamestate-renew-info
   #:gamestate-info-search
   #:gamestate-cell
   #:retrieve-space-cell
   #:store-space-cell
   #:gamestate-with-space
   #:gamestate-cells
   ;; rendering
   #:render-2d-spatial-game-by-char
   ;; agents
   #:agent-symbols
   #:agent-apply
   ;; scheduler
   #:scheduler
   #:solve))
