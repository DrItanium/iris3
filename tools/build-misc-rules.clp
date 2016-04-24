; Copyright (c) 2015 Joshua Scoggins
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgement in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.

(deffunction build-single-argument-function-parser
             (?op)
             (format t 
                     "(defclass %s 
                        (is-a node)
                        (slot argument
                              (visibility public)
                              (storage local)
                              (default ?NONE)))%n%n"
                     ?op)
             (format t
                     "(defrule translate-list-to-%s
                               (stage (current parse))
                               ?f <- (object (is-a list)
                                             (contents %s ?arg)
                                             (name ?name)
                                             (parent ?parent))
                               =>
                               (unmake-instance ?f)
                               (make-instance ?name of %s
                                              (argument ?arg)
                                              (parent ?parent)))%n%n"
                     ?op
                     ?op
                     ?op))
(deffunction build-multi-argument-function-parser
             (?op)
             (format t 
                     "(defclass %s 
                        (is-a node)
                        (multislot arguments 
                                   (visibility public)
                                   (storage local)
                                   (default ?NONE)))%n%n"
                     ?op)
             (format t
                     "(defrule translate-list-to-%s
                               (stage (current parse))
                               ?f <- (object (is-a list)
                                             (contents %s $?rest)
                                             (name ?name)
                                             (parent ?parent))
                               =>
                               (unmake-instance ?f)
                               (make-instance ?name of %s
                                              (arguments ?rest)
                                              (parent ?parent)))%n%n"
                     ?op
                     ?op
                     ?op))
(progn$ (?a (create$ set-dynamic-constraint-checking
                     batch*
                     batch
                     load
                     load*
                     stringp
                     lexemep
                     symbolp
                     numberp
                     integerp
                     floatp
                     instance-name
                     symbol-to-instance-name
                     instance-name-to-symbol
                     length$))
        (build-single-argument-function-parser ?a))
(progn$ (?a (create$ retract
                     create$
                     unmake-instance
                     assert))
        (build-multi-argument-function-parser ?a))

(exit)
