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

; make-instance command

(defclass make-instance
  (is-a thing
        has-title)
  (slot of 
        (type SYMBOL
              INSTANCE)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot fields))


(defclass make-instance-field
  (is-a thing
        has-title)
  (multislot value
             (storage local)
             (visibility public)
             (default ?NONE)))

(defrule build-make-instance-node:has-name
         (stage (current parse))
         ?o <- (object (is-a list)
                       (contents make-instance ?title of ?type
                                 $?fields)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?o)
         (make-instance ?name of make-instance
                        (title ?title)
                        (of ?type)
                        (parent ?parent)
                        (fields $?fields)))
(defrule build-make-instance-node:no-name-so-use-gensym*
         (stage (current parse))
         ?o <- (object (is-a list)
                       (contents make-instance of ?type
                                 $?fields)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?o)
         (make-instance ?name of make-instance
                        (title (gensym*))
                        (of ?type)
                        (fields $?fields)
                        (parent ?parent)))
(defrule build-make-instance-field
         (stage (current parse))
         ?o <- (object (is-a make-instance)
                       (fields $? ?b $?))
         ?f <- (object (is-a list)
                       (name ?b)
                       (contents ?title
                                 $?contents)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?b of make-instance-field
                        (title ?title)
                        (parent ?p)
                        (value ?contents)))
        

(defclass modify-instance
  (is-a thing)
  (slot target
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot fields))


(defclass modify-instance-field
  (is-a thing
        has-title)
  (multislot value
             (storage local)
             (visibility public)
             (default ?NONE)))

(defrule build-modify-instance-node
         (stage (current parse))
         ?o <- (object (is-a list)
                       (contents modify-instance ?target 
                                 $?fields)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?o)
         (make-instance ?name of modify-instance
                        (target ?target)
                        (parent ?parent)
                        (fields $?fields)))

(defrule build-modify-instance-field
         (stage (current parse))
         ?o <- (object (is-a modify-instance)
                       (fields $? ?b $?))
         ?f <- (object (is-a list)
                       (name ?b)
                       (contents ?title
                                 $?contents)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?b of modify-instance-field 
                        (title ?title)
                        (parent ?p)
                        (value ?contents)))
        
