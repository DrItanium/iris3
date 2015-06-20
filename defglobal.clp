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

(defclass defglobal
  (is-a thing)
  (slot module 
        (type SYMBOL))
  (multislot assignments))
(defclass defglobal-assignment
  (is-a thing)
  (slot variable 
        (type INSTANCE)
        (allowed-classes variable)
        (default ?NONE))
  (slot value
        (default ?NONE)))

(deffunction equals-sign () =)
(defrule translate-defglobal:module
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defglobal 
                                 ?module&:(symbolp ?module)
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defglobal
                        (parent ?parent)
                        (module ?module)
                        (assignments ?rest)))

(defrule translate-defglobal:no-module
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defglobal 
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defglobal
                        (parent ?parent)
                        (assignments ?rest)))

(defrule build-defglobal-assignment:value-is-list
         (stage (current parse))
         ?f <- (object (is-a defglobal)
                       (assignments $?before 
                                    ?var =(equals-sign) ?value
                                    $?rest)
                       (name ?parent))
         ?f2 <- (object (is-a global-variable)
                        (name ?var)
                        (parent ?parent))
         ?f3 <- (object (is-a thing)
                        (name ?value)
                        (parent ?parent))
         =>
         (bind ?assignment 
               (instance-name (make-instance of defglobal-assignment
                                             (parent ?parent)
                                             (variable ?var)
                                             (value ?value))))
         (modify-instance ?f2 (parent ?assignment))
         (modify-instance ?f3 (parent ?assignment))
         ; TODO: expand this to support nested lists and such 
         ; (need to take over parentage since it is a new list)
         (modify-instance ?f 
                          (assignments ?before
                                       ?assignment
                                       ?rest)))

(defrule build-defglobal-assignment:value-is-scalar
         (stage (current parse))
         ?f <- (object (is-a defglobal)
                       (assignments $?before 
                                    ?var =(equals-sign) ?value&:(not (instance-namep ?value))
                                    $?rest)
                       (name ?parent))
         ?f2 <- (object (is-a global-variable)
                        (name ?var)
                        (parent ?parent))
         =>
         (bind ?assignment 
               (instance-name (make-instance of defglobal-assignment
                                             (parent ?parent)
                                             (variable ?var)
                                             (value ?value))))
         (modify-instance ?f2 (parent ?assignment))
         ; TODO: expand this to support nested lists and such 
         ; (need to take over parentage since it is a new list)
         (modify-instance ?f 
                          (assignments ?before
                                       ?assignment
                                       ?rest)))
