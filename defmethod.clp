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
(defclass defmethod
  (is-a thing
        has-comment)
  (slot method-name
        (type SYMBOL)
        (default ?NONE))
  (slot index 
        (type INTEGER)
        (default-dynamic -1))
  (slot args
        (type INSTANCE-NAME)
        (allowed-classes list)
        (default ?NONE))
  (multislot body))


(defrule build:defmethod:index:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?index&:(integerp ?index)
                                 ?comment
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ; just match against it to make sure
         (object (is-a list)
                 (name ?args))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (method-name ?name)
                        (index ?index)
                        (comment ?cvalue)
                        (args ?args)
                        (body ?body)))
(defrule build:defmethod:index:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?index&:(integerp ?index)
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ; just match against it to make sure
         (object (is-a list)
                 (name ?args))
         =>
         (unmake-instance ?f)
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (method-name ?name)
                        (index ?index)
                        (args ?args)
                        (body ?body)))

(defrule build:defmethod:no-index:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?comment
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         (object (is-a list)
                 (name ?args))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (method-name ?name)
                        (comment ?cvalue)
                        (args ?args)
                        (body ?body)))

(defrule build:defmethod:no-index:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmethod
                                 ?name
                                 ?args
                                 $?body)
                       (name ?id)
                       (parent ?parent))
         (object (is-a list)
                 (name ?args))
         =>
         (unmake-instance ?f)
         (make-instance ?id of defmethod
                        (parent ?parent)
                        (method-name ?name)
                        (args ?args)
                        (body ?body)))

(defclass defmethod-argument
  "An argument in a defmethod"
  (is-a thing)
  (slot argument-name
        (type INSTANCE-NAME)
        (default ?NONE))
  (multislot types
             (type SYMBOL))
  (slot query))
(defgeneric all-symbolsp
            "returns true if all the elements in a list are symbols")
(defmethod all-symbolsp
  ((?list MULTIFIELD))
  (bind ?result (filter$ symbolp
                         ?list))
  (= (length$ ?list)
     (length$ ?result)))
(defmethod all-symbolsp
 ($?list)
 (all-symbolsp ?list))

(defrule build:defmethod-argument:single-field
         (stage (current parse))
         (object (is-a defmethod)
                 (args ?args))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (contents $?before ?target $?after))
         ?f3 <- (object (is-a singlefield-variable)
                        (name ?target))
         =>
         (bind ?name (instance-name (make-instance of defmethod-argument
                                                   (parent ?args)
                                                   (argument-name ?target))))
         (modify-instance ?f2 
                          (contents ?before ?name ?after))
         (modify-instance ?f3
                          (parent ?name)))

(defrule build:defmethod-argument:parameterized:types-and-query
         (stage (current parse))
         (object (is-a defmethod)
                 (args ?args))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (contents $? ?target $?))
         ?f3 <- (object (is-a list)
                        (name ?target)
                        (contents ?option
                                  ?type&:(symbolp ?type)
                                  $?types&:(all-symbolsp ?types)
                                  ?query))
         (object (is-a singlefield-variable)
                 (name ?option))
         (object (is-a list|global-variable)
                 (name ?query))
         =>
         (unmake-instance ?f3)
         (make-instance ?target of defmethod-argument
                        (parent ?args)
                        (argument-name ?option)
                        (types ?type ?types)
                        (query ?query)))

(defrule build:defmethod-argument:parameterized:query-only
         (stage (current parse))
         (object (is-a defmethod)
                 (args ?args))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (contents $? ?target $?))
         ?f3 <- (object (is-a list)
                        (name ?target)
                        (contents ?option
                                  ?query))
         (object (is-a singlefield-variable)
                 (name ?option))
         (object (is-a list|global-variable)
                 (name ?query))
         =>
         (unmake-instance ?f3)
         (make-instance ?target of defmethod-argument
                        (parent ?args)
                        (argument-name ?option)
                        (query ?query)))

(defrule build:defmethod-argument:parameterized:types-only
         (stage (current parse))
         (object (is-a defmethod)
                 (args ?args))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (contents $? ?target $?))
         ?f3 <- (object (is-a list)
                        (name ?target)
                        (contents ?option
                                  ?type&:(symbolp ?type)
                                  $?types&:(all-symbolsp ?types)))
         (object (is-a singlefield-variable)
                 (name ?option))
         =>
         (unmake-instance ?f3)
         (make-instance ?target of defmethod-argument
                        (parent ?args)
                        (argument-name ?option)
                        (types ?type ?types)))

(defrule error:build:defmethod-argument:parameterized:multifield-variable
         (stage (current parse))
         (object (is-a defmethod)
                 (args ?args))
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (contents $? ?target $?))
         ?f3 <- (object (is-a list)
                        (name ?target)
                        (contents ?option
                                  $?))
         (object (is-a ~singlefield-variable)
                 (name ?option)
                 (value ?option-value))
         =>
         (printout werror "ERROR: provided a non singlefield-variable (" ?option-value ") for a defmethod!" crlf)
         (halt))
