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
(defclass defmethod-argument-list
  (is-a composite-thing))


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
         ?f3 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?args of defmethod-argument-list
                        (parent ?aparent)
                        (contents ?contents))
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
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?args of defmethod-argument-list
                        (parent ?aparent)
                        (contents ?contents))
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
         ?f3 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?args of defmethod-argument-list
                        (parent ?aparent)
                        (contents $?contents))
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
         ?f2 <- (object (is-a list)
                        (name ?args)
                        (parent ?aparent)
                        (contents $?contents))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?args of defmethod-argument-list
                        (parent ?aparent)
                        (contents $?contents))
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

(defmethod multifield-variablep
  ((?var INSTANCE))
  (eq (class ?var)
      multifield-variable))

; we leave the bare ? and $? options alone as it make reconstruction easier
(defrule build:defmethod-argument:wildcard-parameter:nested-list:types-and-query
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (name ?args)
                 (contents $?before ?last))
         ?f2 <- (object (is-a list)
                        (name ?last)
                        (contents ?mname
                                  ?type&:(symbolp ?type)
                                  $?types&:(all-symbolsp ?types)
                                  ?query))
         ?f3 <- (object (is-a multifield-variable)
                        (name ?mname))
         ?f4 <- (object (is-a list|global-variable)
                        (name ?query))
         =>
         (unmake-instance ?f2)
         (bind ?name (instance-name (make-instance ?last of defmethod-argument
                                                   (argument-name ?mname)
                                                   (parent ?args)
                                                   (types ?type ?types)
                                                   (query ?query))))
         (modify-instance ?f3 (parent ?name))
         (modify-instance ?f4 (parent ?name)))

(defrule build:defmethod-argument:wildcard-parameter:nested-list:types-only
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (name ?args)
                 (contents $?before ?last))
         ?f2 <- (object (is-a list)
                        (name ?last)
                        (contents ?mname
                                  ?type&:(symbolp ?type)
                                  $?types&:(all-symbolsp ?types)))
         ?f3 <- (object (is-a multifield-variable)
                        (name ?mname))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f3 
                          (parent (instance-name (make-instance ?last of defmethod-argument
                                                                (argument-name ?mname)
                                                                (parent ?args)
                                                                (types ?type ?types))))))


(defrule build:defmethod-argument:wildcard-parameter:nested-list:query
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (name ?args)
                 (contents $?before ?last))
         ?f2 <- (object (is-a list)
                        (name ?last)
                        (contents ?mname
                                  ?query))
         ?f4 <- (object (is-a multifield-variable)
                        (name ?mname))
         ?f3 <- (object (is-a list|global-variable)
                        (name ?query))
         =>
         (unmake-instance ?f2)
         (bind ?name (instance-name (make-instance ?last of defmethod-argument
                                                   (argument-name ?mname)
                                                   (parent ?args)
                                                   (query ?query))))
         (modify-instance ?f3 (parent ?name))
         (modify-instance ?f4 (parent ?name)))

; Error states
(defrule error:defmethod-argument:wildcard-parameter:nested-list:no-types-or-query
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (contents $?before ?last)
                 (parent ?parent))
         (object (is-a list)
                 (name ?last)
                 (contents ?mname))
         (object (is-a defmethod)
                 (name ?parent)
                 (method-name ?name))

         =>
         (printout werror "ERROR: no type information provided in wildcard-parameter nested-list in defmethod: " ?name crlf)
         (halt))

(defrule error:defmethod-argument:wildcard-parameter:not-last-argument:bare
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (contents $?before ?last ? $?)
                 (parent ?parent))
         (object (is-a multifield-variable)
                 (name ?last))
         (object (is-a defmethod)
                 (name ?parent)
                 (method-name ?name))

         =>
         (printout werror "ERROR: only the last argument of a defmethod list can be a wildcard-parameter!" crlf
                   tab "Offending method is: " ?name crlf)
         (halt))

(defrule error:defmethod-argument:wildcard-parameter:not-last-argument:nested-list
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (contents $?before ?last ? $?)
                 (parent ?parent))
         (object (is-a list)
                 (name ?last)
                 (contents ?arg $?))
         (object (is-a multifield-variable)
                 (name ?arg))
         (object (is-a defmethod)
                 (name ?parent)
                 (method-name ?name))

         =>
         (printout werror "ERROR: only the last argument of a defmethod list can be a wildcard-parameter!" crlf
                   tab "Offending method is: " ?name crlf)
         (halt))

(defrule build:defmethod-argument:singlefield-argument:all
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (contents $? ?curr $?))
         ?f <- (object (is-a list)
                       (name ?curr)
                       (parent ?parent)
                       (contents ?mname
                                 ?type&:(symbolp ?type)
                                 $?types&:(all-symbolsp ?types)
                                 ?query))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?mname))
         ?f3 <- (object (is-a list|global-variable)
                        (name ?query))
         =>
         (unmake-instance ?f)
         (bind ?name (instance-name (make-instance ?curr of defmethod-argument
                                                   (parent ?parent)
                                                   (argument-name ?mname)
                                                   (types ?type ?types)
                                                   (query ?query))))
         (modify-instance ?f2 (parent ?name))
         (modify-instance ?f3 (parent ?name)))

(defrule build:defmethod-argument:singlefield-argument:types-only
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (name ?args)
                 (contents $? ?curr $?))
         ?f <- (object (is-a list)
                       (name ?curr)
                       (contents ?mname
                                 ?type&:(symbolp ?type)
                                 $?types&:(all-symbolsp ?types)))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?mname))
         =>
         (unmake-instance ?f)
         (bind ?name (instance-name (make-instance ?curr of defmethod-argument
                                                   (parent ?args)
                                                   (argument-name ?mname)
                                                   (types ?type ?types))))
         (modify-instance ?f2 (parent ?name)))
(defrule build:defmethod-argument:singlefield-argument:query-only
         (stage (current parse))
         (object (is-a defmethod-argument-list)
                 (name ?args)
                 (contents $? ?curr $?))
         ?f <- (object (is-a list)
                       (name ?curr)
                       (contents ?mname
                                 ?query))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?mname))
         ?f3 <- (object (is-a list|global-variable)
                        (name ?query))
         =>
         (unmake-instance ?f)
         (bind ?name (instance-name (make-instance ?curr of defmethod-argument
                                                   (parent ?args)
                                                   (argument-name ?mname)
                                                   (query ?query))))
         (modify-instance ?f2 (parent ?name))
         (modify-instance ?f3 (parent ?name)))
