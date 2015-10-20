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

(defclass function
  (is-a thing
        has-title
        has-comment)
  (multislot arguments
             (visibility public))
  (multislot local-binds
             (visibility public))
  (multislot body
             (visibility public)))

(defclass deffunction
  (is-a function))

(defrule translate-deffunction:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deffunction 
                                 ?func-name 
                                 ?comment
                                 ?args
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?k <- (object (is-a string)
                       (name ?comment)
                       (value ?cvalue))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f ?k)
         (progn$ (?ag ?a) 
                 (send ?ag put-parent ?name))
         (make-instance ?name of deffunction
                        (title ?func-name)
                        (parent ?parent)
                        (comment ?cvalue)
                        (arguments ?a)
                        (body ?body)))
(defrule translate-deffunction:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deffunction 
                                 ?func-name 
                                 ?args
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f)
         (progn$ (?ag ?a) 
                 (send ?ag put-parent ?name))
         (make-instance ?name of deffunction
                        (title ?func-name)
                        (parent ?parent)
                        (arguments ?a)
                        (body ?body)))


(defglobal MAIN
           ?*handler-types* = (create$ primary 
                                       around
                                       before
                                       after))
(deffunction valid-handlerp
             (?handler-type)
             (not (neq ?handler-type 
                       $?*handler-types*)))

(defclass defmessage-handler
  "A wrapper for a defmessage-handler declaration"
  (is-a function)
  (slot target-class
        (type SYMBOL
              INSTANCE-NAME)
        (visibility public)
        (default ?NONE))
  (slot handler-type
        (type SYMBOL)
        (allowed-symbols primary
                         around
                         before
                         after)))


(defrule convert-defmessage-handler:all
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?handler-type&:(valid-handlerp ?handler-type)
                                 ?comment
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (title ?class-name)
                 (name ?class-id))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (comment ?cvalue)
                        (target-class ?class-id)
                        (title ?message-name)
                        (handler-type ?handler-type)
                        (arguments ?params)
                        (body ?actions)))


(defrule convert-defmessage-handler:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?handler-type&:(valid-handlerp ?handler-type)
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (title ?class-name)
                 (name ?class-id))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (target-class ?class-id)
                        (title ?message-name)
                        (handler-type ?handler-type)
                        (arguments ?params)
                        (body ?actions)))

(defrule convert-defmessage-handler:no-handler
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?comment
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (title ?class-name)
                 (name ?class-id))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (comment ?cvalue)
                        (target-class ?class-id)
                        (title ?message-name)
                        (arguments ?params)
                        (body ?actions)))

(defrule convert-defmessage-handler:no-handler-or-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defmessage-handler
                                 ?class-name&:(symbolp ?class-name)
                                 ?message-name&:(symbolp ?message-name)
                                 ?parameters
                                 $?actions)
                       (name ?id)
                       (parent ?parent))
         (object (is-a defclass)
                 (title ?class-name)
                 (name ?class-id))
         ?f3 <- (object (is-a list)
                        (name ?parameters)
                        (contents $?params))
         =>
         (unmake-instance ?f ?f3)
         (progn$ (?p ?params)
                 (send ?p put-parent ?id))
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (target-class ?class-id)
                        (title ?message-name)
                        (arguments ?params)
                        (body ?actions)))

(deffunction strip-singlefield
             (?element)
             (if (and (instance-namep ?element)
                      (eq (class ?element)
                          singlefield-variable)) then 
               ?element 
               else 
               (create$)))

(deffunction only-single-fields
             (?list)
             (= (length$ ?list)
                (length$ (apply$ strip-singlefield
                                 ?list))))
(deffunction wildcardp
             (?element)
             (and (instance-namep ?element)
                  (eq (class ?element)
                      multifield-variable)))

(deffunction has-wildcard-parameter
             (?list)
             (exists$ wildcardp
                      ?list))

(defrule error:convert-function-arguments:multiple-wildcards
         (stage (current parse))
         ?q <- (object (is-a function)
                       (arguments $?sfs&:(not (only-single-fields ?sfs))
                                  ?wc)
                       (title ?function))
         (object (is-a multifield-variable)
                 (name ?wc))
         =>
         (printout werror "ERROR: extra wildcard defined in argument list of " (class ?q) " " ?function crlf)
         (halt))

(defrule error:convert-function-arguments:out-of-order-wildcard
         (stage (current parse))
         ?q <- (object (is-a function)
                       (arguments $?sf&:(has-wildcard-parameter ?sf)
                                  ?sf0)
                       (title ?function))
         (object (is-a singlefield-variable)
                 (name ?sf0))
         =>
         (printout werror "ERROR: wildcard parameter is not defined as the last argument in " (class ?q) " " ?function crlf)
         (halt))

(defrule reference-function-arguments
         (declare (salience ?*priority:three*)) ; has to go before the bind operations
         (stage (current associate))
         (object (is-a function)
                 (arguments $? ?arg $?)
                 (name ?function))
         (object (name ?arg)
                 (is-a singlefield-variable)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?cvalue)
                       (name ?targ&~?arg)
                       (parent ?parent))
         (test (not (neq ?function 
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?targ of reference
                        (parent ?parent)
                        (value ?arg)))

(defrule reference-function-arguments:last:multifield:exact
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (arguments $? ?last)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-variable)
                       (value ?cvalue)
                       (name ?name&~?last)
                       (parent ?parent))
         (test (not (neq ?function
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

(defrule reference-function-arguments:last:multifield:mismatch
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (arguments $? ?last)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?qvalue&:(eq ?cvalue
                                           (format nil "$%s" ?qvalue)))
                       (name ?name)
                       (parent ?parent))
         (test (not (neq ?function
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

(defrule reference-function-local-binds:singlefield:exact
         (declare (salience ?*priority:three*)) ; has to go before the bind operations
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?arg $?)
                 (name ?function))
         (object (name ?arg)
                 (is-a singlefield-variable)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?cvalue)
                       (name ?targ&~?arg)
                       (parent ?parent))
         (test (not (neq ?function 
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?targ of reference
                        (parent ?parent)
                        (value ?arg)))

(defrule reference-function-local-binds:singlefield:mismatch
         (declare (salience ?*priority:three*)) ; has to go before the bind operations
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?arg $?)
                 (name ?function))
         (object (name ?arg)
                 (is-a singlefield-variable)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-variable)
                       (value ?mfvalue&:(eq ?mfvalue 
                                            (format nil "$%s" ?cvalue)))
                       (name ?targ&~?arg)
                       (parent ?parent))
         (test (not (neq ?function 
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?targ of reference
                        (parent ?parent)
                        (value ?arg)))

(defrule reference-function-local-binds:last:multifield:exact
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?last $?)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-variable)
                       (value ?cvalue)
                       (name ?name&~?last)
                       (parent ?parent))
         (test (not (neq ?function
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

(defrule reference-function-local-binds:multifield:mismatch
         (declare (salience ?*priority:three*))
         (stage (current associate))
         (object (is-a function)
                 (local-binds $? ?last $?)
                 (name ?function))
         (object (is-a multifield-variable)
                 (name ?last)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-variable)
                       (value ?qvalue&:(eq ?cvalue
                                           (format nil "$%s" ?qvalue)))
                       (name ?name)
                       (parent ?parent))
         (test (not (neq ?function
                         (expand$ (send ?f get-parent-chain)))))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (parent ?parent)
                        (value ?last)
                        (expand TRUE)))

