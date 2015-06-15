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
(defglobal MAIN
           ?*handler-types* = (create$ primary 
                                       around
                                       before
                                       after))
(defclass defmessage-handler
  "A wrapper for a defmessage-handler declaration"
  (is-a thing
        has-comment)
  (slot target-class
        (type SYMBOL
              INSTANCE-NAME)
        (visibility public)
        (default ?NONE))
  (slot message-name
        (type SYMBOL)
        (visibility public)
        (default ?NONE))
  (slot handler-type
        (type SYMBOL)
        (allowed-symbols primary
                         around
                         before
                         after))
  (slot parameters
        (type INSTANCE-NAME)
        (default ?NONE))
  (multislot actions
             (type INSTANCE-NAME
                   LEXEME
                   NUMBER)))

(defclass defmessage-handler-parameters
  (is-a thing)
  (multislot arguments 
             (type INSTANCE-NAME)
             (allowed-classes singlefield-variable))
  (slot wildcard-argument
        (type INSTANCE-NAME)
        (allowed-classes multifield-variable)))

(deffunction valid-handlerp
             (?handler-type)
             (not (neq ?handler-type 
                       $?*handler-types*)))

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
                 (class-name ?class-name)
                 (name ?class-id))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         (object (is-a list)
                 (name ?parameters))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (comment ?cvalue)
                        (target-class ?class-id)
                        (message-name ?message-name)
                        (handler-type ?handler-type)
                        (parameters ?parameters)
                        (actions ?actions)))


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
                 (class-name ?class-name)
                 (name ?class-id))
         (object (is-a list)
                 (name ?parameters))
         =>
         (unmake-instance ?f)
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (target-class ?class-id)
                        (message-name ?message-name)
                        (handler-type ?handler-type)
                        (parameters ?parameters)
                        (actions ?actions)))

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
                 (class-name ?class-name)
                 (name ?class-id))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         (object (is-a list)
                 (name ?parameters))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (comment ?cvalue)
                        (target-class ?class-id)
                        (message-name ?message-name)
                        (parameters ?parameters)
                        (actions ?actions)))

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
                 (class-name ?class-name)
                 (name ?class-id))
         (object (is-a list)
                 (name ?parameters))
         =>
         (unmake-instance ?f)
         (make-instance ?id of defmessage-handler
                        (parent ?parent)
                        (target-class ?class-id)
                        (message-name ?message-name)
                        (parameters ?parameters)
                        (actions ?actions)))
(defrule convert-defmessage-handler-list:empty
         (stage (current parse))
         (object (is-a defmessage-handler)
                 (parameters ?params))
         ?f2 <- (object (is-a list)
                        (name ?params)
                        (parent ?parent)
                        (contents))
         =>
         (unmake-instance ?f2)
         (make-instance ?params of defmessage-handler-parameters
                        (parent ?parent)))

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

(defrule convert-defmessage-handler-list:single-and-wildcard
         (stage (current parse))
         (object (is-a defmessage-handler)
                 (parameters ?params))
         ?f2 <- (object (is-a list)
                        (name ?params)
                        (parent ?parent)
                        (contents $?sf&:(only-single-fields ?sf)
                                  ?wc))
         (object (is-a multifield-variable)
                 (name ?wc))
         =>
         (unmake-instance ?f2)
         (make-instance ?params of defmessage-handler-parameters
                        (parent ?parent)
                        (arguments ?sf)
                        (wildcard-argument ?wc)))

(defrule convert-defmessage-handler-list:single-only
         (stage (current parse))
         (object (is-a defmessage-handler)
                 (parameters ?params))
         ?f2 <- (object (is-a list)
                        (name ?params)
                        (parent ?parent)
                        (contents $?sf&:(only-single-fields ?sf)))
         (object (is-a multifield-variable)
                 (name ?wc))
         =>
         (unmake-instance ?f2)
         (make-instance ?params of defmessage-handler-parameters
                        (parent ?parent)
                        (arguments ?sf)))

(defrule error:convert-defmessage-handler-list:multiple-wildcards
         (stage (current parse))
         (object (is-a defmessage-handler)
                 (parameters ?params))
         (object (is-a list)
                 (name ?params)
                 (contents $?sf&:(not (only-single-fields ?sf))
                           ?wc))
         (object (is-a multifield-variable)
                 (name ?wc))
         =>
         (printout werror "ERROR: extra wildcard defined in defmessage-handler parameter list" crlf)
         (halt))

(defrule error:convert-defmessage-handler-list:out-of-order-wildcard
         (stage (current parse))
         (object (is-a defmessage-handler)
                 (parameters ?params))
         (object (is-a list)
                 (name ?params)
                 (contents $?sf&:(has-wildcard-parameter ?sf)
                           ?sf0))
         (object (is-a singlefield-variable)
                 (name ?sf0))
         =>
         (printout werror "ERROR: wildcard is not defined as the last argument in a defmessage-handler list" crlf)
         (halt))
