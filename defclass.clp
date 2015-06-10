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
(defclass message-handler-documentation
  (is-a thing)
  (slot handler-name
        (type SYMBOL)
        (default ?NONE))
  (slot handler-type
        (type SYMBOL)
        (allowed-symbols primary 
                         around 
                         before 
                         after)))
(defclass defclass
  (is-a thing
        has-comment)
  (slot class-name
        (type SYMBOL)
        (default ?NONE))
  (multislot inherits-from)
  (slot role
        (type SYMBOL)
        (allowed-symbols concrete 
                         abstract))
  (slot pattern-match
        (type SYMBOL)
        (allowed-symbols reactive 
                         non-reactive))
  (multislot contents))

(defrule translate-defclass:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defclass 
                                 ?class-name
                                 ?comment
                                 ?is-a
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         ?f3 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f2 <- (object (is-a list)
                        (name ?is-a)
                        (contents is-a $?ia))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defclass
                        (class-name ?class-name)
                        (parent ?parent)
                        (comment ?cvalue)
                        (inherits-from ?ia)
                        (contents ?rest)))


(defrule translate-defclass:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defclass 
                                 ?class-name
                                 ?is-a
                                 $?rest)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a list)
                        (name ?is-a)
                        (contents is-a $?ia))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defclass
                        (class-name ?class-name)
                        (parent ?parent)
                        (inherits-from ?ia)
                        (contents ?rest)))



(defrule translate-defclass:populate-role
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents ?first
                                 $?rest))
         ?q <- (object (is-a list)
                       (name ?first)
                       (contents role ?role))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (role ?role)
                          (contents ?rest)))
(defrule translate-defclass:populate-pattern-match
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents ?first 
                                 $?rest))
         ?q <- (object (is-a list)
                       (name ?first)
                       (contents pattern-match ?pattern-match))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (pattern-match ?pattern-match)
                          (contents ?rest)))

(defrule translate-defclass:convert-message-handler-documentation:no-type
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before ?curr $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents message-handler ?name))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of message-handler-documentation
                        (parent ?parent)
                        (handler-name ?name)))
(defrule translate-defclass:convert-message-handler-documentation:type
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before ?curr $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents message-handler ?name ?type))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of message-handler-documentation
                        (parent ?parent)
                        (handler-name ?name)
                        (handler-type ?type)))



