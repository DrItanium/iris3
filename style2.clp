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

(deftemplate lexer
             (slot file
                   (type LEXEME)
                   (default ?NONE))
             (slot router
                   (type SYMBOL)
                   (default ?NONE))
             (slot fully-loaded
                   (type SYMBOL)
                   (allowed-symbols FALSE 
                                    TRUE))
             (multislot elements))
(defclass list
  (is-a USER)
  (slot parent
        (type SYMBOL)
        (default ?NONE))
  (multislot contents))
(defclass match
  (is-a USER)
  (slot parent
        (type SYMBOL)
        (default ?NONE))
  (slot binding
        (type LEXEME))
  (multislot contents))

(defclass defrule
  (is-a USER)
  (slot rule-name
        (type SYMBOL)
        (default ?NONE))
  (slot comment
        (type STRING))
  (slot parent
        (type SYMBOL)
        (default ?NONE))
  (multislot matches)
  (multislot body))

(defrule open-file
         ?f <- (open ?path)
         =>
         (retract ?f)
         (bind ?name (gensym*))
         (if (open ?path 
                   ?name 
                   "r") then
           (assert (lexer (file ?path)
                          (router ?name)
                          (elements (read ?name))))
           else
           (printout werror "couldn't open " ?path crlf)))

(defrule read-element
         ?f <- (lexer (file ?path)
                      (router ?name)
                      (elements $?elements)
                      (fully-loaded FALSE))
         =>
         (bind ?next (read ?name))
         (if (neq ?next 
                  EOF) then
           (modify ?f (elements ?elements 
                                ?next))
           else
           (modify ?f (fully-loaded TRUE))))

(defrule mark-sub-list
         (declare (salience 1))
         ?f <- (lexer (elements $?before 
                                "(" $?inner&:(and (not (member$ ")" ?inner))
                                                  (not (member$ "(" ?inner))) 
                                ")" 
                                $?after)
                      (router ?parent))
         =>
         (modify ?f (elements ?before 
                              (instance-name (make-instance of list
                                                            (parent ?parent)
                                                            (contents ?inner)))
                              ?after)))

(defrule finished-completely
         ?f <- (lexer (file ?path)
                      (router ?name)
                      (elements $?elements)
                      (fully-loaded TRUE))
         =>
         (close ?name)
         (retract ?f)
         (assert (parse)))
(defrule translate-defrule:comment
         (declare (salience 1))
         (parse)
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 $?matches
                                 =>
                                 $?body))
         =>
         (unmake-instance ?f)
         (make-instance of defrule 
                        (rule-name ?name)
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))
(defrule translate-defrule:no-comment
         (declare (salience 1))
         (parse)
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 $?matches
                                 =>
                                 $?body))
         =>
         (unmake-instance ?f)
         (make-instance of defrule
                        (rule-name ?name)
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))

(defrule translate-match:no-binding
         (parse)
         ?f <- (object (is-a defrule)
                       (matches $?before ?list $?after))
         ?q <- (object (is-a list)
                       (name ?list)
                       (parent ?parent)
                       (contents $?contents))
         =>
         (unmake-instance ?q)
         (modify-instance ?f (matches $?before 
                                      (instance-name (make-instance of match
                                                                    (parent ?parent)
                                                                    (contents ?contents))) 
                                      $?after)))

(defrule translate-match:binding
         (parse)
         ?f <- (object (is-a defrule)
                       (matches $?before ?var <- ?list $?after))
         ?q <- (object (is-a match)
                       (name ?list))
         =>
         (modify-instance ?q 
                          (binding ?var))
         (modify-instance ?f
                          (matches ?before ?list ?after)))
