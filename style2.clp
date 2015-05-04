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
(defclass thing
  (is-a USER)
  (slot parent 
        (type SYMBOL)
        (default ?NONE)))
(defclass list
  (is-a thing)
  (multislot contents))
(defclass match
  (is-a thing)
  (slot binding
        (type LEXEME))
  (multislot contents))

(defclass defrule
  (is-a thing)
  (slot comment
        (type STRING))
  (multislot matches)
  (multislot body))

(defclass deffunction
  (is-a thing)
  (slot comment
        (type STRING))
  (multislot arguments)
  (multislot body))

(defclass defgeneric
  (is-a thing)
  (slot comment
        (type STRING)))

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
         (make-instance ?name of defrule 
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))
(deffunction no-strings-in-list
             (?f) 
             (progn$ (?a ?f) 
                     (if (stringp ?a) then 
                       (return FALSE)))
             (return TRUE))
(defrule translate-defrule:no-comment
         (declare (salience 1))
         (parse)
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
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


(defrule translate-deffunction:comment
         (parse)
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents deffunction 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 ?args
                                 $?body))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j)
         (unmake-instance ?f)
         (make-instance ?name of deffunction
                        (parent ?parent)
                        (comment ?comment)
                        (arguments ?a)
                        (body ?body)))
(defrule translate-deffunction:no-comment
         (parse)
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents deffunction 
                                 ?name 
                                 ?args
                                 $?body))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j)
         (unmake-instance ?f)
         (make-instance ?name of deffunction
                        (parent ?parent)
                        (arguments ?a)
                        (body ?body)))
