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

(defclass conditional-element
  (is-a thing)
  (slot binding
        (type LEXEME
              INSTANCE-NAME))
  (multislot contents))
(defclass defrule
  (is-a thing
        has-title
        has-comment)
  (slot salience
        (type INTEGER
              INSTANCE-NAME)
        (range -10000
               10000)
        (default-dynamic 0))
  (slot auto-focus
        (type SYMBOL
              INSTANCE-NAME)
        (allowed-symbols FALSE
                         TRUE))
  (multislot conditional-elements)
  (multislot body))

(defclass defrule-declaration
  "Used as a layer of indirection for salience and auto-focus fields"
  (is-a thing)
  (slot salience 
        (type INTEGER
              INSTANCE-NAME)
        (range -10000
               10000)
        (default-dynamic 0))
  (slot auto-focus
        (type SYMBOL
              INSTANCE-NAME)
        (allowed-symbols FALSE
                         TRUE)
        (default-dynamic FALSE)))
(defrule capture-declarations:both:salience-first
         (declare (salience ?*priority:three*))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents declare ?salience ?auto-focus)
                       (name ?name)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?salience)
                        (contents salience ?s-value))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus)
                        (contents auto-focus ?af-value))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defrule-declaration
                        (parent ?parent)
                        (salience ?s-value)
                        (auto-focus ?af-value)))

(defrule capture-declarations:both:salience-second
         (declare (salience ?*priority:three*))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents declare ?auto-focus ?salience)
                       (name ?name)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?salience)
                        (contents salience ?s-value))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus)
                        (contents auto-focus ?af-value))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defrule-declaration
                        (parent ?parent)
                        (salience ?s-value)
                        (auto-focus ?af-value)))

(defrule capture-declarations:auto-focus-only
         (declare (salience ?*priority:three*))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents declare ?auto-focus)
                       (name ?name)
                       (parent ?parent))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus)
                        (contents auto-focus ?af-value))
         =>
         (unmake-instance ?f ?f3)
         (make-instance ?name of defrule-declaration
                        (parent ?parent)
                        (auto-focus ?af-value)))

(defrule capture-declarations:salience-only
         (declare (salience ?*priority:three*))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents declare ?salience)
                       (name ?name)
                       (parent ?parent))
         ?f2 <- (object (is-a list)
                        (name ?salience)
                        (contents salience ?s-value))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defrule-declaration
                        (parent ?parent)
                        (salience ?s-value)))

(defrule translate-defrule:comment:no-decl
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule 
                                 ?rule-name 
                                 ?comment
                                 $?conditional-elements
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defrule 
                        (title ?rule-name)
                        (comment ?cvalue)
                        (parent ?parent)
                        (body ?body)
                        (conditional-elements ?conditional-elements)))

(defrule translate-defrule:no-comment:no-decl
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule
                                 ?rule-name
                                 $?conditional-elements&:(no-strings-in-list ?conditional-elements)
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
                        (title ?rule-name)
                        (parent ?parent)
                        (conditional-elements ?conditional-elements)
                        (body ?body)))

(defrule translate-defrule:comment:decl
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule 
                                 ?rule-name 
                                 ?comment
                                 ?decl
                                 $?conditional-elements
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?f4 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         ?f2 <- (object (is-a defrule-declaration)
                        (name ?decl)
                        (salience ?salience)
                        (auto-focus ?auto-focus))
         =>
         (unmake-instance ?f ?f2 ?f4)
         (make-instance ?name of defrule 
                        (title ?rule-name)
                        (auto-focus ?auto-focus)
                        (salience ?salience)
                        (comment ?cvalue)
                        (parent ?parent)
                        (body ?body)
                        (conditional-elements ?conditional-elements)))

(defrule translate-defrule:no-comment:decl
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule
                                 ?rule-name
                                 ?decl
                                 $?conditional-elements&:(no-strings-in-list ?conditional-elements)
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a defrule-declaration)
                        (name ?decl)
                        (salience ?salience)
                        (auto-focus ?auto-focus))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defrule
                        (title ?rule-name)
                        (auto-focus ?auto-focus)
                        (salience ?salience)
                        (parent ?parent)
                        (conditional-elements ?conditional-elements)
                        (body ?body)))


(defrule update-auto-focus
         (declare (salience 1))
         (stage (current parse))
         (object (is-a defrule)
                 (name ?name)
                 (auto-focus ?focus))
         ?f2 <- (object (is-a thing)
                        (name ?focus)
                        (parent ~?name))
         =>
         (modify-instance ?f2 (parent ?name)))
(defrule update-salience
         (declare (salience 1))
         (stage (current parse))
         (object (is-a defrule)
                 (name ?name)
                 (salience ?salience))
         ?f2 <- (object (is-a thing)
                        (name ?salience)
                        (parent ~?name))
         =>
         (modify-instance ?f2 (parent ?name)))

(defrule translate-conditional-element:no-binding
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (name ?parent)
                       (conditional-elements $?before ?list $?after))
         ?q <- (object (is-a list)
                       (name ?list)
                       (contents $?contents))
         =>
         (unmake-instance ?q)
         (make-instance ?list of conditional-element
                        (parent ?parent)
                        (contents ?contents)))

;TODO: handle multiline strings
(defrule translate-conditional-element:binding
         "Before we construct defrule's we have to capture bound conditional-elements to prevent a matching ambiguity in a defrule between a comment and a bound conditional-element (both of them will show up as strings)"
         (declare (salience ?*priority:three*))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents $?before 
                                 ?var <- ?list 
                                 $?after)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?list)
                        (contents $?contents))
         =>
         (unmake-instance ?f2)
         (make-instance ?list of conditional-element
                        (parent ?parent)
                        (binding ?var)
                        (contents ?contents))
         (modify-instance ?f
                          (contents $?before
                                    ?list
                                    $?after)))
