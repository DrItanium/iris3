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

(defclass match
  (is-a node)
  (slot binding
        (type LEXEME
              INSTANCE-NAME))
  (multislot contents))
(defclass defrule
  (is-a node
        has-comment)
  (slot rule-name
        (type SYMBOL)
        (default ?NONE))
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
  (multislot matches)
  (multislot body))




(defrule translate-defrule:comment
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (index ?index)
                       (contents defrule 
                                 ?rule-name 
                                 ?comment
                                 $?matches
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
                        (index ?index)
                        (rule-name ?rule-name)
                        (comment ?cvalue)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (index ?index)
                       (contents defrule
                                 ?rule-name
                                 ?match&:(not (string-classp ?match))
                                 $?matches
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
                        (index ?index)
                        (rule-name ?rule-name)
                        (parent ?parent)
                        (matches ?match 
                                 ?matches)
                        (body ?body)))



(defrule translate-match:no-binding
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (name ?parent)
                       (matches $?before ?list $?after))
         (object (is-a list)
                 (name ?list)
                 (index ?index)
                 (contents $?contents))
         =>
         (unmake-instance ?list)
         (make-instance ?list of match
                        (index ?index)
                        (parent ?parent)
                        (contents ?contents)))

;TODO: handle multiline strings
(defrule translate-match:binding
         "Before we construct defrule's we have to capture bound matches to prevent a matching ambiguity in a defrule between a comment and a bound match (both of them will show up as strings)"
         (declare (salience ?*priority:three*))
         (stage (current parse))
         (declaration parsed for ?rule)
         (object (is-a defrule)
                 (name ?rule)
                 (matches $?before ?var <- ?list $?after))
         ?f2 <- (object (is-a list)
                        (name ?list)
                        (index ?index)
                        (contents $?contents))
         =>
         (unmake-instance ?f2)
         (modify-instance ?rule
                          (matches $?before
                                    (make-instance ?list of match
                                                   (index ?index)
                                                   (parent ?rule)
                                                   (binding ?var)
                                                   (contents ?contents))
                                    $?after)))

(defrule check-defrule-decls:salience
         "Check the (declare ) list at the top of the match set"
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (matches ?decl
                                $?)
                       (name ?rule))
         (object (is-a list)
                 (name ?decl)
                 (contents declare
                           $?before
                           ?salience
                           $?after))
         (object (is-a list)
                 (name ?salience)
                 (contents salience
                           ?s))
         (not (exists (declaration parsed for ?rule)))
         =>
         (assert (please retract ?decl))
         (unmake-instance ?salience)
         (modify-instance ?decl
                          (contents ?before ?after))
         (modify-instance ?f 
                          (salience ?s)))

(defrule check-defrule-decls:auto-focus
         "Check the (declare ) list at the top of the match set"
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (matches ?decl
                                $?)
                       (name ?rule))
         (object (is-a list)
                 (name ?decl)
                 (contents declare
                           $?before
                           ?af
                           $?after))
         (object (is-a list)
                 (name ?af)
                 (contents auto-focus 
                           ?s))
         (not (exists (declaration parsed for ?rule)))
         =>
         (assert (please retract ?decl))
         (unmake-instance ?af)
         (modify-instance ?decl
                          (contents ?before ?after))
         (modify-instance ?f 
                          (auto-focus ?s)))

(defrule check-defrule-decls:illegal-value
 (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (matches ?decl
                                $?)
                       (rule-name ?rn)
                       (name ?ne))
         (object (is-a list)
                 (name ?decl)
                 (contents declare
                           $?before
                           ?target
                           $?after))
         (object (is-a list)
                 (name ?target)
                 (contents ?title&~salience&~auto-focus
                           $?))
         =>
         (printout werror 
                   "ERROR: found an illegal field " ?title 
                   " in the declare statement of rule " ?rn "( " ?ne " )"  crlf)
         (halt))



(defrule check-defrule-decls:retract-declare-statement
         "When done pulling the contents of the declaration out, retract it"
         (declare (salience 1))
         ?f <- (please retract ?decl)
         (object (is-a list)
                 (name ?decl)
                 (contents))
         (object (is-a defrule)
                 (matches ?decl 
                          $?)
                 (name ?rule))
         =>
         (retract ?f)
         (unmake-instance ?decl)
         (assert (declaration parsed for ?rule))
         (slot-delete$ ?rule
                       matches
                       1 1))


