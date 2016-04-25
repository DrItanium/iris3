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
(defclass default-facet
  (is-a node)
  (role abstract)
  (pattern-match non-reactive)
  (multislot expressions))

(defclass default
  (is-a default-facet)
  (role concrete)
  (pattern-match reactive)
  ; define the ?DERIVE, ?NONE thingy
  (slot variable
        (type LEXEME)
        (allowed-symbols undefined
                         nil)
        (allowed-strings "?DERIVE"
                         "?NONE")
        (default-dynamic "?DERIVE")))

(defclass default-dynamic
  (is-a default-facet)
  (role concrete)
  (pattern-match reactive))
(defclass basic-slot
  (is-a node)
  (role abstract)
  (pattern-match non-reactive)
  (slot slot-name
        (type SYMBOL)
        (default ?NONE))
  (slot default-value
        (type SYMBOL INSTANCE)
        (allowed-symbols nil)
        (allowed-classes default
                         default-dynamic)
        (default-dynamic nil))
  (multislot type
             (type LEXEME)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-symbols SYMBOL
                              STRING
                              LEXEME
                              FLOAT
                              NUMBER
                              INSTANCE-NAME
                              INSTANCE-ADDRESS
                              INSTANCE
                              EXTERNAL-ADDRESS
                              FACT-ADDRESS)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-symbols
             (type LEXEME)
             (cardinality 1
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-strings
             (type STRING)
             (cardinality 1
                          ?VARIABLE)
             (default-dynamic "?VARIABLE"))
  (multislot allowed-lexemes
             (type LEXEME)
             (cardinality 1 
                          ?VARIABLE)
             (default-dynamic "?VARIABLE"))
  (multislot allowed-integers
             (type INTEGER 
                   STRING)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-float
             (type FLOAT 
                   STRING)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-numbers
             (type NUMBER 
                   STRING)
             (cardinality 1 
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-instance-names
             (type INSTANCE-NAME 
                   STRING)
             (cardinality 1
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-classes
             (type LEXEME)
             (cardinality 1
                          ?VARIABLE)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"))
  (multislot allowed-values
             (type LEXEME 
                   NUMBER
                   INSTANCE-NAME)
             (cardinality 1
                          ?VARIABLE)
             (default-dynamic "?VARIABLE"))
  (multislot range
             (type NUMBER
                   STRING)
             (cardinality 2 
                          2)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"
                              "?VARIABLE"))
  (multislot cardinality
             (type INTEGER
                   STRING)
             (cardinality 2 
                          2)
             (allowed-strings "?VARIABLE")
             (default-dynamic "?VARIABLE"
                              "?VARIABLE"))

  (multislot facets))


(defclass defclass-slot
  (is-a basic-slot)
  (role abstract)
  (pattern-match non-reactive)
  (slot storage
        (type SYMBOL)
        (allowed-symbols local
                         shared))
  (slot access
        (type SYMBOL)
        (allowed-symbols read-write
                         read-only
                         initialize-only))
  (slot propagation 
        (type SYMBOL)
        (allowed-symbols inherit
                         no-inherit))
  (slot source
        (type SYMBOL)
        (allowed-symbols exclusive
                         composite))
  (slot pattern-match
        (type SYMBOL)
        (allowed-symbols reactive
                         non-reactive))
  (slot visibility
        (type SYMBOL)
        (allowed-symbols private
                         public))
  (slot create-accessor
        (type LEXEME)
        (allowed-strings "?NONE")
        (allowed-symbols read
                         write
                         read-write)
        (default-dynamic read-write))
  (slot override-message
        (type LEXEME)
        (allowed-strings "?DEFAULT")
        (default-dynamic "?DEFAULT")))

(defclass defclass-single-slot
  (is-a defclass-slot)
  (role concrete)
  (pattern-match reactive))

(defclass defclass-multislot
  (is-a defclass-slot)
  (role concrete)
  (pattern-match reactive))

(defclass deftemplate-slot
  (is-a basic-slot)
  (role abstract)
  (pattern-match non-reactive))

(defclass deftemplate-single-slot 
  (is-a deftemplate-slot)
  (role concrete)
  (pattern-match reactive))

(defclass deftemplate-multislot
  (is-a deftemplate-slot)
  (role concrete)
  (pattern-match reactive))

(defrule translate-defclass:slot
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before 
                                 ?curr 
                                 $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents slot|single-slot 
                                 ?name 
                                 $?rest))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of defclass-single-slot
                        (slot-name ?name)
                        (parent ?parent)
                        (facets ?rest)))

(defrule translate-defclass:multislot
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before 
                                 ?curr 
                                 $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents multislot 
                                 ?name 
                                 $?rest))
         =>
         (unmake-instance ?q)
         (make-instance ?curr of defclass-multislot 
                        (slot-name ?name)
                        (parent ?parent)
                        (facets ?rest)))

(defrule translate-slot:type
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents type 
                                  ?first
                                  $?types))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (facets $?a $?b)
                          (type ?first
                                $?types)))

(defrule translate-slot:range
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents range 
                                  ?from 
                                  ?to))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (facets $?a $?b)
                          (range ?from
                                 ?to)))

(defrule translate-slot:cardinality
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents cardinality 
                                  ?from 
                                  ?to))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f
                          (facets $?a $?b)
                          (cardinality ?from
                                       ?to)))

(defrule translate-slot:allowed-symbols
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-symbols
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-symbols ?first 
                                           $?rest)))

(defrule translate-slot:allowed-strings
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-strings
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-strings ?first 
                                           $?rest)))

(defrule translate-slot:allowed-lexemes
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-lexemes
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-lexemes ?first 
                                           $?rest)))

(defrule translate-slot:allowed-integers
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-integers
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-integers ?first 
                                            $?rest)))

(defrule translate-slot:allowed-floats
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-floats
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-floats ?first 
                                          $?rest)))

(defrule translate-slot:allowed-numbers
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-numbers
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-numbers ?first 
                                           $?rest)))
(defrule translate-slot:allowed-instance-names
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-instance-names
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-instance-names ?first 
                                                  $?rest)))

(defrule translate-slot:allowed-classes
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-classes
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-classes ?first 
                                           $?rest)))
(defrule translate-slot:allowed-values
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents allowed-values
                                  ?first
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?b)
                          (allowed-values ?first 
                                          $?rest)))

(defrule translate-slot:default:expression
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default
                                  $?expressions))
         =>
         (unmake-instance ?f2)
         (make-instance ?curr of default
                        (parent ?parent)
                        (variable nil)
                        (expressions ?expressions))
         (modify-instance ?f
                          (facets ?a ?b)
                          (default-value ?curr)))

(defrule translate-slot:default:none-derive
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default
                                  ?c))
         ?f3 <- (object (is-a singlefield-variable)
                        (name ?c)
                        (value ?value&"?NONE"|"?DERIVE"))
         =>
         (unmake-instance ?f2)
         (make-instance ?curr of default
                        (parent ?parent)
                        (variable ?c))
         (modify-instance ?f
                          (facets ?a ?b)
                          (default-value ?curr)))


(defrule translate-slot:default-dynamic:expression
         (stage (current parse))
         ?f <- (object (is-a basic-slot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default-dynamic
                                  $?expressions))
         =>
         (unmake-instance ?f2)
         (make-instance ?curr of default-dynamic
                        (parent ?parent)
                        (expressions ?expressions))
         (modify-instance ?f (facets ?a ?b)
                          (default-value ?curr)))

(defrule translate-slot:defclass-slot:storage
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents storage 
                                  ?storage))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (storage ?storage)))

(defrule translate-slot:defclass-slot:access
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents access 
                                  ?access))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (access ?access)))

(defrule translate-slot:defclass-slot:propagation
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents propagation 
                                  ?propagation))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (propagation ?propagation)))

(defrule translate-slot:defclass-slot:source
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents source 
                                  ?source))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (source ?source)))

(defrule translate-slot:defclass-slot:pattern-match
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents pattern-match 
                                  ?pattern-match))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (pattern-match ?pattern-match)))
(defrule translate-slot:defclass-slot:visibility
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents visibility 
                                  ?visibility))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (visibility ?visibility)))

(defrule translate-slot:defclass-slot:create-accessor
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents create-accessor 
                                  ?create-accessor))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (create-accessor ?create-accessor)))
(defrule translate-slot:defclass-slot:override-message
         (stage (current parse))
         ?f <- (object (is-a defclass-slot)
                       (facets $?a
                               ?b
                               $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents override-message 
                                  ?override-message))
         =>
         (unmake-instance ?f2)
         (modify-instance ?f 
                          (facets ?a ?c)
                          (override-message ?override-message)))

(defrule translate-deftemplate:slot
         (stage (current parse))
         ?f <- (object (is-a deftemplate)
                       (slots $?a 
                              ?slot
                              $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?slot)
                        (contents slot 
                                  ?name
                                  $?facets))
         =>
         (unmake-instance ?f2)
         (make-instance ?slot of deftemplate-single-slot
                        (parent ?parent)
                        (slot-name ?name)
                        (facets ?facets)))

(defrule translate-deftemplate:multislot
         (stage (current parse))
         ?f <- (object (is-a deftemplate)
                       (slots $?a 
                              ?slot
                              $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?slot)
                        (contents multislot 
                                  ?name
                                  $?facets))
         =>
         (unmake-instance ?f2)
         (make-instance ?slot of deftemplate-multislot
                        (parent ?parent)
                        (slot-name ?name)
                        (facets ?facets)))
