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

; create$ and other multifield operations 
(defclass builtin-function
  "Represents a builtin function"
  (is-a thing
        has-title)
  (multislot contents))

(deftemplate defbuiltin
             (slot function
                   (type SYMBOL)
                   (default ?NONE))
             (slot arguments
                   (type SYMBOL
                         INTEGER)
                   (default ?NONE)))

(deffunction generate-ah-forms
             (?input)
             (create$ ?input
                      (sym-cat a ?input)
                      (sym-cat ?input h)
                      (sym-cat a ?input h)))
(deffunction single-input-marker
             (?a) 
             (create$ ?a 1))
(deffunction two-or-more-marker 
             (?a)
             (create$ ?a 2+))
(deffunction one-or-more-marker
             (?a)
             (create$ ?a +))
(deffunction zero-or-more-marker
             (?a)
             (create$ ?a *))
(deffunction zero-or-one-input-marker
             (?a)
             (create$ ?a 
                      zero-or-one))
(deffunction siso-input
             ($?elements)
             (apply$ single-input-marker
                     ?elements))
(deffunction one-or-more-func
             ($?elements)
             (apply$ one-or-more-marker
                     ?elements))
(deffunction zero-or-more-func
             ($?elements)
             (apply$ zero-or-more-marker
                     ?elements))
(deffunction two-or-more-func
             ($?elements)
             (apply$ two-or-more-marker
                     ?elements))
(deffunction zero-or-one-inputs
             ($?elements)
             (apply$ zero-or-one-input-marker 
                     ?elements))

(deffacts defsiso-functions
          (builtin-functions (zero-or-one-inputs exit)
                             (siso-input length$
                                         rest$
                                         first$
                                         explode$
                                         implode$
                                         expand$
                                         eval
                                         build
                                         lowcase
                                         upcase
                                         str-length
                                         check-syntax
                                         string-to-field
                                         integer
                                         float
                                         abs
                                         rad-deg
                                         grad-deg
                                         deg-rad
                                         (generate-ah-forms cos)
                                         (generate-ah-forms cot)
                                         (generate-ah-forms csc)
                                         (generate-ah-forms sec)
                                         (generate-ah-forms sin)
                                         (generate-ah-forms tan) 
                                         log
                                         log10
                                         round
                                         setgen
                                         seed
                                         length
                                         get-function-restrictions
                                         deftemplate-module
                                         deftemplate-slot-names
                                         assert-string
                                         fact-index
                                         fact-existp
                                         fact-relation
                                         fact-slot-names
                                         type
                                         defgeneric-module
                                         class
                                         class-existp
                                         class-abstractp
                                         class-reactivep
                                         set-class-defaults-mode
                                         not)
                             (two-or-more-func + 
                                               - 
                                               * 
                                               / 
                                               eq 
                                               ; these two are special cases
                                               ;= 
                                               ;<> 
                                               div
                                               neq 
                                               and 
                                               or)
                             (one-or-more-func assert)
                             (zero-or-more-func create$)))

(defrule generate-siso-builtin-function
         (stage (current init))
         ?f <- (builtin-functions ?fn ?args $?rest)
         =>
         (retract ?f)
         (assert (builtin-functions $?rest)
                 (defbuiltin (function ?fn)
                             (arguments ?args))))

(defrule generate-siso-builtin-function:done
         (stage (current init))
         ?f <- (builtin-functions)
         =>
         (retract ?f))
(defrule retract-defbuiltin 
         (stage (current init))
         ?f <- (defbuiltin (function ?name))
         (test (member$ (sym-cat build- ?name -node)
                        (get-defrule-list)))
         =>
         (retract ?f))

(defrule process-siso-function
         (stage (current init))
         ?f <- (defbuiltin (function ?name)
                           (arguments 1))
         (test (not (member$ (sym-cat build- ?name -node)
                             (get-defrule-list))))
         =>
         (retract ?f)
         (build (format nil "(defrule build-%s-node 
                                      \"construct a %s node from the given list\"
                                      (stage (current associate))
                                      ?f <- (object (is-a list)
                                                    (contents %s
                                                              ?c)
                                                    (parent ?parent)
                                                    (name ?name))
                                      =>
                                      (unmake-instance ?f)
                                      (make-instance ?name of builtin-function
                                                     (parent ?parent)
                                                     (title %s)
                                                     (contents $?c)))" 
                        ?name 
                        ?name 
                        ?name 
                        ?name))
(build (format nil 
               "(defrule build-%s-node:too-many-args 
                         \"The number of arguments passed to %s isn't exactly one!\"
                         (stage (current associate))
                         ?f <- (object (is-a list)
                                       (contents %s
                                                 ?first
                                                 ?second
                                                 $?rest)
                                       (parent ?parent)
                                       (name ?name))
                         =>
                         (printout t \"ERROR: %s expected one argument but got '(%s \" ?first \" \" ?second \" ....)' instead!\" crlf))"
               ?name
               ?name
               ?name
               ?name
               ?name))
         (build (format nil 
                        "(defrule build-%s-node:too-few-args
                                  \"The number of arguments passed to %s isn't exactly one!\"
                                  (stage (current associate))
                                  ?f <- (object (is-a list)
                                                (contents %s)
                                                (parent ?parent)
                                                (name ?name))
                                  =>
                                  (printout t \"ERROR: %s expected one argument but got zero instead!\" crlf))" 
                        ?name
                        ?name
                        ?name
                        ?name)))


(defrule process-zero-or-more-arg-functions
         (stage (current init))
         ?f <- (defbuiltin (function ?name)
                           (arguments *))
         =>
         (retract ?f)
         (build (format nil "(defrule build-%s-node 
                                      \"construct a %s node from the given list\"
                                      (stage (current associate))
                                      ?f <- (object (is-a list)
                                                    (contents %s
                                                              $?c)
                                                    (parent ?parent)
                                                    (name ?name))
                                      =>
                                      (unmake-instance ?f)
                                      (make-instance ?name of builtin-function
                                                     (parent ?parent)
                                                     (title %s)
                                                     (contents $?c)))" 
                        ?name 
                        ?name 
                        ?name 
                        ?name)))

(defrule process-one-or-more-arg-functions
         (stage (current init))
         ?f <- (defbuiltin (function ?name)
                           (arguments +))
         =>
         (retract ?f)
         (build (format nil "(defrule build-%s-node 
                                      \"construct a %s node from the given list\"
                                      (stage (current associate))
                                      ?f <- (object (is-a list)
                                                    (contents %s
                                                              ?first
                                                              $?rest)
                                                    (parent ?parent)
                                                    (name ?name))
                                      =>
                                      (unmake-instance ?f)
                                      (make-instance ?name of builtin-function
                                                     (parent ?parent)
                                                     (title %s)
                                                     (contents ?first 
                                                               $?rest)))" 
                                      ?name 
                                      ?name 
                                      ?name 
                                      ?name))
         (build (format nil "(defrule build-%s-node:too-few-arguments
         \"construct a %s node from the given list\"
         (stage (current associate))
         ?f <- (object (is-a list)
                       (contents %s)
                       (parent ?parent)
                       (name ?name))
         =>
         (printout t \"ERROR: %s expected one or more arguments, zero provided!\" crlf))"
                        ?name
                        ?name
                        ?name
                        ?name)))

(defrule process-two-or-more-arg-functions
         (stage (current init))
         ?f <- (defbuiltin (function ?name)
                           (arguments 2+))
         =>
         (retract ?f)
         (build (format nil "(defrule build-%s-node 
                                      \"construct a %s node from the given list\"
                                      (stage (current associate))
                                      ?f <- (object (is-a list)
                                                    (contents %s
                                                              ?first
                                                              ?second
                                                              $?rest)
                                                    (parent ?parent)
                                                    (name ?name))
                                      =>
                                      (unmake-instance ?f)
                                      (make-instance ?name of builtin-function
                                                     (parent ?parent)
                                                     (title %s)
                                                     (contents ?first 
                                                               ?second
                                                               $?rest)))" 
                        ?name 
                        ?name 
                        ?name 
                        ?name))
(build (format nil "(defrule build-%s-node:no-args
\"construct a %s node from the given list\"
(stage (current associate))
?f <- (object (is-a list)
              (contents %s)
              (parent ?parent)
              (name ?name))
=>
(printout t \"ERROR: %s expected two or more arguments, zero provided!\" crlf))"
               ?name
               ?name
               ?name
               ?name))
         (build (format nil "(defrule build-%s-node:one-arg
         \"construct a %s node from the given list\"
         (stage (current associate))
         ?f <- (object (is-a list)
                       (contents %s 
                                 ?)
                       (parent ?parent)
                       (name ?name))
         =>
         (printout t \"ERROR: %s expected two or more arguments, one provided!\" crlf))"
                        ?name
                        ?name
                        ?name
                        ?name)))

(defrule process-zero-or-one-arg-functions
         (stage (current init))
         ?f <- (defbuiltin (function ?name)
                           (arguments zero-or-one))
         =>
         (retract ?f)
         (build (format nil "(defrule build-%s-node:zero-args
                                      \"construct a %s node from the given list\"
                                      (stage (current associate))
                                      ?f <- (object (is-a list)
                                                    (contents %s)
                                                    (parent ?parent)
                                                    (name ?name))
                                      =>
                                      (unmake-instance ?f)
                                      (make-instance ?name of builtin-function
                                                     (parent ?parent)
                                                     (title %s)
                                                     (contents)))"
                        ?name 
                        ?name 
                        ?name 
                        ?name))
(build (format nil "(defrule build-%s-node:one-arg
\"construct a %s node from the given list\"
(stage (current associate))
?f <- (object (is-a list)
              (contents %s ?first)
              (parent ?parent)
              (name ?name))
=>
(unmake-instance ?f)
(make-instance ?name of builtin-function
               (parent ?parent)
               (title %s)
               (contents ?first)))"
               ?name 
               ?name 
               ?name 
               ?name))

         (build (format nil 
                        "(defrule build-%s-node:too-many-args 
                                  \"The number of arguments passed to %s isn't exactly one or zero!\"
                                  (stage (current associate))
                                  ?f <- (object (is-a list)
                                                (contents %s
                                                          ?first
                                                          ?second
                                                          $?rest)
                                                (parent ?parent)
                                                (name ?name))
                                  =>
                                  (printout t \"ERROR: %s expected one (or zero) arguments but got '(%s \" ?first \" \" ?second \" ....)' instead!\" crlf))"
                        ?name
                        ?name
                        ?name
                        ?name
                        ?name)))
