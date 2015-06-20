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

(defclass reference
  "An indirect reference to something else, useful for deffunctions and arguments"
  (is-a scalar-thing)
  (slot expand
        (type SYMBOL)
        (allowed-symbols FALSE TRUE)))

(defrule reference-deffunction-arguments
         (stage (current optimize))
         (object (is-a deffunction)
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

(defrule reference-deffunction-arguments:last:multifield:exact
         (stage (current optimize))
         (object (is-a deffunction)
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

(defrule reference-deffunction-arguments:last:multifield:mismatch
         (stage (current optimize))
         (object (is-a deffunction)
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


(defrule reference-global-variables:singlefield:exact
         "Associate global variables even if they aren't of the exact same type"
         (stage (current optimize))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a singlefield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-global-variable) ; we could be looking at an expansion version !
                       (value ?cvalue)
                       (name ?name&~?var)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)))

(defrule reference-global-variables:singlefield:mismatch
         "Associate global variables even if they aren't of the exact same type"
         (stage (current optimize))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a multifield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a singlefield-global-variable) ; we could be looking at an expansion version !
                       (value ?value2&:(eq ?cvalue
                                           (format nil "$%s" ?value2)))
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)))

(defrule reference-global-variables:multifield:exact-match
         "Associate multifield global variables"
         (stage (current optimize))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a multifield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-global-variable) ; we could be looking at an expansion version !
                       (value ?cvalue)
                       (name ?name&~?var)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)
                        (expand TRUE)))

(defrule reference-global-variables:multifield:mismatch
         "Associate multifield global variables"
         (stage (current optimize))
         (object (is-a defglobal)
                 (assignments $? ?a $?))
         (object (is-a defglobal-assignment)
                 (name ?a)
                 (variable ?var))
         (object (is-a singlefield-global-variable)
                 (name ?var)
                 (value ?cvalue))
         ?f <- (object (is-a multifield-global-variable) ; we could be looking at an expansion version !
                       (value ?value2&:(eq ?value2 
                                           (format nil "$%s" ?cvalue)))
                       (name ?name)
                       (parent ?p))
         =>
         (unmake-instance ?f)
         (make-instance ?name of reference
                        (value ?var)
                        (parent ?p)
                        (expand TRUE)))
         
