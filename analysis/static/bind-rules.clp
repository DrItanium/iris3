
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

; rules-relating to bind operations, common mistakes

(defrule create$-found-in-bind
         "It is a common mistake of mine to put (bind ?something (create$ ?a ?b ?c)) when you can do
         (bind ?something ?a ?b ?c). So point that out"
         (stage (current static-analysis))
         ?f <- (object (is-a bind)
                       (variable ?var)
                       (value $? ?a $?))
         (object (is-a builtin-function)
                 (name ?a)
                 (title create$)
                 ; at least one element
                 (contents ?first $?))
         (object (is-a singlefield-variable)
                 (name ?var)
                 (value ?val))
         =>
         (violation t "The statement (bind " ?val " ... (create$ " (send ?first representation) " ... ) ... ) contains a create$ call!" crlf
                    "This is not necessary as bind takes in a variable number of arguments!" crlf
                    "Just place the contents directly like: " crlf crlf
                    "(bind " ?val " ... " (send ?first representation) " ... )"))

(defrule expand$-found-in-bind
         "It is not necessary to expand a multifield in a bind, that is done automatically"
         (stage (current static-analysis))
         ?f <- (object (is-a bind)
                       (variable ?var)
                       (value $? ?a $?))
         (object (is-a builtin-function)
                 (name ?a)
                 (title expand$))
         (object (is-a singlefield-variable)
                 (name ?var)
                 (value ?val))
         =>
         (violation t "The statement (bind " ?val " ...) contains an expand$ call!" crlf
                    "This is not necessary as bind will automatically expand multifields as necessary!"))

(defrule empty-bind-found
         "While it is possible to undefine variables, it shouldn't be done as it isn't clean."
         (stage (current static-analysis))
         ?f <- (object (is-a bind)
                       (variable ?var)
                       (value))
         (object (is-a singlefield-variable)
                 (name ?var)
                 (value ?val))
         =>
         (potential-violation t "The statement (bind " ?val ") is meant to unbind the " ?val " variable." crlf
                              "This is questionable if it is the correct thing to do as it can mean there are attempts at premature optimization" crlf))
