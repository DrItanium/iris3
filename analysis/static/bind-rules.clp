
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
                 (contents $?contents))
         (object (is-a singlefield-variable)
                 (name ?var)
                 (value ?val))
         =>
         (printout t "VIOLATION: the statement (bind " ?val " ...) contains a create$ call!" crlf
                   "           This is not necessary as bind takes in a variable number of arguments!" crlf
                   "           Just place the contents directly!" crlf))
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
         (printout t "VIOLATION: the statement (bind " ?val " ...) contains an expand$ call!" crlf
                   "           This is not necessary as bind will automatically expand multifields as necessary!" crlf))

