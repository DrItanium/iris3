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

; questionable usage of builtin functions

(defrule found-a-call-to-exit
         "Exit should never be called from a rule or function, halt should be used instead. 
         Use of exit prevents proper clean up if something goes totally wrong"
         (stage (current static-analysis))
         (object (is-a builtin-function)
                 (title exit))
         =>
         (printout t "VIOLATION: Found use of exit! Exit should never be called within a run only at the REPL!" crlf
                   "           Use halt instead!" crlf))

(defrule prefer-usage-of-not-neq:matching-constants
         "If we see (or (eq ?a ?b) (eq ?a ?c)) then we should note that it really should be (not (neq ?a ?b ?c))"
         (stage (current static-analysis))
         (object (is-a builtin-function)
                 (title or)
                 (contents $? ?a $? ?b $?))
         (object (is-a builtin-function)
                 (name ?a)
                 (title eq)
                 (contents ?a0 ?a1))
         (object (is-a builtin-function)
                 (name ?b)
                 (title eq)
                 (contents ?a0 ?a2&~?a1))
         =>
         (printout t "VIOLATION: Found (or (eq " (send ?a0 representation) " " (send ?a1 representation) ")" crlf
                   "                      (eq " (send ?a0 representation) " " (send ?a2 representation) "))" crlf crlf
                   "           Use (not (neq " (send ?a0 representation) " " (send ?a1 representation) " " (send ?a2 representation) ")) instead!" crlf))

(defrule prefer-usage-of-not-neq
         "If we see (or (eq ?a ?b) (eq ?a ?c)) then we should note that it really should be (not (neq ?a ?b ?c))"
         (stage (current static-analysis))
         (object (is-a builtin-function)
                 (title or)
                 (contents $? ?a $? ?b $?))
         (object (is-a builtin-function)
                 (name ?a)
                 (title eq)
                 (contents ?a0 ?a1))
         (object (is-a scalar-thing)
                 (name ?a0)
                 (value ?v))
         (object (is-a builtin-function)
                 (name ?b)
                 (title eq)
                 (contents ?a2 ?a3))
         (object (is-a scalar-thing)
                 (name ?a2)
                 (value ?v))
         =>
         (printout t "VIOLATION: Found (or (eq "  ?v " " (send ?a1 representation) ")" crlf
                   "                      (eq " ?v " " (send ?a3 representation) "))" crlf crlf
                   "           Use (not (neq " ?v " " (send ?a1 representation) " " (send ?a3 representation) ")) instead!" crlf))

