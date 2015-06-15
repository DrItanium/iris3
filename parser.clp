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
(defglobal MAIN 
           ?*priority:first* = 10000
           ?*priority:three* = 3
           ?*priority:two* = 2
           ?*priority:one* = 1
           ?*priority:last* = -9999
           ?*priority:dead-last* = -10000)
(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(defrule next-stage
         (declare (salience ?*priority:last*))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f (current ?next)
                 (rest ?rest)))
(defrule done-with-stages
         (declare (salience ?*priority:dead-last*))
         ?f <- (stage (rest))
         =>
         (retract ?f))
(deffacts init-stages
          (stage (current load)
                 (rest lex
                       parse)))
(defgeneric apply$
            "Apply a function to each member of a list and return the result")
(defgeneric filter$
            "Fliter elements out of a list and return a new list with the results")
(defgeneric hex-to-number
            "Converts a hexidecimal number to its decimal representation")
(defgeneric hexchar-to-number
            "Converts a hexidecimal character to its decimal representation")
(defgeneric exists$
            "Checks to see if a given predicate function returns true for at least one element in a list")
(defmethod apply$
  ((?fn SYMBOL)
   (?list MULTIFIELD))
  (bind ?output 
        (create$))
  (progn$ (?a ?list)
          (bind ?output 
                (create$ ?output 
                         (funcall ?fn 
                                  ?a))))
  (return ?output))

(defmethod apply$
  ((?fn SYMBOL)
   $?list)
  (apply$ ?fn
          ?list))

(defmethod filter$
  ((?fn SYMBOL)
   (?list MULTIFIELD))
  (bind ?output (create$))
  (progn$ (?a ?list)
          (if (funcall ?fn ?a) then
            (bind ?output (create$ ?output
                                   ?a))))
  (return ?output))

(defmethod filter$
  ((?fn SYMBOL)
   $?list)
  (filter$ ?fn 
           ?list))
(defmethod hexchar-to-number
  ((?char STRING))
  (bind ?field (string-to-field (lowcase ?char)))
  (return 
    (if (numberp ?field) then
      ?field
      else
      (if (symbolp ?field) then
        (switch ?field
                (case a then 10)
                (case b then 11)
                (case c then 12)
                (case d then 13)
                (case e then 14)
                (case f then 15)
                (default FALSE))
        else
        FALSE))))
(defmethod hex-to-number
  ((?a SYMBOL))
  (bind ?result 0)
  ; strip off the first two characters
  (bind ?strip (sub-string 3 
                           (length$ ?a)
                           ?a))
  (bind ?len (length$ ?strip))
  ; Now go through and build up the number by extracting the current char
  ; converting it and then shifting left by the position of the digit in the
  ; original "number"
  (loop-for-count (?ind 1 ?len) do
                  (bind ?result (+ ?result 
                                   (left-shift (hexchar-to-number (sub-string ?ind ?ind ?strip))
                                               (* (- ?len ?ind) 4)))))
  (return ?result))

(defmethod exists$
  ((?fn SYMBOL)
   (?list MULTIFIELD))
  (progn$ (?e ?list)
          (if (funcall ?fn ?e) then
            (return TRUE)))
  (return FALSE))
(defmethod exists$
  ((?fn SYMBOL)
   $?list)
  (exists$ ?fn
           ?list))
(defgeneric no-strings-in-list
            "checks to see if the given list is free of strings")
(defgeneric no-primitive-strings-in-list
            "checks to see if the given list is free of primitive strings")
(deffunction string-classp
             (?value)
             (and (instance-namep ?value)
                  (eq (class ?value) string)))
(defmethod no-primitive-strings-in-list
  ((?list MULTIFIELD))
  (not (exists$ stringp
                ?list)))
(defmethod no-primitive-strings-in-list
  ($?list)
  (no-primitive-strings-in-list ?list))
(defmethod no-strings-in-list
  ((?list MULTIFIELD))
  (not (exists$ string-classp
                ?list)))
(defmethod no-strings-in-list
  ($?list)
  (no-strings-in-list ?list))

(deftemplate lexer
             (slot file
                   (type LEXEME)
                   (default ?NONE))
             (slot router
                   (type SYMBOL)
                   (default ?NONE))
             (slot top
                   (type SYMBOL
                         INSTANCE-NAME)
                   (default ?NONE))
             (multislot elements))
(defgeneric construct-instance
            "constructs an instance of soemthing given a series of variables. Returns the instance name of the thing")
(defmessage-handler SYMBOL part-of-a primary
                    "returns FALSE unless the target type is SYMBOL"
                    (?type)
                    (not (neq (upcase ?type)
                              LEXEME
                              SYMBOL)))
(defclass thing
  (is-a USER)
  (slot parent 
        (type SYMBOL
              INSTANCE-NAME)
        (default ?NONE))
  (message-handler part-of-a primary))
(defmessage-handler thing part-of-a
                    "Checks to see if the current object or one of its parents are of a given type"
                    (?type)
                    (if (eq (class ?self)
                            ?type) then
                      TRUE
                      else
                      (send ?self:parent 
                            part-of-a ?type)))

(defclass has-comment
  (is-a USER)
  (slot comment
        (type STRING)))
(defclass composite-thing
  "A thing that is made up of other things. This is separate from a list!"
  (is-a thing)
  (multislot contents
             (visibility public)
             (default ?NONE)))
(defclass scalar-thing
  (is-a thing)
  (slot value
        (visibility public)
        (default ?NONE)))
(defmethod construct-instance
  ((?class SYMBOL)
   (?parent SYMBOL
            INSTANCE-NAME)
   ?value)
  (instance-name (make-instance of ?class 
                                (parent ?parent)
                                (value ?value))))
(defclass typed-scalar-thing
  (is-a scalar-thing)
  (slot type
        (type SYMBOL)
        (default ?NONE)))

(defclass string
  "Strings need to be wrapped in their own nodes"
  (is-a scalar-thing)
  (slot value
        (type STRING)
        (source composite)
        (storage local)))
(defclass global-variable (is-a scalar-thing))
(defclass multifield-global-variable (is-a scalar-thing))
(defclass multifield-variable (is-a scalar-thing))
(defclass singlefield-variable (is-a scalar-thing))
(defclass not-constraint (is-a scalar-thing))
(defclass and-constraint (is-a scalar-thing))
(defclass or-constraint (is-a scalar-thing))
(defclass multifield-wildcard (is-a scalar-thing))
(defclass singlefield-wildcard (is-a scalar-thing))

(defmethod construct-instance
  ((?class SYMBOL (eq ?class OR_CONSTRAINT))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance or-constraint ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class AND_CONSTRAINT))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance and-constraint ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class NOT_CONSTRAINT))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance not-constraint ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class MF_WILDCARD))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance multifield-wildcard ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class SF_WILDCARD))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance singlefield-wildcard ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class MF_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance multifield-variable ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class SF_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance singlefield-variable ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class MF_GBL_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance multifield-global-variable ?parent ?value))
(defmethod construct-instance
  ((?class SYMBOL (eq ?class GBL_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance global-variable ?parent ?value))


(defclass list
  (is-a thing)
  (multislot contents))

(defrule open-file
         (stage (current load))
         ?f <- (open ?path)
         =>
         (retract ?f)
         (bind ?name (gensym*))
         (if (open ?path 
                   ?name 
                   "r") then
           (assert (lexer (file ?path)
                          (router ?name)
                          (elements (next-token ?name))
                          (top ?name)))
           else
           (printout werror 
                     "couldn't open " ?path crlf)))
(defrule read-element
         (stage (current lex))
         ?f <- (lexer (elements)
                      (file ?path)
                      (router ?name)
                      (top ?top))
         =>
         ; read an entire line at a time instead so we can capture quoted parens ahead of time
         (modify ?f (elements (next-token ?name))))
(defrule new-top-level
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements LPAREN ?)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (bind ?name (instance-name (make-instance of list
                                                   (parent ?top))))
         (modify ?f (elements)
                 (top ?name)))
(defrule new-list
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements LPAREN ?)
                      (top ?top&:(instance-namep ?top)))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (contents $?contents))
         =>
         (bind ?name (instance-name (make-instance of list
                                                   (parent ?top))))
         (modify-instance ?f2 (contents ?contents ?name))
         (modify ?f (elements)
                 (top ?name)))
(defrule end-list
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements RPAREN ?)
                      (top ?top&:(instance-namep ?top)))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (parent ?parent))
         =>
         (modify ?f (elements)
                 (top ?parent)))

(defrule parse-special-element
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (elements ?type ?value)
                      (top ?top))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (contents $?contents))
         =>
         (modify-instance ?f2 (contents $?contents
                                        (construct-instance ?type
                                                            ?top
                                                            ?value)))
         (modify ?f (elements)))

(defrule warn:parse-special-element-outside-list
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (elements ?type ?value)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (printout werror "WARNING: Found a special tag outside a list!" crlf)
         (construct-instance ?type 
                             ?top 
                             ?value)
         (modify ?f (elements)))

(defrule parse-string
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements ?value&:(stringp ?value))
                      (top ?top))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (contents $?contents))
         =>
         (modify ?f (elements))
         (modify-instance ?f2 (contents $?contents 
                                        (construct-instance string
                                                            ?top
                                                            ?value))))
(defrule parse-string-outside-list
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements ?value&:(stringp ?value))
                      (top ?top&:(symbolp ?top)))
         =>
         (printout werror "WARNING: Found a string outside a list!" crlf)
         (modify ?f (elements))
         (make-instance of string
                        (parent ?top)
                        (value ?value)))

(defrule parse-normal-element
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (elements ?value)
                      (top ?top))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (contents $?contents))
         =>
         (modify ?f (elements))
         (modify-instance ?f2 (contents ?contents 
                                        ?value)))

(defrule warn:parse-normal-element-outside-list
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (elements ?value)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (format werror 
                 "WARNING: Found a %s (%s) outside a list!%n" 
                 (class ?value)
                 ?value)
         (modify ?f (elements))
         (make-instance of typed-scalar-thing
                        (parent ?top)
                        (type (class ?value))
                        (value ?value)))


(defrule error:end-list-without-beginning
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements RPAREN ?)
                      (router ?top)
                      (top ?top)
                      (file ?file))
         =>
         (printout werror
                   "ERROR: " ?file crlf
                   tab "found a ) outside an actual list!" crlf)
         (halt))

(defrule finished-completely
         (declare (salience 3))
         (stage (current lex))
         ?f <- (lexer (elements STOP ?)
                      (router ?name))
         =>
         (close ?name)
         (retract ?f))
;-----------------------------------------------------------------------------
