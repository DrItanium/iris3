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
(defmessage-handler OBJECT get-parent-chain primary
                    "Returns the an empty multifield as this object has no parents!"
                    ()
                    (create$))
(defclass node
  (is-a USER)
  (slot parent 
        (type SYMBOL
              INSTANCE-NAME)
        (default ?NONE))
  (message-handler part-of-a primary)
  (message-handler get-parent-chain primary))
(defmessage-handler node part-of-a
                    "Checks to see if the current object or one of its parents are of a given type"
                    (?type)
                    (if (eq (class ?self)
                            ?type) then
                      TRUE
                      else
                      (send ?self:parent 
                            part-of-a ?type)))
(defmessage-handler node get-parent-chain primary
                    "Construct a chain of all the parents of this node"
                    ()
                    (create$ ?self:parent (send ?self:parent get-parent-chain)))

(defclass has-comment
  (is-a USER)
  (slot comment
        (visibility public)
        (type STRING)))
(defclass has-local-binds
  (is-a USER)
  (multislot local-binds
             (visibility public)))

(defclass has-arguments
  (is-a USER)
  (multislot arguments
             (visibility public)))

(defclass has-body
  (is-a USER)
  (multislot body
             (visibility public)))

(defclass exec-fragment 
  "An amalgamation of several different types"
  (is-a node
        has-body
        has-local-binds
        has-arguments))


(defclass composite-node
  "A node that is made up of other nodes. This is separate from a list!"
  (is-a node)
  (multislot contents
             (visibility public)
             (default ?NONE)))
(defclass scalar-node
  (is-a node)
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
(defclass typed-scalar-node
  (is-a scalar-node)
  (slot type
        (type SYMBOL)
        (default ?NONE)))

(defclass string
  "Strings need to be wrapped in their own nodes"
  (is-a scalar-node)
  (slot value
        (type STRING)
        (source composite)
        (storage local)))
(defclass variable (is-a scalar-node))
(defclass global-variable (is-a variable))
(defclass singlefield-global-variable (is-a global-variable))
(defclass multifield-global-variable (is-a global-variable))

(defclass local-variable (is-a variable))
(defclass multifield-variable (is-a local-variable))
(defclass singlefield-variable (is-a local-variable))

(defclass constraint (is-a scalar-node))
(defclass not-constraint (is-a constraint))
(defclass and-constraint (is-a constraint))
(defclass or-constraint (is-a constraint))

(defclass wildcard (is-a scalar-node))
(defclass multifield-wildcard (is-a wildcard))
(defclass singlefield-wildcard (is-a wildcard))



(defmethod construct-instance
  "convert a symbol of type OR_CONSTRAINT to class of type or-constraint"
  ((?class SYMBOL 
           (eq ?current-argument
               OR_CONSTRAINT))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance or-constraint
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type AND_CONSTRAINT to class of type and-constraint"
  ((?class SYMBOL 
           (eq ?current-argument
               AND_CONSTRAINT))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance and-constraint
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type NOT_CONSTRAINT to class of type not-constraint"
  ((?class SYMBOL 
           (eq ?current-argument
               NOT_CONSTRAINT))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance not-constraint
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type MF_WILDCARD to class of type multifield-wildcard"
  ((?class SYMBOL 
           (eq ?current-argument
               MF_WILDCARD))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance multifield-wildcard
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type SF_WILDCARD to class of type singlefield-wildcard"
  ((?class SYMBOL 
           (eq ?current-argument
               SF_WILDCARD))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance singlefield-wildcard
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type MF_VARIABLE to class of type multifield-variable"
  ((?class SYMBOL 
           (eq ?current-argument
               MF_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance multifield-variable
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type SF_VARIABLE to class of type singlefield-variable"
  ((?class SYMBOL 
           (eq ?current-argument
               SF_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance singlefield-variable
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type MF_GBL_VARIABLE to class of type multifield-global-variable"
  ((?class SYMBOL 
           (eq ?current-argument
               MF_GBL_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance multifield-global-variable
                      ?parent 
                      ?value))

(defmethod construct-instance
  "convert a symbol of type GBL_VARIABLE to class of type singlefield-global-variable"
  ((?class SYMBOL 
           (eq ?current-argument
               GBL_VARIABLE))
   (?parent SYMBOL
            INSTANCE-NAME)
   (?value LEXEME))
  (construct-instance singlefield-global-variable
                      ?parent 
                      ?value))


(defclass list
  (is-a node)
  (multislot contents
             (visibility public)))

(defclass reference
  "An indirect reference to something else, useful for deffunctions and arguments"
  (is-a scalar-node)
  (slot expand
        (type SYMBOL)
        (allowed-symbols FALSE TRUE)))
;------------------------------------------------------------------------------
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
         (modify ?f 
                 (elements (next-token ?name))))

(defrule new-top-level
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements LPAREN ?)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (modify ?f
                 (elements)
                 (top (make-instance of list 
                                     (parent ?top)))))
(defrule new-list
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements LPAREN ?)
                      (top ?top))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (modify-instance ?top 
                          (contents ?contents 
                                    (bind ?name 
                                          (make-instance of list
                                                         (parent ?top)))))
         (modify ?f 
                 (elements)
                 (top ?name)))

(defrule end-list
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements RPAREN ?)
                      (top ?top))
         (object (is-a list)
                 (name ?top)
                 (parent ?parent))
         =>
         (modify ?f 
                 (elements)
                 (top ?parent)))

(defrule parse-special-element
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (elements ?type 
                                ?value)
                      (top ?top))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (contents $?contents))
         =>
         (modify-instance ?f2 
                          (contents $?contents
                                    (construct-instance ?type
                                                        ?top
                                                        ?value)))
         (modify ?f 
                 (elements)))

(defrule warn:parse-special-element-outside-list
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (elements ?type ?value)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (printout werror 
                   "WARNING: Found a special tag outside a list!" crlf)
         (construct-instance ?type 
                             ?top 
                             ?value)
         (modify ?f 
                 (elements)))

(defrule parse-string
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements ?value&:(stringp ?value))
                      (top ?top))
         (object (is-a list)
                 (name ?top)
                 (contents $?contents))
         =>
         (modify ?f 
                 (elements))
         (modify-instance ?top
                          (contents $?contents 
                                    (construct-instance string
                                                        ?top
                                                        ?value))))
(defrule parse-string-outside-list
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (elements ?value&:(stringp ?value))
                      (top ?top&:(symbolp ?top)))
         =>
         (printout werror 
                   "WARNING: Found a string outside a list!" crlf)
         (modify ?f 
                 (elements))
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
         (modify ?f 
                 (elements))
         (modify-instance ?f2 
                          (contents ?contents 
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
         (modify ?f 
                 (elements))
         (make-instance of typed-scalar-node
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
