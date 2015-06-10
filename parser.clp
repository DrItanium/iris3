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
(deffunction is-string-class
             (?value)
             (and (instance-namep ?value)
                  (eq (class ?value) string)))
(defmethod no-strings-in-list
  ((?list MULTIFIELD))
  (not (exists$ is-string-class
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
(defclass thing
  (is-a USER)
  (slot parent 
        (type SYMBOL
              INSTANCE-NAME)
        (default ?NONE)))
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

(defclass match
  (is-a thing)
  (slot binding
        (type LEXEME
              INSTANCE-NAME))
  (multislot contents))

(defclass defrule
  (is-a thing
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

(defclass deffunction
  (is-a thing
        has-comment)
  (slot function-name
        (type SYMBOL)
        (default ?NONE))
  (multislot arguments)
  (multislot body))


(defclass defmethod
  (is-a thing
        has-comment)
  (slot index 
        (type INTEGER))
  (multislot args)
  (multislot body))



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
(defclass defrule-declaration
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
                        (rule-name ?rule-name)
                        (comment ?cvalue)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment:no-decl
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule
                                 ?rule-name
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body)
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
                        (rule-name ?rule-name)
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))

(defrule translate-defrule:comment:decl
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule 
                                 ?rule-name 
                                 ?comment
                                 ?decl
                                 $?matches
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
                        (rule-name ?rule-name)
                        (auto-focus ?auto-focus)
                        (salience ?salience)
                        (comment ?cvalue)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment:decl
         (declare (salience 2))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defrule
                                 ?rule-name
                                 ?decl
                                 $?matches&:(no-strings-in-list ?matches)
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
          (rule-name ?rule-name)
          (auto-focus ?auto-focus)
                        (salience ?salience)
                        (parent ?parent)
                        (matches ?matches)
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

(defrule translate-match:no-binding
         (stage (current parse))
         ?f <- (object (is-a defrule)
                       (name ?parent)
                       (matches $?before ?list $?after))
         ?q <- (object (is-a list)
                       (name ?list)
                       (contents $?contents))
         =>
         (unmake-instance ?q)
         (make-instance ?list of match
                        (parent ?parent)
                        (contents ?contents)))

;TODO: handle multiline strings
(defrule translate-match:binding
         "Before we construct defrule's we have to capture bound matches to prevent a matching ambiguity in a defrule between a comment and a bound match (both of them will show up as strings)"
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
         (make-instance ?list of match
                        (parent ?parent)
                        (binding ?var)
                        (contents ?contents))
         (modify-instance ?f
                          (contents $?before
                                    ?list
                                    $?after)))



(defrule translate-deffunction:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deffunction 
                                 ?func-name 
                                 ?comment
                                 ?args
                                 $?body)
                       (parent ?parent)
                       (name ?name))

         ?k <- (object (is-a string)
                       (name ?comment)
                       (value ?cvalue))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f ?k)
         (make-instance ?name of deffunction
                        (function-name ?func-name)
                        (parent ?parent)
                        (comment ?cvalue)
                        (arguments ?a)
                        (body ?body)))
(defrule translate-deffunction:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deffunction 
                                 ?func-name 
                                 ?args
                                 $?body)
                       (parent ?parent)
                       (name ?name))

         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f)
         (make-instance ?name of deffunction
                        (function-name ?func-name)
                        (parent ?parent)
                        (arguments ?a)
                        (body ?body)))


