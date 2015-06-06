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
(deffacts stages
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
(defmethod no-strings-in-list
  ((?list MULTIFIELD))
  (not (exists$ stringp
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
             (slot fully-loaded
                   (type SYMBOL)
                   (allowed-symbols FALSE 
                                    TRUE))
             (slot top
                   (type SYMBOL
                         INSTANCE-NAME)
                   (default ?NONE))
             (multislot elements))
(defclass thing
  (is-a USER)
  (slot parent 
        (type SYMBOL)
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
(defclass typed-scalar-thing
  (is-a scalar-thing)
  (slot type
        (type SYMBOL)
        (default ?NONE)))

(defclass lexeme
  (is-a scalar-thing)
  (slot value
        (type LEXEME)
        (source composite)
        (storage local)))
(defclass list
  (is-a thing)
  (multislot contents))
(defclass match
  (is-a thing)
  (slot binding
        (type LEXEME))
  (multislot contents))

(defclass defrule
  (is-a thing
        has-comment)
  (slot salience
        (type INTEGER)
        (range -10000
               10000)
        (default-dynamic 0))
  (slot auto-focus
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (multislot matches)
  (multislot body))

(defclass deffunction
  (is-a thing
        has-comment)
  (multislot arguments)
  (multislot body))

(defclass defgeneric
  (is-a thing
        has-comment))

(defclass defmethod
  (is-a thing
        has-comment)
  (slot index 
        (type INTEGER))
  (multislot args)
  (multislot body))

(defclass defglobal
  (is-a thing)
  (slot module 
        (type SYMBOL))
  (multislot assignments))
(defclass defglobal-assignment
  (is-a thing)
  (slot variable 
        (type STRING)
        (default ?NONE))
  (slot value
        (default ?NONE)))
(defclass deftemplate
  (is-a thing
        has-comment)
  (multislot slots))

(defclass message-handler-documentation
  (is-a thing)
  (slot handler-name
        (type SYMBOL)
        (default ?NONE))
  (slot handler-type
        (type SYMBOL)
        (allowed-symbols primary 
                         around 
                         before 
                         after)))
(defclass defclass
  (is-a thing
        has-comment)
  (multislot inherits-from)
  (slot role
        (type SYMBOL)
        (allowed-symbols concrete 
                         abstract))
  (slot pattern-match
        (type SYMBOL)
        (allowed-symbols reactive 
                         non-reactive))
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
         ?f <- (lexer (fully-loaded FALSE)
                      (elements)
                      (file ?path)
                      (router ?name)
                      (top ?top))
         =>
         ; read an entire line at a time instead so we can capture quoted parens ahead of time
         (bind ?next (next-token ?name))
         (if (neq ?next EOF) then
           (modify ?f (elements ?next))
           else
           (modify ?f (fully-loaded TRUE))))

(defrule new-top-level
         (declare (salience 2))
         (stage (current lex))
         ?f <- (lexer (fully-loaded FALSE)
                      (elements LPAREN ?)
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
         ?f <- (lexer (fully-loaded FALSE)
                      (elements LPAREN ?)
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
         ?f <- (lexer (fully-loaded FALSE)
                      (elements RPAREN ?)
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
         ?f <- (lexer (fully-loaded FALSE)
                      (elements ?type ?value)
                      (top ?top))
         ?f2 <- (object (is-a list)
                        (name ?top)
                        (contents $?contents))
         =>
         (bind ?name (instance-name (make-instance of typed-scalar-thing
                                                   (parent ?top)
                                                   (type ?type)
                                                   (value ?value))))
         (modify-instance ?f2 (contents $?contents ?name))
         (modify ?f (elements)))

(defrule warn:parse-special-element-outside-list
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (fully-loaded FALSE)
                      (elements ?type ?value)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (printout WERROR "WARNING: Found a special tag outside a list!" crlf)
         (make-instance of typed-scalar-thing
                        (parent ?top)
                        (type ?type)
                        (value ?value))
         (modify ?f (elements)))


(defrule parse-normal-element
         (declare (salience 1))
         (stage (current lex))
         ?f <- (lexer (fully-loaded FALSE)
                      (elements ?value)
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
         ?f <- (lexer (fully-loaded FALSE)
                      (elements ?value)
                      (router ?top)
                      (top ?top&:(symbolp ?top)))
         =>
         (format WERROR 
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
         ?f <- (lexer (fully-loaded FALSE)
                      (elements RPAREN ?)
                      (router ?top)
                      (top ?top)
                      (file ?file))
         =>
         (printout WERROR
                   "ERROR: " ?file crlf
                   tab "found a ) outside an actual list!" crlf)
         (halt))


(defrule finished-completely
         (stage (current lex))
         ?f <- (lexer (fully-loaded TRUE)
                      (elements)
                      (router ?name))
         =>
         (close ?name)
         (retract ?f))
;-----------------------------------------------------------------------------
(defrule translate-defrule:comment:no-decl
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 $?matches
                                 =>
                                 $?body))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule 
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment:no-decl
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))

(defrule translate-defrule:comment:decl:salience-only
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 ?decl
                                 $?matches
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare 
                                  ?salience-list))
         ?f3 <- (object (is-a list)
                        (name ?salience-list)
                        (contents salience 
                                  ?salience))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defrule 
                        (salience ?salience)
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment:decl:salience-only
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 ?decl
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare 
                                  ?salience-list))
         ?f3 <- (object (is-a list)
                        (name ?salience-list)
                        (contents salience 
                                  ?salience))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defrule
                        (salience ?salience)
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))

(defrule translate-defrule:comment:decl:auto-focus-only
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 ?decl
                                 $?matches
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare 
                                  ?auto-focus-list))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus-list)
                        (contents auto-focus 
                                  ?auto-focus))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defrule 
                        (auto-focus ?auto-focus)
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment:decl:auto-focus-only
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 ?decl
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare
                                  ?auto-focus-list))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus-list)
                        (contents auto-focus 
                                  ?auto-focus))
         =>
         (unmake-instance ?f ?f2 ?f3)
         (make-instance ?name of defrule
                        (auto-focus ?auto-focus)
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))

(defrule translate-defrule:comment:decl:both-salience-first
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 ?decl
                                 $?matches
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare 
                                  ?salience-list
                                  ?auto-focus-list))
         ?f3 <- (object (is-a list)
                        (name ?salience-list)
                        (contents salience
                                  ?salience))
         ?f4 <- (object (is-a list)
                        (name ?auto-focus-list)
                        (contents auto-focus 
                                  ?auto-focus))
         =>
         (unmake-instance ?f ?f2 ?f3 ?f4)
         (make-instance ?name of defrule 
                        (salience ?salience)
                        (auto-focus ?auto-focus)
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:comment:decl:both-salience-second
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 ?decl
                                 $?matches
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare 
                                  ?auto-focus-list
                                  ?salience-list))
         ?f3 <- (object (is-a list)
                        (name ?salience-list)
                        (contents salience
                                  ?salience))
         ?f4 <- (object (is-a list)
                        (name ?auto-focus-list)
                        (contents auto-focus 
                                  ?auto-focus))
         =>
         (unmake-instance ?f ?f2 ?f3 ?f4)
         (make-instance ?name of defrule 
                        (salience ?salience)
                        (auto-focus ?auto-focus)
                        (comment ?comment)
                        (parent ?parent)
                        (body ?body)
                        (matches ?matches)))

(defrule translate-defrule:no-comment:decl:both-salience-first
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 ?decl
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare
                                  ?salience-list
                                  ?auto-focus-list))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus-list)
                        (contents auto-focus 
                                  ?auto-focus))
         ?f4 <- (object (is-a list)
                        (name ?salience-list)
                        (contents salience 
                                  ?salience))
         =>
         (unmake-instance ?f ?f2 ?f3 ?f4)
         (make-instance ?name of defrule
                        (salience ?salience)
                        (auto-focus ?auto-focus)
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))
(defrule translate-defrule:no-comment:decl:both-salience-second
         (declare (salience 1))
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defrule
                                 ?name
                                 ?decl
                                 $?matches&:(no-strings-in-list ?matches)
                                 =>
                                 $?body))
         ?f2 <- (object (is-a list)
                        (name ?decl)
                        (contents declare
                                  ?auto-focus-list
                                  ?salience-list))
         ?f3 <- (object (is-a list)
                        (name ?auto-focus-list)
                        (contents auto-focus 
                                  ?auto-focus))
         ?f4 <- (object (is-a list)
                        (name ?salience-list)
                        (contents salience 
                                  ?salience))
         =>
         (unmake-instance ?f ?f2 ?f3 ?f4)
         (make-instance ?name of defrule
                        (salience ?salience)
                        (auto-focus ?auto-focus)
                        (parent ?parent)
                        (matches ?matches)
                        (body ?body)))

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
         (modify-instance ?f 
                          (matches $?before 
                                   (instance-name (make-instance of match
                                                                 (parent ?parent)
                                                                 (contents ?contents))) 
                                   $?after)))
;TODO: handle multiline strings
(defrule translate-match:binding
         "Before we construct defrule's we have to capture bound matches to prevent a matching ambiguity in a defrule between a comment and a bound match (both of them will show up as strings)"
         (declare (salience ?*priority:two*))
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
         (bind ?z (instance-name (make-instance ?list of match
                                                (parent ?parent)
                                                (binding ?var)
                                                (contents ?contents))))
         (modify-instance ?f
                          (contents $?before
                                    ?list
                                    $?after)))


(defrule translate-deffunction:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (name ?parent)
                       (contents deffunction 
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 ?args
                                 $?body))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f)
         (make-instance ?name of deffunction
                        (parent ?parent)
                        (comment ?comment)
                        (arguments ?a)
                        (body ?body)))
(defrule translate-deffunction:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents deffunction 
                                 ?name 
                                 ?args
                                 $?body))
         ?j <- (object (is-a list)
                       (name ?args)
                       (contents $?a))
         =>
         (unmake-instance ?j ?f)
         (make-instance ?name of deffunction
                        (parent ?parent)
                        (arguments ?a)
                        (body ?body)))

(defrule translate-defgeneric:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defgeneric
                                 ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defgeneric 
                        (parent ?parent)))
(defrule translate-defgeneric:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defgeneric
                                 ?name
                                 ?comment))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defgeneric 
                        (comment ?comment)
                        (parent ?parent)))
(defrule translate-defclass:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defclass 
                                 ?name
                                 ?comment&:(stringp ?comment)
                                 ?is-a
                                 $?rest))
         ?f2 <- (object (is-a list)
                        (name ?is-a)
                        (contents is-a $?ia))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defclass
                        (parent ?parent)
                        (comment ?comment)
                        (inherits-from ?ia)
                        (contents ?rest)))


(defrule translate-defclass:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defclass 
                                 ?name
                                 ?is-a
                                 $?rest))
         ?f2 <- (object (is-a list)
                        (name ?is-a)
                        (contents is-a $?ia))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defclass
                        (parent ?parent)
                        (inherits-from ?ia)
                        (contents ?rest)))



(defrule translate-defclass:populate-role
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents ?first
                                 $?rest))
         ?q <- (object (is-a list)
                       (name ?first)
                       (contents role ?role))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (role ?role)
                          (contents ?rest)))
(defrule translate-defclass:populate-pattern-match
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents ?first 
                                 $?rest))
         ?q <- (object (is-a list)
                       (name ?first)
                       (contents pattern-match ?pattern-match))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (pattern-match ?pattern-match)
                          (contents ?rest)))

(defrule translate-defclass:convert-message-handler-documentation:no-type
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before ?curr $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents message-handler ?name))
         =>
         (unmake-instance ?q)
         (modify-instance ?f (contents ?before (make-instance of message-handler-documentation
                                                              (parent ?parent)
                                                              (handler-name ?name)) 
                                       ?after)))
(defrule translate-defclass:convert-message-handler-documentation:type
         (stage (current parse))
         ?f <- (object (is-a defclass)
                       (contents $?before ?curr $?after)
                       (name ?parent))
         ?q <- (object (is-a list)
                       (name ?curr)
                       (contents message-handler ?name ?type))
         =>
         (unmake-instance ?q)
         (modify-instance ?f 
                          (contents ?before 
                                    (make-instance of message-handler-documentation
                                                   (parent ?parent)
                                                   (handler-name ?name)
                                                   (handler-type ?type))

                                    ?after)))


(defrule translate-defglobal:module
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defglobal 
                                 ?module&:(symbolp ?module)
                                 $?rest)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance of defglobal
                        (parent ?parent)
                        (module ?module)
                        (assignments ?rest)))

(defrule translate-defglobal:no-module
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents defglobal 
                                 $?rest)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance of defglobal
                        (parent ?parent)
                        (assignments ?rest)))

(defrule build-defglobal-assignment
         (stage (current parse))
         ?f <- (object (is-a defglobal)
                       (assignments $?before 
                                    ?var =(string-to-field "=") ?value 
                                    $?rest)
                       (name ?parent))
         =>
         (modify-instance ?f 
                          (assignments ?before
                                       (make-instance of defglobal-assignment
                                                      (parent ?parent)
                                                      (variable ?var)
                                                      (value ?value))
                                       ?rest)))

(defclass defmodule
  (is-a thing
        has-comment)
  (multislot specifications))
(defrule build-defmodule:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defmodules
                                 ?name
                                 ?comment&:(stringp ?comment)
                                 $?specs))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defmodule
                        (parent  ?parent)
                        (comment ?comment)
                        (specifications ?specs)))



(defclass port-specification
  (is-a thing)
  (role abstract)
  (pattern-match non-reactive)
  (slot construct
        (type SYMBOL)
        (allowed-symbols undefined
                         nil
                         deftemplate
                         defclass
                         defglobal
                         deffunction
                         defgeneric))
  (multislot qualifiers))

(defclass export-specification
  (is-a port-specification)
  (role concrete)
  (pattern-match reactive))

(defrule build-export-specification
         (stage (current parse))
         ?f <- (object (is-a defmodule)
                       (name ?parent)
                       (specifications $?a 
                                       ?b 
                                       $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents export 
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?a
                          (specifications $?a
                                          (instance-name (make-instance of export-specification
                                                                        (parent ?parent)
                                                                        (items $?rest)))
                                          $?b))) 
(defrule expand-export-specification:all-or-none
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers "?ALL"|"?NONE"))
         =>
         (modify-instance ?f (construct nil)))
(defrule expand-export-specification:specific-construct-all-or-none
         (stage (current parse))
         ?f  <- (object (is-a export-specification)
                        (construct undefined)
                        (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                    ?qualifier&"?ALL"|"?NONE"))
         =>
         (modify-instance ?f 
                          (construct ?construct)
                          (qualifiers ?qualifier)))

(defrule expand-export-specification:specific-construct-and-qualifiers
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                   $?qualifiers&:(and (> (length$ ?qualifiers) 0)
                                                      (no-strings-in-list $?qualifiers))))
         =>
         (modify-instance ?f
                          (construct ?construct)
                          (qualifiers ?qualifiers)))

(defclass import-specification
  (is-a port-specification)
  (role concrete)
  (pattern-match reactive)
  (slot module-name
        (type SYMBOL)
        (visibility public)
        (default ?NONE)))

(defrule build-import-specification
         (stage (current parse))
         ?f <- (object (is-a defmodule)
                       (name ?parent)
                       (specifications $?a 
                                       ?b 
                                       $?c))
         ?f2 <- (object (is-a list)
                        (name ?b)
                        (contents import 
                                  $?rest))
         =>
         (unmake-instance ?f2)
         (modify-instance ?a
                          (specifications $?a
                                          (instance-name (make-instance of import-specification
                                                                        (parent ?parent)
                                                                        (qualifiers $?rest)))
                                          $?b))) 

(defrule expand-import-specification:all-or-none
         (stage (current parse))
         ?f <- (object (is-a import-specification)
                       (construct undefined)
                       (qualifiers ?module-name&:(symbolp ?module-name)
                                   ?qualifier&"?ALL"|"?NONE"))
         =>
         (modify-instance ?f 
                          (module-name ?module-name)
                          (construct nil)
                          (qualifiers ?qualifier)))
(defrule expand-import-specification:specific-construct-all-or-none
         (stage (current parse))
         ?f  <- (object (is-a import-specification)
                        (construct undefined)
                        (qualifiers ?module-name&:(symbolp ?module-name)
                                    ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                    ?qualifier&"?ALL"|"?NONE"))
         =>
         (modify-instance ?f 
                          (module-name ?module-name)
                          (construct ?construct)
                          (qualifiers ?qualifier)))

(defrule expand-import-specification:specific-construct-and-qualifiers
         (stage (current parse))
         ?f <- (object (is-a import-specification)
                       (construct undefined)
                       (qualifiers ?module-name&:(symbolp ?module-name)
                                   ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                   $?qualifiers&:(and (> (length$ ?qualifiers) 0)
                                                      (no-strings-in-list $?qualifiers))))
         =>
         (modify-instance ?f
                          (module-name ?module-name)
                          (construct ?construct)
                          (qualifiers ?qualifiers)))
(defclass default-facet
  (is-a thing)
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
  (is-a thing)
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

(defrule translate-defclass:convert-slot
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

(defrule translate-defclass:convert-multislot
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
                       (facets $?a
                               ?curr
                               $?b)
                       (name ?parent))
         ?f2 <- (object (is-a list)
                        (name ?curr)
                        (contents default
                                  ?c&"?NONE"|"?DERIVE"))
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
         ?f <- (object (is-a defclass-single-slot|deftemplate-single-slot|defclass-multislot|deftemplate-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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
         ?f <- (object (is-a defclass-single-slot|defclass-multislot)
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

(defrule translate-deftemplate:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deftemplate
                                 ?name 
                                 ?comment&:(stringp ?comment)
                                 $?slots)
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deftemplate
                        (comment ?comment)
                        (parent ?parent)
                        (slots $?slots)))

(defrule translate-deftemplate:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deftemplate
                                 ?name 
                                 $?slots&:(no-strings-in-list ?slots))
                       (parent ?parent))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deftemplate
                        (parent ?parent)
                        (slots $?slots)))

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
