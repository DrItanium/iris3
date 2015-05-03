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
; lexing nodes
(deffunction starts-with
             (?tgt ?str)
             (eq (str-index ?tgt 
                            ?str) 
                 1))
(deftemplate lex-element
             (slot parent
                   (type SYMBOL)
                   (default ?NONE))
             (slot value
                   (default ?NONE))
             (slot index
                   (type INTEGER)
                   (range 0 ?VARIABLE)
                   (default ?NONE)))
(deffunction new-lex-element
             (?value ?index ?parent)
             (assert (lex-element (value ?value)
                                  (index ?index)
                                  (parent ?parent))))
(defrule open-file
         ?f <- (open ?file)
         =>
         (retract ?f)
         (bind ?name (gensym*))
         (if (open ?file 
                   ?name 
                   "r") then
           (assert (read elements from ?name)
                   (translation ?name to ?file))
           else
           (printout werror "Couldn't open " ?file crlf)))
(defrule read-elements
         ?f <- (read elements from ?rtr)
         =>
         (retract ?f)
         (bind ?element (read ?rtr))
         (bind ?index 0)
         (while (neq ?element EOF) do
                (new-lex-element ?element
                                 ?index
                                 ?rtr)
                (bind ?element (read ?rtr))
                (bind ?index (+ ?index 1)))
         (close ?rtr)
         (assert (file ?rtr has ?index elements)))
(defclass iris-node
  (is-a USER)
  (slot parent
        (type SYMBOL)
        (default ?NONE))
  (slot type
        (visibility public)
        (type SYMBOL)
        (default ?NONE))
  (slot index
        (visibility public)
        (range 0 ?VARIABLE)
        (type INTEGER))
  (slot value
        (visibility public)
        (default ?NONE)))

(defclass iris-symbol
  (is-a iris-node)
  (slot type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default symbol))
  (slot value
        (source composite)
        (type SYMBOL)))

(defrule lex-element->symbol
         ?f <- (lex-element (value ?value&:(symbolp ?value))
                            (index ?index)
                            (parent ?parent))

         =>
         (retract ?f)
         (make-instance of iris-symbol
                        (value ?value)
                        (index ?index)
                        (parent ?parent)))

(defclass iris-string
  (is-a iris-node)
  (slot type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default string))
  (slot value
        (source composite)
        (type STRING)))

(defrule lex-element->string
         ?f <- (lex-element (value ?value&:(stringp ?value))
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-string
                        (value ?value)
                        (index ?index)
                        (parent ?parent)))

(defclass iris-left-paren
  (is-a iris-node)
  (slot type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default left-paren))
  (slot value
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default "(")))

(defrule lex-element->left-paren
         (declare (salience 1))
         ?f <- (lex-element (value "(")
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-left-paren
                        (index ?index)
                        (parent ?parent)))

(defclass iris-right-paren
  (is-a iris-node)
  (slot type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default right-paren))
  (slot value
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default ")")))

(defrule lex-element->right-paren
         (declare (salience 1))
         ?f <- (lex-element (value ")")
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-right-paren
                        (index ?index)
                        (parent ?parent)))

(defclass iris-number
  (is-a iris-node)
  (slot type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default number))
  (slot subtype
        (type SYMBOL)
        (allowed-symbols binary hex decimal)
        (default ?NONE))
  (slot value
        (source composite)
        (type INTEGER SYMBOL)))

(defrule lex-element->pure-number
         (declare (salience 5))
         ?f <- (lex-element (value ?value&:(integerp ?value))
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-number 
                        (subtype decimal)
                        (value ?value)
                        (index ?index)
                        (parent ?parent)))

(defrule lex-element->float-number:error
         (declare (salience 5))
         ?f <- (lex-element (value ?value&:(floatp ?value))
                            (index ?index)
                            (parent ?parent))
         (translation ?parent to ?path
                      =>
                      (printout werror "ERROR: floating point numbers not supported!" crlf
                                (format nil 
                                        "%s:%d: %f%n" 
                                        ?path 
                                        ?index 
                                        ?value) crlf)
                      (exit))

         (defrule lex-element->hex-number
                  (declare (salience 5))
                  ?f <- (lex-element (value ?value&:(and (symbolp ?value)
                                                         (starts-with 0x 
                                                                      ?value)))
                                     (index ?index)
                                     (parent ?parent))
                  =>
                  (retract ?f)
                  (make-instance of iris-number
                                 (subtype hex)
                                 (value ?value)
                                 (index ?index)
                                 (parent ?parent)))

         (defrule lex-element->binary-number
                  (declare (salience 5))
                  ?f <- (lex-element (value ?value&:(and (symbolp ?value)
                                                         (starts-with 0b 
                                                                      ?value)))
                                     (index ?index)
                                     (parent ?parent))
                  =>
                  (retract ?f)
                  (make-instance of iris-number
                                 (subtype binary)
                                 (value ?value)
                                 (index ?index)
                                 (parent ?parent)))

         (defclass iris-variable
           (is-a iris-node)
           (slot value
                 (source composite)
                 (type LEXEME)))

         (defclass iris-single-variable
           (is-a iris-variable)
           (slot type
                 (source composite)
                 (storage shared)
                 (access read-only)
                 (create-accessor read)
                 (default single-variable)))

         (defrule lex-element->single-variable
                  (declare (salience 1))
                  ?f <- (lex-element (value ?value&:(and (stringp ?value)
                                                         (starts-with "?" 
                                                                      ?value)))
                                     (index ?index)
                                     (parent ?parent))
                  =>
                  (retract ?f)
                  (make-instance of iris-single-variable
                                 (index ?index)
                                 (value ?value)
                                 (parent ?parent)))

         (defclass iris-multifield-variable
           (is-a iris-variable)
           (slot type
                 (source composite)
                 (storage shared)
                 (access read-only)
                 (create-accessor read)
                 (default multifield-variable)))

         (defrule lex-element->multifield-variable
                  (declare (salience 1))
                  ?f <- (lex-element (value ?value&:(and (stringp ?value)
                                                         (starts-with "$?" 
                                                                      ?value)))
                                     (index ?index)
                                     (parent ?parent))
                  =>
                  (retract ?f)
                  (make-instance of iris-multifield-variable
                                 (index ?index)
                                 (value ?value)
                                 (parent ?parent)))

         (defclass iris-instance-name
           (is-a iris-node)
           (slot type
                 (source composite)
                 (storage shared)
                 (access read-only)
                 (create-accessor read)
                 (default instance-name))
           (slot value
                 (source composite)
                 (type INSTANCE-NAME)))

         (defrule lex-element->instance-name
                  ?f <- (lex-element (value ?value&:(instance-namep ?value))
                                     (index ?index)
                                     (parent ?parent))
                  =>
                  (retract ?f)
                  (make-instance of iris-instance-name
                                 (index ?index)
                                 (value ?value)
                                 (parent ?parent)))
