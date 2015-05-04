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
(defglobal MAIN
           ?*first* = 10000
           ?*dead-last* = -10000
           ?*last* = -9999
           ?*priority1* = 1
           ?*priority2* = 2)

(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(defrule next-stage
         (declare (salience ?*dead-last*))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f (current ?next)
                 (rest ?rest)))
(defrule no-more-stages
         (declare (salience ?*dead-last*))
         ?f <- (stage (rest))
         =>
         (retract ?f))
(deffacts stages
          (stage (current startup)
                 (rest read 
                       lex 
                       pre-parse 
                       parse)))
(deffunction starts-with
             (?tgt ?str)
             (eq (str-index ?tgt 
                            ?str) 
                 1))
(deffunction fact-addressp
             (?q)
             (eq (class ?q) 
                 FACT-ADDRESS))
(deffunction external-addressp
             (?q)
             (eq (class ?q)
                 EXTERNAL-ADDRESS))
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
(deftemplate file-translation
             (slot path
                   (type LEXEME)
                   (default ?NONE))
             (slot logical-name
                   (type SYMBOL)
                   (default ?NONE)))
(deftemplate parse-location
             (slot count
                   (type INTEGER)
                   (range 0 ?VARIABLE)
                   (default ?NONE))
             (slot index
                   (type INTEGER)
                   (default-dynamic 0))
             (slot parent
                   (type SYMBOL)
                   (default ?NONE)))
(defrule open-file
         (stage (current read))
         ?f <- (open ?file)
         =>
         (retract ?f)
         (bind ?name (gensym*))
         (if (open ?file 
                   ?name 
                   "r") then
           (assert (file-translation (path ?name)
                                     (logical-name ?file))
                   (read elements from ?name))
           else
           (printout werror 
                     "Couldn't open " ?file crlf)))
(defrule read-elements
         (stage (current read))
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
         (assert (parse-location (count ?index)
                                 (parent ?rtr))))
(defgeneric rule-defined)
(defgeneric build-standard-lex-rule)
(defgeneric standard-lex-rule-name)
(defmethod build-standard-lex-rule
  ((?class-type SYMBOL)
   (?function SYMBOL))
  (build (format nil "(defrule lex-element->%s
                               (stage (current lex))
                               ?f <- (lex-element (value ?value&:(%s ?value))
                                                  (index ?index)
                                                  (parent ?parent))
                               =>
                               (retract ?f)
                               (make-instance of %s (parent ?parent)
                                              (value ?value)
                                              (index ?index)))" 
  ?class-type
  ?function
  ?class-type)))

(defmethod rule-defined
  ((?rule-name SYMBOL))
  (neq (member$ ?rule-name 
                (get-defrule-list))
       FALSE))
(defmethod rule-defined
  ((?rule-name STRING))
  (rule-defined (string-to-field ?rule-name)))

(defmethod standard-lex-rule-name
  ((?class SYMBOL))
  (sym-cat lex-element-> ?class))
; To do automatic generation of the basic lex rules it is necessary to define
; the rules and then unload the deffacts once finished as we don't want this to 
; run each time in the same expert system
(defrule build-lex-rule
         (stage (current startup))
         ?f <- (build-standard-lex-rule ?class-type
                                        ?function)
         =>
         (retract ?f)
         (build-standard-lex-rule ?class-type
                                  ?function))
(defrule delete-deffacts
         (declare (salience ?*last*))
         (stage (current startup))
         ?f <- (delete deffacts ?name)
         =>
         ; once all of the facts of a given deffacts have been processed, we can delete 
         ; the deffacts. This doesn't actually have to go last....
         (retract ?f)
         (undeffacts ?name))

(defrule build-subsymbol-type
         (stage (current startup))
         ?f <- (defsubsymbol-type ?symbol ?type)
         =>
         (retract ?f)
         (bind ?clazz 
               (format nil "(defclass iris-symbol-%s
                              (is-a iris-symbol)
                              (slot type 
                                    (source composite)
                                    (default %s))
                              (slot value 
                                    (source composite)
                                    (storage shared)
                                    (access read-only)
                                    (create-accessor read)
                                    (default %s)))" 
                       ?type 
                       ?type 
                       ?symbol))
         (build ?clazz)
         (build (format nil "
                        (defrule lex-element->iris-symbol-%s
                                 (declare (salience ?*priority2*))
                                 ?f <- (lex-element (value %s)
                                                    (index ?index)
                                                    (parent ?parent))
                                 =>
                                 (retract ?f)
                                 (make-instance of iris-symbol-%s (parent ?parent)
                                                (index ?index)))"
                        ?type
                        ?symbol
                        ?type)))

(defclass iris-node
  (is-a USER)
  (role abstract)
  (pattern-match non-reactive)
  (slot parent
        (type SYMBOL)
        (default ?NONE))
  (slot type
        (visibility public)
        (type SYMBOL)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default nil))
  (slot index
        (visibility public)
        (range 0 ?VARIABLE)
        (type INTEGER)
        (default ?NONE))
  (slot value
        (visibility public)
        (default ?NONE)))


(defclass iris-symbol
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default symbol))
  (slot value
        (source composite)
        (type SYMBOL)))
; some extra symbols
; :, &, |, ~
(deffacts initialization-iris-symbol
          (delete deffacts initialization-iris-symbol)
          (build-standard-lex-rule iris-symbol
                                   symbolp)
          (defsubsymbol-type + 
                             add)
          (defsubsymbol-type - 
                             subtract)
          (defsubsymbol-type / 
                             divide)
          (defsubsymbol-type div 
                             integer-divide)
          (defsubsymbol-type * 
                             multiply)
          (defsubsymbol-type < 
                             less-than)
          (defsubsymbol-type <= 
                             less-than-or-equal)
          (defsubsymbol-type > 
                             greater-than)
          (defsubsymbol-type >= 
                             greater-than-or-equal)
          (defsubsymbol-type <> 
                             not-integer-equals)
          (defsubsymbol-type eq 
                             equals)
          (defsubsymbol-type neq 
                             not-equals)
          (defsubsymbol-type bind 
                             bind-variable)
          (defsubsymbol-type not 
                             logical-not)
          (defsubsymbol-type and 
                             logical-and)
          (defsubsymbol-type or 
                             logical-or)
          (defsubsymbol-type deffunction 
                             define-function)
          (defsubsymbol-type defrule 
                             define-rule)
          (defsubsymbol-type defgeneric 
                             define-generic)
          (defsubsymbol-type defmethod 
                             define-method)
          (defsubsymbol-type defmessage-handler 
                             define-message-handler)
          (defsubsymbol-type defclass 
                             define-class)
          (defsubsymbol-type progn 
                             progn)
          (defsubsymbol-type progn$ 
                             progn$)
          (defsubsymbol-type if 
                             if-statement)
          (defsubsymbol-type defglobal 
                             define-global)
          (defsubsymbol-type create$ 
                             create-multifield)
          (defsubsymbol-type mod 
                             modulus)
          )


(defclass iris-string
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default string))
  (slot value
        (source composite)
        (type STRING)))

(deffacts initialization-iris-string
          (delete deffacts initialization-iris-string)
          (build-standard-lex-rule iris-string
                                   stringp))

(defclass iris-left-paren
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default left-paren))
  (slot value
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default "(")))

(defrule lex-element->left-paren
         (declare (salience ?*priority1*))
         (stage (current lex))
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
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default right-paren))
  (slot value
        (source composite)
        (storage shared)
        (access read-only)
        (create-accessor read)
        (default ")")))

(defrule lex-element->right-paren
         (declare (salience ?*priority1*))
         (stage (current lex))
         ?f <- (lex-element (value ")")
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-right-paren
                        (index ?index)
                        (parent ?parent)))

(defclass iris-integer
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default integer))
  (slot subtype
        (type SYMBOL)
        (allowed-symbols binary hex decimal)
        (default ?NONE))
  (slot value
        (source composite)
        (type INTEGER SYMBOL)))

(defrule lex-element->integer
         (stage (current lex))
         ?f <- (lex-element (value ?value&:(integerp ?value))
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-integer 
                        (subtype decimal)
                        (value ?value)
                        (index ?index)
                        (parent ?parent)))

(defrule lex-element->hex-integer
         (declare (salience ?*priority2*))
         (stage (current lex))
         ?f <- (lex-element (value ?value&:(and (symbolp ?value)
                                                (starts-with 0x 
                                                             ?value)))
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-integer
                        (subtype hex)
                        (value ?value)
                        (index ?index)
                        (parent ?parent)))

(defrule lex-element->binary-integer
         (declare (salience ?*priority2*))
         (stage (current lex))
         ?f <- (lex-element (value ?value&:(and (symbolp ?value)
                                                (starts-with 0b 
                                                             ?value)))
                            (index ?index)
                            (parent ?parent))
         =>
         (retract ?f)
         (make-instance of iris-integer
                        (subtype binary)
                        (value ?value)
                        (index ?index)
                        (parent ?parent)))
(defclass iris-float
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default float))
  (slot value
        (source composite)
        (type FLOAT)))

(deffacts initialization-iris-float
          (delete deffacts initialization-iris-float)
          (build-standard-lex-rule iris-float
                                   floatp))

(defclass iris-variable
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot value
        (source composite)
        (type LEXEME)))

(defclass iris-single-variable
  (is-a iris-variable)
  (slot type
        (source composite)
        (default single-variable)))

(defrule lex-element->single-variable
         (declare (salience ?*priority1*))
         (stage (current lex))
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
        (default multifield-variable)))

(defrule lex-element->multifield-variable
         (declare (salience ?*priority1*))
         (stage (current lex))
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
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default instance-name))
  (slot value
        (source composite)
        (type INSTANCE-NAME)))

(deffacts initialization-iris-instance-name
          (delete deffacts initialization-iris-instance-name)
          (build-standard-lex-rule iris-instance-name
                                   instance-namep))

(defclass iris-instance-address
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default instance-address))
  (slot value
        (source composite)
        (type INSTANCE-ADDRESS)))

(deffacts initialization-iris-instance-address
          (delete deffacts initialization-iris-instance-address)
          (build-standard-lex-rule iris-instance-address
                                   instance-addressp))

(defclass iris-fact-address
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default fact-address))
  (slot value
        (source composite)
        (type FACT-ADDRESS)))

(deffacts initialization-iris-fact-address
          (delete deffacts initialization-iris-fact-address)
          (build-standard-lex-rule iris-fact-address
                                   fact-addressp))

(defclass iris-external-address
  (is-a iris-node)
  (role concrete)
  (pattern-match reactive)
  (slot type
        (source composite)
        (default external-address))
  (slot value
        (source composite)
        (type EXTERNAL-ADDRESS)))

(deffacts initialization-iris-external-address
          (delete deffacts initialization-iris-external-address)
          (build-standard-lex-rule iris-external-address
                                   external-addressp))
