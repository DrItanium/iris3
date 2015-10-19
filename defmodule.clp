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

(defclass defmodule
  (is-a thing
        has-title
        has-comment)
  (multislot specifications))
; TODO: flesh out further
(defrule build-defmodule:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defmodules
                                 ?name
                                 ?comment
                                 $?specs))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of defmodule
                        (title ?name)
                        (parent  ?parent)
                        (comment ?cvalue)
                        (specifications ?specs)))

(defrule build-defmodule:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (parent ?parent)
                       (contents defmodules
                                 ?name
                                 $?specs&:(no-strings-in-list ?specs)))
         =>
         (unmake-instance ?f)
         (make-instance ?name of defmodule
                        (parent  ?parent)
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
(defclass import-specification
  (is-a port-specification)
  (role concrete)
  (pattern-match reactive)
  (slot module-name
        (type SYMBOL)
        (visibility public)
        (default ?NONE)))

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
         (make-instance ?b of export-specification
                        (parent ?parent)
                        (items $?rest)))
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
                                   ?qualifier))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?qualifier)
                        (value "?ALL"|"?NONE"))
         =>
         (modify-instance ?f 
                          (construct ?construct)
                          (qualifiers ?qualifier)))
(defrule expand-export-specification:specific-construct-and-qualifiers
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric
                                   $?qualifiers&:(and (not (empty$ ?qualifiers))
                                                      (no-primitive-strings-in-list $?qualifiers))))
         =>
         (modify-instance ?f
                          (construct ?construct)
                          (qualifiers ?qualifiers)))

(defrule expand-export-specification:error:no-qualifiers
         (stage (current parse))
         ?f <- (object (is-a export-specification)
                       (construct undefined)
                       (qualifiers ?construct&deftemplate|defclass|defglobal|deffunction|defgeneric))
         =>
         (printout werror "ERROR: no qualifiers specified for export-specification!" crlf)
         (halt))


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
         (make-instance ?b of import-specification
                        (parent ?parent)
                        (qualifiers ?rest)))

(defrule expand-import-specification:all-or-none
         (stage (current parse))
         ?f <- (object (is-a import-specification)
                       (construct undefined)
                       (qualifiers ?module-name&:(symbolp ?module-name)
                                   ?qualifier))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?qualifier)
                        (value "?ALL"|"?NONE"))
         =>
         (unmake-instance ?f2)
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
                                    ?qualifier))
         ?f2 <- (object (is-a singlefield-variable)
                        (name ?qualifier)
                        (value "?ALL"|"?NONE"))
         =>
         (unmake-instance ?f2)
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
                                   $?qualifiers&:(and (not (empty$ ?qualifiers))
                                                      (no-primitive-strings-in-list $?qualifiers))))
         =>
         (modify-instance ?f
                          (module-name ?module-name)
                          (construct ?construct)
                          (qualifiers ?qualifiers)))
