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

; bind operations

(defclass bind
  (is-a node)
  (slot variable
        (type INSTANCE)
        (default ?NONE))
  (multislot value
             (default ?NONE)))


(defrule parse-bind-operation
         (stage (current parse))
         (object (is-a list)
                 (contents bind
                           ?var
                           $?value)
                 (parent ?parent)
                 (name ?name))
         (object (is-a variable)
                 (name ?var))
         =>
         (unmake-instance ?name)
         (make-instance ?name of bind
                        (parent ?parent)
                        (variable ?var)
                        (value ?value)))
; association rules
(defrule register-local-binds 
         "take ownership of the local bound variables found in the current function"
         (declare (salience ?*priority:two*))
         (stage (current associate))
         (object (is-a bind)
                 (variable ?var)
                 (name ?bind))
         (object (is-a local-variable)
                 (name ?var))
         (object (is-a function)
                 (name ?function&:(send ?bind parent-is ?function))
                 (local-binds $?lb&:(not (member$ ?var 
                                                  ?lb)))
                 (arguments $?args&:(not (member$ ?var 
                                                  ?args))))
         =>
         ; now we need to take ownership of the variable in the bind since we didn't find it 
         ; in the local binds nor the arguments
         (modify-instance ?var
                          (parent ?function))
         (bind ?ref-construction
               (symbol-to-instance-name (gensym*)))
         (modify-instance ?bind
                          (variable ?ref-construction))
         (assert (make-reference ?ref-construction ?bind ?var))
         (slot-insert$ ?function
                       local-binds
                       1
                       ?var))

(defrule build-reference-from-fact
         (declare (salience ?*priority:one*))
         (stage (current associate))
         ?f <- (make-reference ?id ?parent ?value)
         =>
         (retract ?f)
         (make-instance ?id of reference
                        (parent ?parent)
                        (value ?value)))

                       


