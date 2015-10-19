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
  (is-a thing)
  (slot variable
        (type INSTANCE)
        (default ?NONE))
  (multislot value)
  (message-handler is-unbind primary))

(defmessage-handler bind is-unbind primary
                    ()
                    (= (length$ ?self:value) 0))

(defrule parse-bind-operation
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents bind
                                 ?var
                                 $?value)
                       (parent ?parent)
                       (name ?name))
         (object (is-a variable)
                 (name ?var))
         =>
         (unmake-instance ?f)
         (make-instance ?name of bind
                        (parent ?parent)
                        (variable ?var)
                        (value ?value)))
; association rules
(defrule register-local-binds 
         "take ownership of the local bound variables found in the current function"
         (declare (salience ?*priority:two*))
         (stage (current associate))
         ?bind <- (object (is-a bind)
                          (variable ?var)
                          (name ?bind-name))
         ?v <- (object (is-a local-variable)
                       (name ?var))
         ?func <- (object (is-a function)
                          (name ?function)
                          (local-binds $?lb)
                          (arguments $?args))
         (test (and (not (neq ?function 
                              (send ?bind get-parent-chain)))
                    (neq ?var $?lb)
                    (neq ?var $?args)))
         =>
         ; now we need to take ownership of the variable in the bind since we didn't find it 
         ; in the local binds nor the arguments
         (bind ?ref (instance-name (make-instance of reference
                                                  (parent ?bind-name)
                                                  (value ?var))))
         (modify-instance ?v (parent ?function))
         (modify-instance ?bind (variable ?ref))
         (modify-instance ?func (local-binds $?lb ?var)))
