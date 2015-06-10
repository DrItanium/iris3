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

(defclass deftemplate
  (is-a thing
        has-comment)
  (slot template-name
        (type SYMBOL)
        (default ?NONE))
  (multislot slots))

(defrule translate-deftemplate:comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deftemplate
                                 ?template-name 
                                 ?comment
                                 $?slots)
                       (parent ?parent)
                       (name ?name))
         ?f2 <- (object (is-a string)
                        (name ?comment)
                        (value ?cvalue))
         =>
         (unmake-instance ?f ?f2)
         (make-instance ?name of deftemplate
                        (template-name ?template-name)
                        (comment ?cvalue)
                        (parent ?parent)
                        (slots $?slots)))

(defrule translate-deftemplate:no-comment
         (stage (current parse))
         ?f <- (object (is-a list)
                       (contents deftemplate
                                 ?template-name 
                                 $?slots&:(no-strings-in-list ?slots))
                       (parent ?parent)
                       (name ?name))
         =>
         (unmake-instance ?f)
         (make-instance ?name of deftemplate
          (template-name ?template-name)
                        (parent ?parent)
                        (slots $?slots)))

