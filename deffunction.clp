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

(defclass deffunction
  (is-a thing
        has-comment)
  (slot function-name
        (type SYMBOL)
        (default ?NONE))
  (multislot arguments)
  (multislot body))

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
         (progn$ (?ag ?a) 
                 (send ?ag put-parent ?name))
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
         (progn$ (?ag ?a) 
                 (send ?ag put-parent ?name))
         (make-instance ?name of deffunction
                        (function-name ?func-name)
                        (parent ?parent)
                        (arguments ?a)
                        (body ?body)))


