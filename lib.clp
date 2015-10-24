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
(defgeneric all-match$
            "Checks to see if a given predicate applies to all elements in the provided list")

(defmethod all-match$
 ((?fn SYMBOL)
  (?list MULTIFIELD))
 (progn$ (?a ?list)
         (if (not (funcall ?fn ?a)) then
             (return FALSE)))
 TRUE)
(defmethod apply$
  ((?fn SYMBOL)
   (?list MULTIFIELD))
  (bind ?output 
        (create$))
  (progn$ (?a ?list)
          (bind ?output 
                ?output 
                (funcall ?fn ?a)))
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
            (bind ?output 
                  ?output
                  ?a)))
  (return ?output))

(defmethod filter$
  ((?fn SYMBOL)
   $?list)
  (filter$ ?fn ?list))
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
  (bind ?strip (sub-string 3 (length$ ?a) ?a))
  (bind ?len (length$ ?strip))
  ; Now go through and build up the number by extracting the current char
  ; converting it and then shifting left by the position of the digit in the
  ; original "number"
  (loop-for-count (?ind 1 ?len) do
                  (bind ?result 
                        (+ ?result 
                           (left-shift 
                             (hexchar-to-number (sub-string ?ind 
                                                            ?ind 
                                                            ?strip))
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
  (exists$ ?fn ?list))
