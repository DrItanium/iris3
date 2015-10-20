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

; defmessage-handler static-analysis rules

(defrule message-handler-name-starts-with-a-dot
         "having message-handlers which respond to messages beginning with . is a tad strange and decreases readability"
         (object (is-a defmessage-handler)
                 (title ?title&:(eq (str-index . ?title) 1))
                 (target-class ?class)
                 (handler-type ?ht))
         (object (is-a has-title)
                 (name ?class)
                 (title ?cname))
         =>
         (violation t "found a message handler tied to defclass " ?cname " which responds to a message starting with a '.'!" crlf
                    "This is a bad idea all around and shouldn't be done." crlf 
                    "The offending message handler is: " crlf crlf
                    "(defmessage-handler " ?cname " " ?title " " ?ht " .....)" crlf))



