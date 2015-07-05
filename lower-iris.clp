(defclass iris:register
  (is-a USER)
  (slot value
        (type INTEGER)
        (range 0 65535)
        (default-dynamic 0)))
(defclass iris:high-level-op
  (is-a USER)
  (slot destination
        (type SYMBOL
              INSTANCE-NAME)
        (allowed-classes iris:register)
        (default ?NONE))
  (multislot operations
             (default ?NONE)))

(defgeneric try-convert-register)
(defgeneric iris:move:form)
(defgeneric iris:arithmetic:form)
(defgeneric iris:add)
(defgeneric iris:sub)
(defgeneric iris:mul)
(defgeneric iris:div)
(defgeneric iris:rem)
(defgeneric iris:shl)
(defgeneric iris:shr)
(defgeneric iris:and)
(defgeneric iris:or)
(defgeneric iris:not)
(defgeneric iris:xor)
(defgeneric iris:set)

(defmethod try-convert-register
  ((?value SYMBOL))
  ?value)
(defmethod try-convert-register
  ((?value iris:register 
           (instance-namep ?value)))
  (instance-name-to-symbol ?value))
(defmethod try-convert-register
  ((?value iris:register 
           (instance-addressp ?value)))
  (instance-name-to-symbol (instance-name ?value)))

(defglobal MAIN
           ?*iris-arithmetic-form-register* = "%s %s %s %s"
           ?*iris-arithmetic-form-immediate* = "%s %s %s %d"
           ?*scratch-register0* = sr0
           ?*scratch-register1* = sr1
           ?*scratch-register2* = sr2
           ?*scratch-register3* = sr3
           ?*scratch-register4* = sr4
           ?*scratch-register5* = sr5
           ?*scratch-register6* = sr6
           ?*scratch-register7* = sr7)
(defmethod iris:arithmetic:form
  ((?fmt STRING)
   (?op SYMBOL)
   (?destination SYMBOL)
   (?source0 SYMBOL)
   (?source1 SYMBOL
             INTEGER))
  (format nil ?fmt ?op ?destination ?source0 ?source1))
(defmethod iris:arithmetic:form
  ((?op SYMBOL)
   (?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form ?*iris-arithmetic-form-register*
                        ?op
                        (try-convert-register ?destination)
                        (try-convert-register ?source0)
                        (try-convert-register ?source1)))

(defmethod iris:arithmetic:form
  ((?op SYMBOL)
   (?destination SYMBOL)
   (?source0 SYMBOL)
   (?source1 INTEGER 
             (<= 0 ?source1 255)))
  (iris:arithmetic:form ?*iris-arithmetic-form-immediate*
                        ?op
                        (try-convert-register ?destination)
                        (try-convert-register ?source0)
                        ?source1))

(defmethod iris:arithmetic:form
  ((?op SYMBOL)
   (?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER
             (not (<= 0 ?source1 255))))
  ; need to actually construct two instructions to fit it
  (create$ (iris:set ?*scratch-register0* ?source1)
           (iris:arithmetic:form ?op ?destination ?source0 ?*scratch-register0*)))

(defmethod iris:add
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form add ?destination ?source0 ?source1))
(defmethod iris:add
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form addi ?destination ?source0 ?source1))

(defmethod iris:sub
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form sub ?destination ?source0 ?source1))
(defmethod iris:sub
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form subi ?destination ?source0 ?source1))

(defmethod iris:mul
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form mul ?destination ?source0 ?source1))
(defmethod iris:mul
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form muli ?destination ?source0 ?source1))
(defmethod iris:div
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form div ?destination ?source0 ?source1))
(defmethod iris:div
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form divi ?destination ?source0 ?source1))
(defmethod iris:rem
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form rem ?destination ?source0 ?source1))
(defmethod iris:rem
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form remi ?destination ?source0 ?source1))
(defmethod iris:shl
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form shl ?destination ?source0 ?source1))
(defmethod iris:shl
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form shli ?destination ?source0 ?source1))
(defmethod iris:shr
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form shr ?destination ?source0 ?source1))
(defmethod iris:shr
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 INTEGER))
  (iris:arithmetic:form shri ?destination ?source0 ?source1))

(defmethod iris:and
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form and ?destination ?source0 ?source1))
(defmethod iris:or
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form or ?destination ?source0 ?source1))
(defmethod iris:not
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register))
  (format nil 
          "not %s %s" 
          (try-convert-register ?destination)
          (try-convert-register ?source0)))
(defmethod iris:xor
  ((?destination SYMBOL
                 iris:register)
   (?source0 SYMBOL
             iris:register)
   (?source1 SYMBOL
             iris:register))
  (iris:arithmetic:form xor ?destination ?source0 ?source1))

(defmethod iris:set
  ((?destination SYMBOL
                 iris:register)
   (?source0 INTEGER))
  (format nil 
          "set %s %d" 
          (try-convert-register ?destination) 
          ?source0))
