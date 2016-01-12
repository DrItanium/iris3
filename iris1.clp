(defrule inspect-deffunction:too-many-arguments
	 "Currently we only support 32 arguments max to a deffunction in iris1"
	 (declare (salience 1))
	 (stage (current build))
	 (object (is-a deffunction)
		 (arguments $?args&:(> (length$ ?args) 32))
		 (function-name ?title))
	 =>
	 (printout werror "ERROR: function " ?title " has more than 32 arguments!" crlf)
	 (exit 1))

(defrule inspect-deffunction:too-many-local-binds
	 "Currently we only support 32 local binds max to a deffunction in iris1"
	 (declare (salience 1))
	 (stage (current build))
	 (object (is-a deffunction)
		 (local-binds $?args&:(> (length$ ?args) 32))
		 (function-name ?title))
	 =>
	 (printout werror "ERROR: function " ?title " has more than 32 local binds!" crlf)
	 (exit 1))

(defrule inspect-deffunction:no-rest-argument
	 "currently we don't support the rest param in a deffunction"
	 (declare (salience 1))
	 (stage (current build))
	 (object (is-a deffunction)
		 (arguments $? ?last)
		 (function-name ?title))
	 (object (is-a multifield-variable)
		 (name ?last))
	 =>
	 (printout werror "ERROR: function " ?title " contains a rest parameter, not supported!" crlf)
	 (exit 1))

(defrule associate-inputs-with-registers
	 (stage (current build))
	 (object (is-a deffunction)
		 (arguments $?ind ?arg $?))
	 ?q <- (object (is-a singlefield-variable)
		       (name ?arg)
		       (register UNKNOWN))
	 =>
	 (modify-instance ?q 
			  (register (sym-cat i 
					     (length$ ?ind)))))

(defrule associate-locals-with-registers
	 (stage (current build))
	 (object (is-a deffunction)
		 (local-binds $?ind ?arg $?))
	 ?q <- (object (is-a singlefield-variable)
		       (name ?arg)
		       (register UNKNOWN))
	 =>
	 (modify-instance ?q 
			  (register (sym-cat l 
					     (length$ ?ind)))))
(defclass loop-for-count
  (is-a thing)
  (slot iterator
	(storage local)
	(visibility public)
	(default ?NONE))
  (slot from
	(storage local)
	(visibility public)
	(default ?NONE))
  (slot to
	(storage local)
	(visibility public)
	(default ?NONE))
  (multislot body
	     (visibility public)))

(defrule select-loop-for-count:format0
	 (stage (current build))
	 ?f <- (object (is-a list)
		       (contents loop-for-count ?iterator do $?body)
		       (name ?name)
		       (parent ?parent))
	 ?f2 <- (object (is-a list)
			(name ?iterator)
			(contents ?index ?range))
	 ?f3 <- (object (is-a list)
			(name ?range)
			(contents ?from ?to))
	 ?f4 <- (object (is-a thing)
			(name ?to))
	 =>
	 (unmake-instance ?f ?f2 ?f3)
	 (modify-instance ?f4 (parent ?name))
	 (modify-instance ?index (parent ?name))
	 (make-instance ?name of loop-for-count
			(parent ?parent)
			(iterator ?index)
			(from ?from)
			(to ?to)
			(body ?body)))


(defclass if-statement
  (is-a thing)
  (slot condition
	(storage local)
	(visibility public)
	(default ?NONE))
  (slot on-true
	(storage local)
	(visibility public)
	(default ?NONE))
  (slot on-false
	(storage local)
	(visibility public)
	(default-dynamic FALSE)))

(defrule construct-if-statement:full
	 (stage (current build))
	 ?f <- (object (is-a list)
		       (contents if ?condition then ?true else ?false)
		       (name ?name)
		       (parent ?parent))
	 =>
	 (unmake-instance ?f)
	 (make-instance ?name of if-statement
			(parent ?parent)
			(condition ?condition)
			(on-true ?true)
			(on-false ?false)))

(defrule construct-if-statement:only-true
	 (stage (current build))
	 ?f <- (object (is-a list)
		       (contents if ?condition then ?true)
		       (name ?name)
		       (parent ?parent))
	 =>
	 (unmake-instance ?f)
	 (make-instance ?name of if-statement
			(parent ?parent)
			(condition ?condition)
			(on-true ?true)))
(defclass comparison
  (is-a thing)
  (slot operation
	(type SYMBOL)
	(visibility public)
	(storage local)
	(default ?NONE))
  (multislot arguments))
(deffunction build-comparison-operation
	     (?operation ?title)
	     (build (format nil "(defrule construct-%s-comparison-%s
					  (stage (current build))
					  ?f <- (object (is-a list)
							(contents %s $?rest)
							(name ?name)
							(parent ?parent))
					  =>
					  (unmake-instance ?f)
					  (make-instance ?name of comparison
							 (operation %s)
							 (parent ?parent)
							 (arguments $?rest)))"
			    ?title
			    (gensym*)
			    ?operation
			    ?operation)))

(build-comparison-operation < 
			    less-than)
(build-comparison-operation >
			    greater-than)
(build-comparison-operation <=
			    less-than-or-equal)
(build-comparison-operation >=
			    greater-than-or-equal)
(build-comparison-operation <>
			    not-equal)
(build-comparison-operation !=
			    not-equal)

(deffunction eq-op () =)
(defrule build-equal-comparison-operator
	 (stage (current build))
	 ?f <- (object (is-a list)
		       (contents =(eq-op) $?rest)
		       (name ?name)
		       (parent ?parent))
	 =>
	 (unmake-instance ?f)
	 (make-instance ?name of comparison
			(operation =)
			(parent ?parent)
			(arguments ?rest)))

(defclass return
  (is-a thing)
  (slot value
	(visibility public)
	(storage local)))

(defrule build-return-statement
	 (stage (current build))
	 ?f <- (object (is-a list)
		       (contents return ?value)
		       (name ?name)
		       (parent ?parent))
	 =>
	 (unmake-instance ?f)
	 (make-instance ?name of return
			(value ?value)
			(parent ?parent)))
(defclass nth$
  (is-a thing)
  (slot index
	(visibility public)
	(storage local))
  (slot multifield
	(visibility public)
	(storage local)))

(defrule build-nth$-statement
	 (stage (current build))
	 ?f <- (object (is-a list)
		       (contents nth$ ?index ?field)
		       (name ?name)
		       (parent ?parent))
	 =>
	 (unmake-instance ?f)
	 (make-instance ?name of nth$
			(index ?index)
			(multifield ?field)
			(parent ?parent)))

