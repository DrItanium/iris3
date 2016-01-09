(defrule inspect-deffunction:too-many-arguments
	 "Currently we only support 32 arguments max to a deffunction in iris1"
	 (stage (current build))
	 (object (is-a deffunction)
		 (arguments $?args&:(> (length$ ?args) 32))
		 (function-name ?title))
	 =>
	 (printout werror "ERROR: function " ?title " has more than 32 arguments!" crlf)
	 (exit 1))

(defrule inspect-deffunction:no-rest-argument
	 "currently we don't support the rest param in a deffunction"
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
	 (modify-instance ?q (register (sym-cat i (length$ ?ind)))))
