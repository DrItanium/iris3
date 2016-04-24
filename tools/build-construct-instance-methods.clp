(deffunction build-construct-instance-method
             (?input-type ?output-type)
             (format t 
                     "(defmethod construct-instance
                        \"convert a symbol of type %s to class of type %s\"
                        ((?class SYMBOL 
                                 (eq ?class 
                                     %s))
                         (?parent SYMBOL
                                  INSTANCE-NAME)
                         (?value LEXEME))
                        (construct-instance %s
                                            ?parent 
                                            ?value))%n%n"
                     ?input-type
                     ?output-type
                     ?input-type
                     ?output-type))

(build-construct-instance-method OR_CONSTRAINT 
                                 or-constraint)
(build-construct-instance-method AND_CONSTRAINT
                                 and-constraint)
(build-construct-instance-method NOT_CONSTRAINT
                                 not-constraint)
(build-construct-instance-method MF_WILDCARD
                                 multifield-wildcard)
(build-construct-instance-method SF_WILDCARD
                                 singlefield-wildcard)
(build-construct-instance-method MF_VARIABLE
                                 multifield-variable)
(build-construct-instance-method SF_VARIABLE
                                 singlefield-variable)
(build-construct-instance-method MF_GBL_VARIABLE
                                 multifield-global-variable)
(build-construct-instance-method GBL_VARIABLE
                                 singlefield-global-variable)

(exit)
