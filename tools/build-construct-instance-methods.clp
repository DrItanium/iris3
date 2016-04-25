(deffunction build-construct-instance-method
             (?input-type ?output-type)
             (format t 
                     "(defrule construct-special-instance:%s
                               \"convert a symbol of type %s to class of type %s\"
                               (declare (salience 2))
                               (stage (current lex))
                               ?f <- (object (is-a file)
                                             (elements %s
                                                       ?value)
                                             (top ?top 
                                                  $?)
                                             (router ?r))
                               (object (is-a list)
                                       (name ?top)
                                       (contents $?contents))
                               =>
                               (modify-instance ?top
                                                (contents ?contents
                                                          (make-instance of %s
                                                                         (parent ?top)
                                                                         (value ?value))))
                               (modify-instance ?f 
                                                (elements (next-token ?r))))%n%n
                     (defrule construct-special-instance-outside-list:%s
                              \"convert a symbol of type %s to class of type %s\"
                              (declare (salience 2))
                              (stage (current lex))
                              ?f <- (object (is-a file)
                                            (elements %s
                                                      ?value)
                                            (top ?file)
                                            (name ?file)
                                            (router ?r))
                              =>
                              (printout werror
                                        \"WARNING: Found a special tag outside a list!\" crlf)
                              (modify-instance ?f
                                               (elements (next-token ?r)))
                              (make-instance of %s
                                             (parent ?file)
                                             (value ?value)))%n%n"
                     ?output-type
                     ?input-type
                     ?output-type
                     ?input-type
                     ?output-type
                     ?output-type
                     ?input-type
                     ?output-type
                     ?input-type
                     ?output-type
                     ))

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
