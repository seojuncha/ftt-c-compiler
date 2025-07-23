(defpackage :ftt-cc
  (:use :cl)
  (:export
    #:start
    #:version))

(defpackage :ftt-cc.token
  (:use :cl)
  (:export
    #:token
    #:tok-kind
    #:tok-lexeme
    #:is?
    #:dump-token))

(defpackage :ftt-cc.lexer
  (:use :cl)
  (:export
    #:init-lexer
    #:lex))

(defpackage :ftt-cc.parser
  (:use :cl)
  (:export
    #:init-parser
    #:parse-ast))

(defpackage :ftt-cc.ast
  (:use :cl)
  (:export 
    #:create-ast-binary-operator
    #:create-ast-integer-literal
    #:create-ast-identifier
    #:create-ast-return-stmt
    #:create-ast-compound-stmt
    #:tok-kind->op-kind
    #:dump-ast))
