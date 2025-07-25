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
   #:declspec
   #:decl
   #:decl-spec
   #:decl-name
   #:decl-params
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
   #:create-ast-function-decl
   #:create-ast-translation-unit-decl
   #:tok-kind->op-kind
   #:dump-ast))
