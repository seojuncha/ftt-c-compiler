(in-package :ftt-cc.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl)
  (use-package :ftt-cc.token)
  (use-package :ftt-cc.lexer)
  (use-package :ftt-cc.ast))

; this is the temporary counter!
(defparameter *count* 0)

;; operation precedence
(defparameter +op-prec-unknown+ 0) ; not binary operator, e.g) numeric constant, identifier
(defparameter +op-prec-comma+ 1)
(defparameter +op-prec-assignment+ 2)
(defparameter +op-prec-additive+ 3)

;;; utilities
(defun bin-op-precedence (tokkind)
  (case tokkind
    (:tok-equal +op-prec-assignment+)
    (:tok-minus +op-prec-additive+)
    (:tok-plus +op-prec-additive+)
    (:tok-comma +op-prec-comma+)
    (otherwise +op-prec-unknown+)))

;;; current token to be lexed
(defparameter *cur-tok* (make-instance 'token))

;;; lookahead token symbol
(defparameter *lookahead-tok* nil)

(defun parse-ast ()
  (parse-translation-unit))

(defun init-parser ()
  (consume-token)
  (setf *count* 0))

(defun consume-token ()
  ;; just lex the global token instance at now.
  (setf *cur-tok* (lex))
  (format t "CONSUMED TOKEN: ~s~%" (tok-lexeme *cur-tok*)))

; todo: connect with the block scope manager
(defparameter *paran-count* 0)
(defparameter *brace-count* 0)

(defun consume-open (tokkind)
  (cond ((eq :tok-l-brace tokkind)
          (consume-brace))
        (t (format t "invalid consume open: ~a~%" tokkind))))

(defun consume-close (tokkind)
  (cond ((eq :tok-r-brace tokkind)
          (consume-brace))
        (t (format t "invalid consume close: ~a~%" tokkind))))

(defun consume-brace ()
  (let ((kind (tok-kind *cur-tok*)))
    (cond ((eq :tok-l-brace kind)
            (incf *brace-count*))
          ((eq :tok-r-brace kind)
            (decf *brace-count*))
          (t
            (format t "invalid consume brace~%"))))
  (format t "BRACE COUNT: ~d~%" *brace-count*)
  (setf *cur-tok* (lex)))

(defun skip-until (tokkind)
  (loop while (eq tokkind (tok-kind *cur-tok*)) do
          (consume-token)))

(defun next-token ())

(defun lookahead-token (size))

; (6.9) translation-unit:
;   external-declaration
;   translation-unit external-declaration
(defun parse-translation-unit ()
  (parse-external-declaration))

; (6.9) external-declaration:
;   function-definition
;   declaration
;
; function-definition:
;   declaration-specifiers declarator declaration-list[opt] compound-statement
;
; declaration:
;   declaration-specifiers init-declarator-list[opt] ;
(defun parse-external-declaration ()
  (let* ((declspec (parse-declaration-specifiers)) ; read the function return type (e.g. void, int)
                                                  (decl (parse-declarator declspec)))
    (create-ast-translation-unit-decl (parse-function-definition decl))))

; (6.9.1) function-definition:
;   declaration-specifiers declarator declaration-list[opt] compound-statement
;
; declaration-list:
;   declaration
;   declaration-list declaration
;
; e.g)
;  foo(void) => from declarator
;  max(int a, int b) => from declarator
;
; NOT SUPPORT declaration-list[opt]
(defun parse-function-definition (decl)
  (create-ast-function-decl decl (parse-compound-statement)))

(defun parse-function-declarator (declspec name)
  (format t "  [FUNC] parse-function-declarator~%")
  (unless (eq :tok-l-paran (tok-kind *cur-tok*))
    (format t "[ERROR] not start with the left paran~%")
    (return-from parse-function-declarator nil))

  (consume-token) ; eat the left paran. (consume-open :tok-l-paran)
  (let ((ret (make-decl
              :spec declspec
              :name name
              :params (parse-paramater-declaration))))
    (format t "final function decl: ")
    (format t "~a~%" ret)
    (consume-token) ; eat the right paran. (consume-close :tok-r-paran)
    ret))

; (6.7) declaration:
;   declaration-specifiers init-declarator-list[opt] ;
;
; init-declarator-list:
;   init-declarator
;   init-declarator-list , init-declarator
;
; init-declarator:
;   declarator
;   declarator = initializer
(defun parse-declaration ())

; (6.7.5) declarator:
;   pointer[opt] direct-declarator
;
; int a;
;     ^ declarator
; int* a;
;    ^^^ declarator
; int* foo(void);
;    ^^^^^ declarator
(defun parse-declarator (declspec)
  (format t "  [FUNC] parse-declarator~%")
  (let ((kind (tok-kind *cur-tok*)))
    (if (eq :tok-star kind)
        (progn
         (format t "is pointer type!~%")
         nil) ; todo: process pointer
        (progn
         (format t "is NOT pointer type!~%")
         (parse-direct-declarator declspec))))) ; return decl

; (6.7.5) direct-declarator:
;   identifier
;   ( declarator )
;   direct-declarator [ type-qualifier-list[opt] assignment-expressionopt ]
;   direct-declarator [ static type-qualifier-list[opt] assignment-expression ]
;   direct-declarator [ type-qualifier-list static assignment-expression ]
;   direct-declarator [ type-qualifier-list[opt] * ]
;   direct-declarator ( parameter-type-list )
;   direct-declarator ( identifier-list[opt] )
(defun parse-direct-declarator (declspec)
  (format t "  [FUNC] parse-direct-declarator~%")
  (let ((kind (tok-kind *cur-tok*)) ident)
    (unless (eq :tok-identifier kind)
      (format t "is not identifier: ~a~%" kind)
      (return-from parse-direct-declarator nil))
    (setf ident (tok-lexeme *cur-tok*))
    (consume-token) ; eat the identifier (would be a function name), *cur-tok* will be tok-l-paran
    (parse-function-declarator declspec ident)))


; direct-declarator:
;   ( declarator )
;   direct-declarator ( parameter-type-list )
;   direct-declarator ( identifier-list[opt] )
(defun parse-paran-declarator ())


; (6.7.5) parameter-type-list:
;   parameter-list
;   parameter-list , ...
;
; parameter-list:
;   parameter-declaration
;   parameter-list , parameter-declaration
;
; parameter-declaration:
;   declaration-specifiers declarator
;   declaration-specifiers abstract-declarator[opt]
(defun parse-paramater-declaration () ; return the array of the parameter
  (format t "  [FUNC] parse-paramater-declaration~%")
  (let ((param-vec (make-array 3 :adjustable t :fill-pointer 3))) ; temp, accept 3 parameters.
    (loop ; loop until the right paran
       until (is? *cur-tok* :tok-r-paran) do
         (let* ((declspec (parse-declaration-specifiers))
                (decl (if (eq :tok-r-paran (tok-kind *cur-tok*)) nil (parse-declarator declspec))))
           (vector-push
             (make-decl :name (if decl (decl-name decl) "")
                        :spec declspec
                        :params nil)
             param-vec))
         (format t "param length: ~d~%" (length param-vec))
         ; (consume-token)
         (format t "check point ~a | ~s~%" (tok-kind *cur-tok*) (tok-lexeme *cur-tok*))
       finally (format t "end of loop, curtok: ~a | ~s~%" (tok-kind *cur-tok*) (tok-lexeme *cur-tok*)))
    param-vec))

; (6.7) declaration-specifiers:
;   storage-class-specifier declaration-specifiers[opt]
;   type-specifier declaration-specifiers[opt]
;   type-qualifier declaration-specifiers[opt]
;   function-specifier declaration-specifiers[opt]
(defstruct declspec
  type-spec
  type-qualifier
  storage-spec
  func-spec)

(defstruct decl
  spec
  name
  params)

(defparameter *declspec-type*
              '(:dst-void
                :dst-int))

; return declspec (declaration-specifier)
(defun parse-declaration-specifiers ()
  (format t "  [FUNC] parse-declaration-specifiers~%")
  (format t "~a~%" (tok-kind *cur-tok*))
  (let ((kind (tok-kind *cur-tok*)))
    (case kind
      (:kw-void
       (consume-token) ; consume keyword type. points to the identifier.
       (make-declspec :type-spec :dst-void))
      (:kw-int
       (consume-token)
       (make-declspec :type-spec :dst-int))
      (otherwise
       (format t "unkonwn decl spec: ~a~%" kind)
       nil))))


(defun parse-expression ()
  (format t "  [FUNC] parse-expression~%")

  (let ((lhs (parse-assignment-expression)))
    (format t "call in parse-expression~%")
    (parse-rhs-of-binary-expression lhs +op-prec-comma+)))

;;; (6.5.16) assignment-expression:
;;;   conditional-expression
;;;   unary-expression assignment-operator assignment-expression
;;;
;;; The assignment-expression is also a binary operation.
;;; conditional-expression and uanry-expression is pared in parse-cast-expression
;;; so LHS is from parse-cast-expression,
(defun parse-assignment-expression ()
  (format t "<<< parse-assignment-expression~%")
  (let ((lhs (parse-cast-expression)))
    ; (format t "LHS in assignment-expression: ~%")
    ; (dump-ast lhs)
    (format t "call in parse-assignment-expression~%")
    (parse-rhs-of-binary-expression lhs +op-prec-assignment+)))

(defun parse-cast-expression ()
  (format t "  !!! parse-cast-expression~%")
  (let ((saved-kind (tok-kind *cur-tok*))
        (result nil))
    (cond ((eq saved-kind :tok-identifier)
            (setf result (create-ast-identifier (tok-lexeme *cur-tok*)))
            (consume-token)
            result)
          ((eq saved-kind :tok-numeric-constant)
            (setf result (create-ast-integer-literal (tok-lexeme *cur-tok*)))
            (consume-token)
            result)
          ((or (eq saved-kind :tok-plus)
               (eq saved-kind :tok-minus)
               (eq saved-kind :tok-equal)
               (eq saved-kind :tok-semi)
               (eq saved-kind :tok-unkonwn))
            (consume-token)
            result)
          (t
            (format t "[ERROR] unknown token: ~a~%" saved-kind)))))

(defun parse-rhs-of-binary-expression (lhs minprec)
  ; (terpri)
  (format t ">>> parse-rhs-of-binary-expression~%")
  ; (format t "[~d] LHS: ~%" (incf *count*))
  ; (dump-ast lhs)
  (terpri)
  (format t "curtok: ~a~%" *cur-tok*)

  (let ((ret nil))
    (loop with optok = nil and lhs = lhs and rhs = nil
            and thisprec = nil and nextprec = nil
          do
            (when ret (setf lhs ret))

            (setf nextprec (bin-op-precedence (tok-kind *cur-tok*)))

            (when (< nextprec minprec)
                  (format t "termination by precedence [~d/~d] ~%" nextprec minprec)
                  (return-from parse-rhs-of-binary-expression lhs))

            ; will(and must) be an operator.
            (setf optok *cur-tok*)
            (dump-token optok)
            (consume-token)

            (setf rhs (parse-cast-expression))

            (setf thisprec nextprec)
            ; (format t "curtok: ~a~%" *cur-tok*)
            (setf nextprec (bin-op-precedence (tok-kind *cur-tok*)))

            (let ((right-associative? (eq thisprec +op-prec-assignment+)))
              (when (or (< thisprec nextprec) (and (= thisprec nextprec) right-associative?))
                    (setf rhs (parse-rhs-of-binary-expression rhs (incf thisprec)))
                    (setf nextprec (bin-op-precedence (tok-kind *cur-tok*)))))

            ; (format t "check point---------------~%")
            ; (format t "LHS=======~%")
            ; (dump-ast lhs)
            ; (format t "RHS=======~%")
            ; (dump-ast rhs)
            ; (terpri)
            (setf ret (create-ast-binary-operator
                       (tok-kind->op-kind (tok-kind optok)) lhs rhs)))))


; distinguish declaration and statements in top-level
(defun parse-declaration-or-statement ()
  ; check if it is declaration
  (parse-statement))

; (6.8) statement:
;   labeled-statement
;   compound-statement
;   expression-statement
;   selection-statement
;   iteration-statement
;   jump-statement
(defun parse-statement ()
  (format t "  [FUNC] parse-statement~%")
  (format t "curtok: ~a~%" *cur-tok*)
  (let ((kind (tok-kind *cur-tok*)))
    ; check if it is declaration?
    (cond
     ((eq kind :tok-l-brace)
       (return-from parse-statement (parse-compound-statement)))
     ((eq kind :kw-return)
       (return-from parse-statement (parse-return-statement)))
     ; ((eq kind :tok-semi)
     ;   (consume-token) (format t "last???~%"))
     (t (format t "[ERROR] unknown keyword: ~a~%" kind)))))

; return expressionopt ;
(defun parse-return-statement ()
  (format t "  [FUNC] pasre-return-statement~%")
  (consume-token) ; consume 'return'
  (let ((ret-expr (parse-expression)))
    (format t "IN RETURN : ~a~%" ret-expr)
    (dump-ast ret-expr)
    (consume-token) ;; temp...? eat ';'
    (create-ast-return-stmt ret-expr)))

(defun parse-compound-statement ()
  (format t "  [FUNC] parse-compound-statement~%")
  (let ((stmts '()))
    (consume-open :tok-l-brace)
    (loop
   until (is? *cur-tok* :tok-r-brace) do
     (push (parse-declaration-or-statement) stmts))
    (format t "the number of stmts: ~d~%" (length stmts))
    (consume-close :tok-r-brace)
    (create-ast-compound-stmt stmts)))