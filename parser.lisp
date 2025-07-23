(in-package :ftt-cc.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl)
  (use-package :ftt-cc.token)
  (use-package :ftt-cc.lexer)
  (use-package :ftt-cc.ast)) 

; (import '(ftt-cc.token:token
;           ftt-cc.token:tok-kind
;           ftt-cc.token:tok-lexeme
;           ftt-cc.token:dump-token
;           ftt-cc.lexer:lex
;           ftt-cc.ast:tok-kind->op-kind
;           ftt-cc.ast:create-ast-identifier
;           ftt-cc.ast:create-ast-integer-literal
;           ftt-cc.ast:create-ast-binary-operator
;           ftt-cc.ast:create-ast-return-stmt
;           ftt-cc.ast:create-ast-compound-stmt
;           ftt-cc.ast:dump-ast))

;;; ---------- Parser

; this is the temporary counter!
(defparameter *count* 0)

;; operation precedence
(defparameter +op-prec-unknown+ 0)  ; not binary operator, e.g) numeric constant, identifier
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
  (setf *cur-tok* (consume-token))
  (setf *count* 0))

(defun consume-token ()
  ;; just lex the global token instance at now.
  (format t "CONSUME TOKEN~%")
  (setf *cur-tok* (lex)))

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
; NOTE:
;  - left-associative
(defun parse-translation-unit ()
  (parse-external-declaration))

; (6.9) external-declaration:
;   function-definition
;   declaration
; NOTE:
;  - declaration
;    - extern int a = 3;
;  - function-definition
;    - void foo() {}
;  - first, check a keyword of a token
(defun parse-external-declaration ()
  ; function-definition and declaration are started with decl-spec
  (let (declspec (parse-declaration-specifiers))
    ))

; (6.9.1) function-definition:
;   declaration-specifiers declarator declaration-list[opt] compound-statement
; (6.9.1) declaration-list:
;   declaration
;   declaration-list declaration
; e.g)
;  int main() {}
;  - int: declaration-specifiers <- type-specifier <- 'int'
;  - main: declarator <- direct-declarator <- identifier <- 'main'
;  - {} : compound-statement
(defun parse-function-definition ()
  )

; (6.7) declaration:
;   declaration-specifiers init-declarator-list[opt] ;
; (6.7) init-declarator-list:
;   init-declarator
;   init-declarator-list , init-declarator
; (6.7) init-declarator:
;   declarator
;   declarator = initializer
(defun parse-declaration ())

; (6.7.5) declarator:
;   pointeropt direct-declarator
(defun parse-declarator ())


; (6.7) declaration-specifiers:
;   storage-class-specifier declaration-specifiers[opt]
;   type-specifier declaration-specifiers[opt]
;   type-qualifier declaration-specifiers[opt]
;   function-specifier declaration-specifiers[opt]
(defstruct declspec
  )

; return declspec (declaration-specifier)
(defun parse-declaration-specifiers ()
  (let ((kind (tok-kind *cur-tok*)))
    (case kind
      (:kw-void (make-declspec))
      (:kw-int (make-declspec))
      (otherwise (format t "unkonwn decl spec: ~a~%" kind)))))


(defun parse-expression ()
  (format t "parse-expression~%")

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
       
        (format t "check point---------------~%")
        (format t "LHS=======~%")
        (dump-ast lhs)
        (format t "RHS=======~%")
        (dump-ast rhs)
        (terpri)
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
  (format t "parse-statement~%")
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
  (format t "pasre-return-statement~%")
  (consume-token) ; consume 'return'
  (let ((ret-expr (parse-expression)))
    (format t "IN RETURN : ~a~%" ret-expr)
    (dump-ast ret-expr)
    (consume-token)  ;; temp...? eat ';'
    (create-ast-return-stmt ret-expr)))

(defun parse-compound-statement ()
  (format t "parse-compound-statement~%")
  (let ((stmts '()))
    (consume-open :tok-l-brace)
    (loop
      until (ftt-cc.token:is? *cur-tok* :tok-r-brace) do
          (push (parse-declaration-or-statement) stmts))
    (format t "the number of stmts: ~d~%" (length stmts))
    (consume-close :tok-r-brace)
    (create-ast-compound-stmt stmts)))