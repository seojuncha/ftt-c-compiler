(defparameter *puctuator*
  '(:tok-plus
    :tok-minus
    :tok-equal))

(defparameter *tok-kind*
  (append
      ; identifier
      ; i.e) abc
    '(:tok-identifier

      ; numeric constant(integer, float),
      ; i.e) 123, 1.2
      :tok-numeric-constant

      ; string literal
      ; i.e) "abc"
      :tok-string-literal

      ; unkown token
      :tok-unknown

      ; end of file
      :tok-eof)
    *puctuator*))

;; Success
; (defparameter *test-expr* "5+2")
; (defparameter *test-expr* "5-2")
; (defparameter *test-expr* "51-21")
; (defparameter *test-expr* "232  - 5010")

;; working
(defparameter *test-expr* "a=5")

;; todo
; (defparameter *test-expr* "a=12+23")
; (defparameter *test-expr* "a=b+2")

;;; ----------  Token
(defclass token ()
  ((kind
    :initarg :kind
    :initform :tok-unknown
    :accessor tok-kind)
   (lexeme
    :initarg :lexeme
    :initform ""
    :accessor tok-lexeme)))

(defgeneric dump-token (obj))
(defgeneric is? (obj token-kind))
(defmethod dump-token ((obj token))
  (format t "TOK: ~a~%" obj)
  (format t "  KIND: ~a~%" (tok-kind obj))
  (format t "  LEXEME: ~s~%" (tok-lexeme obj)))
(defmethod is? ((obj token) token-kind)
  (eq (tok-kind obj) token-kind))

;;; managing tokens
;;; current token to be lexed
(defparameter *cur-tok* (make-instance 'token))

;;; lookahead token symbol
(defparameter *lookahead-tok* nil)

;; points to the current token index
(defparameter *buf-ptr* nil)
(defparameter *buf-start* nil)
(defparameter *buf-end* nil)

;;; ----------  character utilities
;; read one character with the incremented pointer.
(defun consume-char (ptr)
  ; todo: use a buffer instead of *test-expr* to store code
  (when (< ptr *buf-end*)
    (values (char *test-expr* ptr) (incf ptr))))

; read just a chracter. not modify the pointer
(defun get-char (ptr)
  (when (< ptr *buf-end*)
    (char *test-expr* ptr)))

(defun is-whitespace? (ptr)
  (char= (char *test-expr* ptr) #\Space))

;;; ---------- Lexer
(defun init-lexer ()
  (setq *buf-ptr* 0)
  (setq *buf-start* 0)
  (setq *buf-end* (length *test-expr*))) ; length, fixed now.

(defun form-token-with-chars (tokend kind)
  (let ((toklen (- tokend *buf-ptr*))
        (lexeme nil)
        (result nil))
    ; (format t " TOKLEN: ~d~%" toklen)
    (setf lexeme (subseq *test-expr* *buf-ptr* (+ *buf-ptr* toklen)))
    (setf *buf-ptr* tokend)
    ; (format t "buffer pointer after creating token: ~d~%" *buf-ptr*)
    (setf result
      (make-instance 'token
        :lexeme lexeme
        :kind kind))
    ; (format t "CREATE TOKEN [~a]: ~a | ~s~%" result (tok-kind result) (tok-lexeme result))
    result))

;; start to lex, return a token
(defun lex ()
  (when (< *buf-ptr* *buf-end*)
    (let ((cur-ptr nil)
          (c nil)
          (tok-kind nil))
      (setf cur-ptr *buf-ptr*)
      ; (format t "buffer pointer: ~d~%" *buf-ptr*)

      (loop
        while (is-whitespace? cur-ptr) do
          (incf cur-ptr)
          ; (format t "skip whitespace: ~d~%" cur-ptr)
        finally (setf *buf-ptr* cur-ptr))

      ; read the first character from the current pointer.
      (setf c (get-char cur-ptr))
      ; points to the next character.
      (incf cur-ptr)
      ; (format t "first character: ~s~%" c)
      (cond ((or (alpha-char-p c) (char= c #\_))
              (return-from lex (lex-identifier cur-ptr)))
            ((digit-char-p c)
              (return-from lex (lex-numeric-constant cur-ptr)))
            ((char= c #\+)
              (setq tok-kind :tok-plus))
            ((char= c #\-)
              (setq tok-kind :tok-minus))
            ((char= c #\=)
              (setq tok-kind :tok-equal))
            (t
              (format t "unkonwn: ~s~%" c)
              (setq tok-kind :tok-unkonwn)))
      (form-token-with-chars cur-ptr tok-kind))))

;; [_A-Za-z0-9]*
(defun lex-identifier (curptr)
  ; (format t "lex-identifier~%")
  (let ((firstchar (get-char curptr)))
    (loop
      with ptr = curptr
      for ch = firstchar then (when (< ptr *buf-end*) (get-char ptr))
      while (and (< ptr *buf-end*) (or (alpha-char-p ch) (char= ch #\_))) do
        ; (format t "before consume; p: ~d ch: ~s~%" ptr ch)
        (multiple-value-bind (c p) (consume-char ptr)
          ; (format t "after consume p: ~d, ch: ~s~%" p c)
          (setf ptr p)
          (setf curptr p))))
  (form-token-with-chars curptr :tok-identifier))

;; lexing the continuous numeric characters.
(defun lex-numeric-constant (curptr)
  ; (format t "lex-numeric-constant~%")
  (let ((firstchar (get-char curptr)))
    (loop
      with ptr = curptr
      for ch = firstchar then (when (< ptr *buf-end*) (get-char ptr))
      while (and (< ptr *buf-end*) (digit-char-p ch)) do
        ; (format t "before consume; p: ~d ch: ~s~%" ptr ch)
        (multiple-value-bind (c p) (consume-char ptr)
          ; (format t "after consume p: ~d, ch: ~s~%" p c)
          (setf ptr p)
          (setf curptr p))))
  (form-token-with-chars curptr :tok-numeric-constant))

;;; ---------- Parser
(defun init-parser ()
  (setf *cur-tok* (consume-token)))

(defun consume-token ()
  ;; just lex the global token instance at now.
  (setf *cur-tok* (lex)))

(defun next-token ())

(defun lookahead-token (size))

(defun parse-ast ()
  (parse-expression))

(defun parse-expression ()
  ; (format t "parse-expression~%")
  (let ((lhs (parse-assignment-expression)))
    ; (format t "LHS in expression: ")
    ; (dump-ast lhs)
    (parse-rhs-of-binary-expression lhs)))

;;; (6.5.16) assignment-expression:
;;;   conditional-expression
;;;   unary-expression assignment-operator assignment-expression
;;;
;;; The assignment-expression is also a binary operation.
;;; conditional-expression and uanry-expression is pared in parse-cast-expression
;;; so LHS is from parse-cast-expression,
(defun parse-assignment-expression ()
  ; (format t "parse-assignment-expression~%")
  (let ((lhs (parse-cast-expression)))
    ; (format t "LHS in assignment-expression: ")
    ; (dump-ast lhs)
    (parse-rhs-of-binary-expression lhs)))

(defun parse-cast-expression ()
  ; (format t "  !!! parse-cast-expression~%")
  (let ((saved-kind (tok-kind *cur-tok*))
        (result nil))
    (cond ((eq saved-kind :tok-identifier)
            ; (format t "1111111111~%")
            (setf result (create-ast-identifier (tok-lexeme *cur-tok*)))
            (consume-token)
            result)
          ((eq saved-kind :tok-numeric-constant)
            (setf result (create-ast-integer-literal (tok-lexeme *cur-tok*)))
            (consume-token)
            result)
          ((or (eq saved-kind :tok-plus) (eq saved-kind :tok-minus) (eq saved-kind :tok-equal))
            (consume-token)
            result)
          (t
            (format t "unknown token: ~a~%" saved-kind)))))

(defun parse-rhs-of-binary-expression (lhs)
  ; (format t "  >>> parse-rhs-of-binary-expression~%")
  (let ((optok nil)
        (rhs nil))
    ; (format t "LHS 22: ")
    ; (dump-ast lhs)

    ; temp: should calculate the precedence of operators.
    ; (format t " buffer pointer compare [~d : ~d]~%" *buf-ptr* *buf-end*)
    (when (= *buf-ptr* *buf-end*)
      (return-from parse-rhs-of-binary-expression lhs))

    ; will(and must) be an operator.
    (setf optok *cur-tok*)
    ; (dump-token optok)
    (consume-token)

    (let ((right-associative? (eq (tok-kind optok) :tok-equal)))
      (when right-associative?
        ; (format t "is right associative!! ~%")
        (setf rhs (parse-cast-expression))
        (return-from parse-rhs-of-binary-expression
          (create-ast-binary-operator
            (tok-kind->op-kind (tok-kind optok))
            lhs
            rhs))))

    ; (format t "RHS: ")
    ; (dump-ast rhs)
    (create-ast-binary-operator
      (tok-kind->op-kind (tok-kind optok))
      lhs
      rhs)))

;;; ---------- AST
(defparameter *op-kind*
  '(:op-unkonwn
    :op-equal
    :op-plus
    :op-minus))

(defun tok-kind->op-kind (tok-kind)
  (cond
    ((eq tok-kind :tok-equal) (return-from tok-kind->op-kind :op-equal))
    ((eq tok-kind :tok-plus) (return-from tok-kind->op-kind :op-plus))
    ((eq tok-kind :tok-minus) (return-from tok-kind->op-kind :op-minus))
    (t (return-from tok-kind->op-kind :op-unkonwn))))

(defclass ast-binary-operator ()
  ((binary-operation-kind
    :initarg :opkind
    :initform :op-unkown
    :accessor op-kind)
   (lhs
    :initarg :lhs
    :initform nil
    :accessor lhs)
   (rhs
    :initarg :rhs
    :initform nil
    :accessor rhs)))

(defun create-ast-binary-operator (opkind lhs rhs)
  (make-instance 'ast-binary-operator :opkind opkind :lhs lhs :rhs rhs))

(defclass ast-integer-literal ()
  ((value
   :initarg :value
   :initform nil
   :accessor value)))

(defun create-ast-integer-literal (value)
  (make-instance 'ast-integer-literal :value value))

(defclass ast-identifier ()
  ((name
    :initarg :name
    :initform nil
    :accessor name)))

(defun create-ast-identifier (name)
  (make-instance 'ast-identifier :name name))

(defgeneric dump-ast (obj))
(defmethod dump-ast ((obj ast-binary-operator))
  (when obj
    (format t "AST: ~a~%" obj)
    (format t "  OP: ~a~%" (op-kind obj))
    (format t "  LHS: ~a~%" (lhs obj))
    (dump-ast (lhs obj))
    (format t "  RHS: ~a~%" (rhs obj))
    (dump-ast (rhs obj))))
(defmethod dump-ast ((obj ast-integer-literal))
  (when obj
    (format t "    AST: ~a~%" obj)
    (format t "      VALUE: ~a~%" (value obj))))
(defmethod dump-ast ((obj ast-identifier))
  (when obj
    (format t "    AST: ~a~%" obj)
    (format t "      NAME: ~a~%" (name obj))))

;;; ---------- CodeGen
;;; AST -> IR(TAC) -> ARM assembly
;;; For example,
;;;  C:
;;;    2 + 3
;;;  TAC (3 address code):
;;;   t = 2 + 3
(defclass codegen ()
  ())

(defclass backend-arm ()
  ())

(defgeneric emit-add (obj op arg1 arg2 result))

(defmethod emit-add ((obj codegen) op arg1 arg2 result)
)

(defmethod emit-add ((obj backend-arm) op arg1 arg2 result)
)

;;; ---- compiler
(format t "start parsing: ~s~%~%" *test-expr*)
(init-lexer)
(init-parser)
(dump-ast (parse-ast))