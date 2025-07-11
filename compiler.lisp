(defparameter *puctuator*
  '(:tok-plus
    :tok-minus))

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
(defparameter *test-expr* "5+2")
; (defparameter *test-expr* "5-2")

;; TODO
;(defparameter *test-expr* "12+2")
;(defparameter *test-expr* "12+23")

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
;; read one character, return the incremented pointer.
(defun consume-char (size)
  (format t "buffer pointer: [~d] ~s~%" *buf-ptr* (char *test-expr* *buf-ptr*))
  ; todo: use a buffer instead of *plus-expr* to store code
  (char *test-expr* *buf-ptr*))

;;; ---------- Lexer
(defun init-lexer ()
  (setq *buf-ptr* 0)
  (setq *buf-start* 0)
  (setq *buf-end* (length *test-expr*))) ; length, fixed now.

(defun form-token-with-chars (lexeme tok-kind)
  ; (format t "CREATE NEW TOKEN~%")
  (make-instance 'token :lexeme lexeme :kind tok-kind))

;; start to lex, return a token
(defun lex ()
  ;; todo: loop here! until finding a token lexeme not a single character.
  (when (< *buf-ptr* *buf-end*)
    (let ((c (consume-char 1))
          (tok-kind nil))
      ; (format t "   current buffer pointer: ~d~%" *buf-ptr*)
      (incf *buf-ptr*)
      (cond ((and (char> c #\0) (char< c #\9))
              (setq tok-kind :tok-numeric-constant))
            ((char= c #\+)
              (setq tok-kind :tok-plus))
            ((char= c #\-)
              (setq tok-kind :tok-minus))
            (t
              (format t "unkonwn: ~s~%" c)
              (setq tok-kind :tok-unkonwn)))
      (form-token-with-chars c tok-kind))))

;; lexing the continuous numeric characters.
(defun lex-numeric-constant ()
  (format t "lex-numeric-constant~%"))

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
    ; (format t "LHS 00: ")
    ; (dump-ast lhs)
    (parse-rhs-of-binary-expression lhs)))

(defun parse-cast-expression ()
  ; (format t "parse-cast-expression~%")
  (let ((saved-kind (tok-kind *cur-tok*)))
    (cond ((eq saved-kind :tok-numeric-constant)
            (consume-token) (create-ast-integer-literal))
          ((or (eq saved-kind :tok-plus) (eq saved-kind :tok-minus))
            (consume-token))
          (t
            (format t "unknown token: ~a~%" saved-kind)))))

(defun parse-assignment-expression ()
  ; (format t "parse-assignment-expression~%")
  (let ((lhs (parse-cast-expression)))
    ; (format t "LHS 11: ")
    ; (dump-ast lhs)
    (parse-rhs-of-binary-expression lhs)))

(defun parse-rhs-of-binary-expression (lhs)
  ; (format t "parse-rhs-of-binary-expression~%")
  (let ((optok nil)
        (rhs nil))
    ; (format t "LHS 22: ")
    ; (dump-ast lhs)
    ; temp: should calculate the precedence of operators.
    (when (= *buf-ptr* *buf-end*)
      (return-from parse-rhs-of-binary-expression lhs))
    ; will(and must) be an operator.
    (setf optok *cur-tok*)
    ; (dump-token optok)
    (consume-token)
    (setf rhs (parse-cast-expression))
    ; (format t "RHS: ")
    ; (dump-ast rhs)
    (create-ast-binary-operator (tok-kind->op-kind (tok-kind optok)) lhs rhs)))

;;; ---------- AST
(defparameter *op-kind* '(:op-unkonwn :op-plus :op-minus))

(defun tok-kind->op-kind (tok-kind)
  (cond
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
  ())

(defun create-ast-integer-literal ()
  (make-instance 'ast-integer-literal))

(defgeneric dump-ast (obj))
(defmethod dump-ast ((obj ast-binary-operator))
  (when obj
    (format t "AST: ~a~%" obj)
    (format t "  KIND: ~a~%" (op-kind obj))
    (format t "  LHS: ~a~%" (lhs obj))
    (format t "  RHS: ~a~%" (rhs obj))))

(defmethod dump-ast ((obj ast-integer-literal))
  (when obj
    (format t "AST: ~a~%" obj)))

;;; ---- compiler
(format t "start parsing: ~s~%~%" *test-expr*)
(init-lexer)
(init-parser)
(dump-ast (parse-ast))