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

(defparameter *test-expr* "5+2")
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
(defgeneric is (obj token-kind))
(defmethod dump-token ((obj token))
  (format t "TOK: ~a~%" obj)
  (format t "  kind: ~a~%" (tok-kind obj))
  (format t "  lexeme: ~s~%" (tok-lexeme obj)))

(defmethod is ((obj token) token-kind)
  (eq (tok-kind obj) token-kind))

;;; managing tokens
;;; current token to be lexed
(defparameter *cur-tok* (make-instance 'token))

;;; lookahead token symbol
(defparameter *lookahead-tok* nil)

;; points to the current token index
(defparameter *buf-ptr* nil)

;;; ----------  character utilities
;; read one character by using *buf-ptr*
(defun consume-char (size)
  (format t "buffer pointer: ~d~%" *buf-ptr*)
  ; todo: use a buffer instead of *plus-expr* to store code
  (char *test-expr* *buf-ptr*))

;;; ---------- Lexer
(defun form-token-with-chars (lexeme tok-kind)
  (make-instance 'token :lexeme lexeme :kind tok-kind))

(defun init-lexer ()
  (setq *buf-ptr* 0))

;; start to lex, return a token
(defun lex ()
  ;; todo: loop here! until finding a token lexeme not a single character.
  (let ((c (consume-char 1))
        (tok-kind nil))
    (incf *buf-ptr*)
    (cond
      ((and (char> c #\0) (char< c #\9))
       (setq tok-kind :tok-numeric-constant)
       (lex-numeric-constant))
      ((char= c #\+)
       (setq tok-kind :tok-plus))
      (t
       (format t "unkonwn: ~s~%" c)
       (setq tok-kind :tok-unkonwn)))
    (form-token-with-chars c tok-kind)))

;; lexing the continuous numeric characters.
(defun lex-numeric-constant ()
  (format t "lex-numeric-constant~%"))

;;; ---------- Parser
(defun consume-token ()
  ;; just lex the global token instance at now.
  (lex))

(defun next-token ())

(defun lookahead-token (size))

(defun parse-ast ()
  (parse-expression))

(defun parse-expression ()
  (let ((lhs (parse-assignment-expression)))
    (dump-token lhs)
    (parse-rhs-of-binary-expression lhs)))

(defun parse-cast-expression())

(defun parse-assignment-expression ()
  (let ((tok (consume-token)))
    (dump-token tok)
    (setf *cur-tok* tok)))

(defun parse-rhs-of-binary-expression (lhs)
  (format t "parse rhs of binary expression~%")
  (let ((optok (consume-token))
        (rhs nil))
    ;(when (is optok :tok-plus) (format t "test, 'is' method~%"))
    (dump-token optok)
    (setf rhs (parse-cast-expression))
    ; todo: lhs and rhs should be an AST node also.
    (create-ast-binary-operator :op-plus lhs rhs)))

;;; ---------- AST
(defparameter *op-kind* '(:op-unkonwn :op-plus :op-minus))

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

(defgeneric dump-ast (obj))
(defmethod dump-ast ((obj ast-binary-operator))
  (format t "AST: ~a~%" obj)
  (format t "  kind: ~a~%" (op-kind obj))
  (format t "  lhs: ~a~%" (lhs obj))
  (format t "  rhs: ~a~%" (rhs obj)))

(defun create-ast-binary-operator (opkind lhs rhs)
  (make-instance 'ast-binary-operator :opkind opkind :lhs lhs :rhs rhs))

;;; ---- compiler
(format t "start parsing: ~s~%~%" *test-expr*)
(init-lexer)
(dump-ast (parse-ast))