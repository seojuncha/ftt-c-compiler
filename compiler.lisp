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

(defparameter *plus-expr* "1+2")
;(defparameter *plus-expr* "12+2")
;(defparameter *plus-expr* "12+23")

(defclass token ()
  ((kind
    :initarg :kind
    :initform :tok-unknown)
   (lexeme
    :initarg :lexeme
    :initform "")))

(defgeneric dump-token (obj))
(defmethod dump-token ((obj token)))

;;; managing tokens
;;; current token to be lexed
(defparameter *cur-tok* (make-instance 'token))
;;; lookahead token symbol
(defparameter *lookahead-tok* nil)
;; points to the current token index
(defparameter *buf-ptr* nil)

(defun create-token (lexeme tok-kind)
  (make-instance 'token :lexeme lexeme :kind tok-kind))

;;; ----------  character utilities
;; read one character by using *buf-ptr*
(defun read-one-char ()
  (char *plus-expr* *buf-ptr*)
  (incf *buf-ptr*))

(defun read-numeric-char ()
  (read-one-char))

;;; ---------- lexing functions
(defun init-lexer ()
  (format t "initialize the lexer~%")
  (setq *buf-ptr* 0))

;; return a token?
(defun lex ()
  ; read a characeter
  ; update the buffer pointer
  (create-token "" :tok-eof))

(defun next-token ())

(defun lookahead-token (size))

;;; ---------- parsing functions
(defun parse-expression ()
  (format t "TEST EXPR: ~s~%" *plus-expr*)
  (let ((lhs (parse-assignment-expression)))
    (format t "LHS: ~s~%" lhs)
    (parse-rhs-of-binary-expression lhs)))

(defun parse-assignment-expression ()
  (let ((tok (next-token)))
    (format t "tok: ~s~%" tok)
    (setq *cur-tok* tok)))


(defun parse-rhs-of-binary-expression (lhs))

(defun parse-numeric-constant (str))


;;; ---- compiler
(format t "Start parsing~%")
(init-lexer)
(format t "result: ~a~%" (parse-expression))