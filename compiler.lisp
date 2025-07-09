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

(defparameter *plus-expr* "5+2")
;(defparameter *plus-expr* "12+2")
;(defparameter *plus-expr* "12+23")

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
(defmethod dump-token ((obj token)))

;;; managing tokens
;;; current token to be lexed
(defparameter *cur-tok* (make-instance 'token))
;;; lookahead token symbol
(defparameter *lookahead-tok* nil)
;; points to the current token index
(defparameter *buf-ptr* nil)

;;; ----------  character utilities
;; read one character by using *buf-ptr*
(defun get-char (size)
  (format t "buffer pointer: ~d~%" *buf-ptr*)
  (char *plus-expr* *buf-ptr*))

;;; ---------- lexing functions
(defun create-token (lexeme tok-kind)
  (make-instance 'token :lexeme lexeme :kind tok-kind))

(defun init-lexer ()
  (format t "initialize the lexer~%")
  (setq *buf-ptr* 0))

;; return a token
(defun lex ()
  (let ((c (get-char 1))
        (tok-kind nil))
    (incf *buf-ptr*)
    (format t "CHAR: ~s~%" c)
    ; (format t "type: ~a~%" (type-of c))
    (cond 
      ((and (char> c #\0) (char< c #\9))
       (format t "NUMERIC~%")
       (setq tok-kind :tok-numeric-constant))
      ((char= c #\+)
       (format t "PLUS~%")
       (setq tok-kind :tok-plus))
      (t 
       (format t "unkonwn~%")
       (setq tok-kind :tok-unkonwn)))
    (create-token c tok-kind)))


;;; ---------- parsing functions
(defun next-token ())

(defun lookahead-token (size))

(defun parse-expression ()
  (format t "TEST EXPR: ~s~%" *plus-expr*)
  (let ((lhs (parse-assignment-expression)))
    (format t "lhs: ~a,~a~%" (tok-kind lhs) (tok-lexeme lhs))
    (parse-rhs-of-binary-expression lhs)))

(defun parse-assignment-expression ()
  (let ((tok (lex)))
    (format t "tok: ~a,~a~%" (tok-kind tok) (tok-lexeme tok))
    (setf *cur-tok* tok)))

(defun parse-rhs-of-binary-expression (lhs))

(defun parse-numeric-constant (str))

;;; ---- compiler
(format t "Start parsing~%")
(init-lexer)
(format t "result: ~a~%" (parse-expression))