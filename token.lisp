(in-package :ftt-cc.token)

(defparameter *puctuator*
              '(:tok-plus
                :tok-minus ; -
                :tok-star ; *
                :tok-equal ; =
                :tok-l-paren ; (
                :tok-r-paren ; )
                :tok-l-brace ; {
                :tok-r-brace ; }
                :tok-comma ; ,
                :tok-semi))

(defparameter *keyword*
              '(:kw-return
                :kw-void
                :kw-int))

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
                *puctuator*
                *keyword*))

(defclass token ()
    ((kind :initarg :kind :initform :tok-unknown :accessor tok-kind)
     (lexeme :initarg :lexeme :initform "" :accessor tok-lexeme)))

(defgeneric dump-token (obj))
(defgeneric is? (obj token-kind))

(defmethod dump-token ((obj token))
  (format t "TOK: ~a~%" obj)
  (format t "  KIND: ~a~%" (tok-kind obj))
  (format t "  LEXEME: ~s~%" (tok-lexeme obj)))
(defmethod is? ((obj token) token-kind)
  (eq (tok-kind obj) token-kind))
