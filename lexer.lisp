(in-package :ftt-cc.lexer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl)
  (use-package :ftt-cc.token))

;; fixme
(defparameter *test-code* "int main(void) { return 3+4;}")

(defparameter *id-table* (make-hash-table :test 'equal))

;; points to the current token index
(defparameter *buf-ptr* nil)
(defparameter *buf-start* nil)
(defparameter *buf-end* nil)

;;; ----------  character utilities
;; read one character with the incremented pointer.
(defun consume-char (ptr)
  ; todo: use a buffer instead of *test-expr* to store code
  (when (< ptr *buf-end*)
        (values (char *test-code* ptr) (incf ptr))))

; read just a chracter. not modify the pointer
(defun get-char (ptr)
  (when (< ptr *buf-end*)
        (char *test-code* ptr)))

(defun is-whitespace? (ptr)
  (char= (char *test-code* ptr) #\Space))

(defun init-lexer ()
  (setq *buf-ptr* 0)
  (setq *buf-start* 0)
  (setq *buf-end* (length *test-code*)) ; length, fixed now.

  ; init the identifier table
  (setf (gethash "return" *id-table*) :kw-return)
  (setf (gethash "void" *id-table*) :kw-void)
  (setf (gethash "int" *id-table*) :kw-int)
  (format t "init the identifier table: ")
  (format t "~a~%" *id-table*))

;; todo: Now, just keep keywords.
(defun lookup-identifier-info (identifier-name)
  (format t "lookup: ~s~%" identifier-name)
  (gethash identifier-name *id-table*))


(defun form-token-with-chars (tokend kind)
  (let ((toklen (- tokend *buf-ptr*))
        (lexeme nil)
        (result nil))
    ; (format t " TOKLEN: ~d~%" toklen)
    (setf lexeme (subseq *test-code* *buf-ptr* (+ *buf-ptr* toklen)))
    (setf *buf-ptr* tokend)
    ; (format t "buffer pointer after creating token: ~d~%" *buf-ptr*)
    (setf result
      (make-instance 'token
        :lexeme lexeme
        :kind kind))
    (format t "CREATE TOKEN [~a]: ~a | ~s~%" result (tok-kind result) (tok-lexeme result))
    result))

;; start to lex, return a token
(defun lex ()
  ; (format t "buffer pointer: ~d~%" *buf-ptr*)

  (when (= *buf-ptr* *buf-end*)
        ; (format t "END OF BUFFER~%")
        (return-from lex (form-token-with-chars *buf-ptr* :tok-unkonwn)))

  (when (< *buf-ptr* *buf-end*)
        (let ((cur-ptr nil)
              (c nil)
              (tok-kind nil))
          (setf cur-ptr *buf-ptr*)
          (loop
         while (is-whitespace? cur-ptr) do
           (incf cur-ptr)
           ; (format t "skip whitespace: ~d~%" cur-ptr)
         finally (setf *buf-ptr* cur-ptr))

          ; read the first character from the current pointer.
          (setf c (get-char cur-ptr))
          ; points to the next character.
          (incf cur-ptr)
          ; (format t "CURPTR: ~d~%" cur-ptr)
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
                ((char= c #\;)
                  (setq tok-kind :tok-semi))
                ((char= c #\{)
                  (setq tok-kind :tok-l-brace))
                ((char= c #\})
                  (setq tok-kind :tok-r-brace))
                ((char= c #\()
                  (setq tok-kind :tok-l-paran))
                ((char= c #\))
                  (setq tok-kind :tok-r-paran))
                (t
                  (format t "unkonwn: ~s~%" c)
                  (setq tok-kind :tok-unkonwn)))
          (form-token-with-chars cur-ptr tok-kind))))

;; [_A-Za-z0-9]*
(defun lex-identifier (curptr)
  (format t "lex-identifier~%")
  (let ((firstchar (get-char curptr)))
    (loop
   with ptr = curptr
   for ch = firstchar then (when (< ptr *buf-end*) (get-char ptr))
   while (and (< ptr *buf-end*) (or (alpha-char-p ch) (char= ch #\_))) do
     (multiple-value-bind (c p) (consume-char ptr)
       (setf ptr p)
       (setf curptr p))))
  ; create a token
  (let ((id-tok (form-token-with-chars curptr :tok-unkonwn)))
    (multiple-value-bind (kw-kind keyword?) (lookup-identifier-info (tok-lexeme id-tok))
      (format t "keyword?: ~a~%" kw-kind)
      (if keyword?
          (setf (tok-kind id-tok) kw-kind)
          (setf (tok-kind id-tok) :tok-identifier)))
    (format t "after: ~a~%" (tok-kind id-tok))
    (return-from lex-identifier id-tok)))

;; lexing the continuous numeric characters.
(defun lex-numeric-constant (curptr)
  ; (format t "lex-numeric-constant~%")
  (let ((firstchar (get-char curptr)))
    (loop
   with ptr = curptr
   for ch = firstchar then (when (< ptr *buf-end*) (get-char ptr))
   while (and (< ptr *buf-end*) (digit-char-p ch)) do
     (multiple-value-bind (c p) (consume-char ptr)
       (setf ptr p)
       (setf curptr p))))
  (form-token-with-chars curptr :tok-numeric-constant))