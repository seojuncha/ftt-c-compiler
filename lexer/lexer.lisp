(defvar *debug-lexer* t)

(defstruct (lexer
            (:print-function
             (lambda (format t "print test state~%"))) ;test
            (:constructor make-null-lexer ()) ; test
            (:constructor make-new-lexer ())) ;test
  bufptr
  bufstart
  bufend)

; return (values new-state token)
(defun lex (lexer))

(defun lex-identifier ())

(defun lex-numeric-constant ())

(defun consume (ptr))

(defun getchar (ptr)
  (when (< ptr *buf-end*)
        (char *test-code* ptr)))

(defun whitespace-p (ptr)
  (char= (char *test-code* ptr) #\Space))

(defun form-token-with-chars (lexer tokend kind)
  (let* ((toklen (- tokend (lexer-bufend lexer)))
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