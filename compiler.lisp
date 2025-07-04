(defparameter *tok-kind*
  '(:tok-identifier
    :tok-numeric-constant))

(defparameter *puctuator*
  '(:tok-plus
    :tok-minus))

(defparameter *plus-expr* "1+2")

(defun parse-expression (expr)
  (format t "EXPR: ~s~%" expr)
  (let ((lhs (parse-assignment-expression expr)))
    (format t "LHS: ~a~%" lhs)))

(defun parse-assignment-expression (expr))


(defun parse-rhs-of-binary-expression (lhs))


(format t "Start parsing~%")
(format t "result: ~a~%" (parse-expression *plus-expr*))