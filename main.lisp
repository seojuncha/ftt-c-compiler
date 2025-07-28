(in-package :ftt-cc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl)
  (use-package :ftt-cc.token)
  (use-package :ftt-cc.lexer)
  (use-package :ftt-cc.parser)
  (use-package :ftt-cc.ast)
  (use-package :ftt-cc.codegen-arm))

;; done
; (defparameter *test-expr* "5+2")
; (defparameter *test-expr* "5-2")
; (defparameter *test-expr* "51-21")
; (defparameter *test-expr* "232  - 5010")
; (defparameter *test-expr* "a=5")

; (defparameter *test-expr* "_a_c = 12 + 52")
; (defparameter *test-expr* "ac = 12 + 52")
; (defparameter *test-expr* "ac_ = 12 + 52")
; (defparameter *test-expr* "a=12+23")
; (defparameter *test-expr* "a=ba + 2")
; (defparameter *test-expr* "a=1+5")
; (defparameter *test-code* "return 32 + 15;")
; (defparameter *test-code* "{ return 3+5; }")  

;; working
(defparameter *test-code* "int main(void) { return 3+5;}")

;; todo

(defun start ()
  (format t "start parsing: ~s~%~%" *test-code*)
  (init-lexer)
  (init-parser)
  (dump-ast (parse-ast)))

(defun version ()
  (list 0 0 1))