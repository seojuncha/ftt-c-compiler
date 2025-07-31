(in-package :ftt-cc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl)
  (use-package :ftt-cc.token)
  (use-package :ftt-cc.lexer)
  (use-package :ftt-cc.parser)
  (use-package :ftt-cc.ast)
  (use-package :ftt-cc.codegen-arm))

(defparameter *test-code* "int main(void) { return 3+5;}")

(defun start ())
; (format t "start parsing: ~s~%~%" *test-code*)
; (init-lexer)
; (init-parser)
; (dump-ast (parse-ast)))

(defun version ()
  (list 0 0 1))