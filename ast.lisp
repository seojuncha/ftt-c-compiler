;;; ---------- AST
(defparameter *op-kind*
  '(:op-unkonwn
    :op-equal
    :op-plus
    :op-minus))

(defun tok-kind->op-kind (tok-kind)
  (cond
    ((eq tok-kind :tok-equal) (return-from tok-kind->op-kind :op-equal))  ; equal? assign?
    ((eq tok-kind :tok-plus) (return-from tok-kind->op-kind :op-plus))
    ((eq tok-kind :tok-minus) (return-from tok-kind->op-kind :op-minus))
    (t (return-from tok-kind->op-kind :op-unkonwn))))

(defclass ast-binary-operator ()
  ((binary-operation-kind :initarg :opkind :initform :op-unkown :accessor op-kind)
   (lhs :initarg :lhs :initform nil :accessor lhs)
   (rhs :initarg :rhs :initform nil :accessor rhs)))

(defun create-ast-binary-operator (opkind lhs rhs)
  (make-instance 'ast-binary-operator :opkind opkind :lhs lhs :rhs rhs))

(defclass ast-integer-literal ()
  ((value :initarg :value :initform nil :accessor value)))

(defun create-ast-integer-literal (value)
  (make-instance 'ast-integer-literal :value value))

(defclass ast-identifier ()
  ((name :initarg :name :initform nil :accessor name)))

(defun create-ast-identifier (name)
  (make-instance 'ast-identifier :name name))

(defgeneric dump-ast (obj))
(defmethod dump-ast ((obj ast-binary-operator))
  (when obj
    (format t "AST: ~a~%" obj)
    (format t "  OP: ~a~%" (op-kind obj))
    (format t "  LHS: ~a~%" (lhs obj))
    (dump-ast (lhs obj))
    (format t "  RHS: ~a~%" (rhs obj))
    (dump-ast (rhs obj))))
(defmethod dump-ast ((obj ast-integer-literal))
  (when obj
    (format t "    AST: ~a~%" obj)
    (format t "      VALUE: ~a~%" (value obj))))
(defmethod dump-ast ((obj ast-identifier))
  (when obj
    (format t "    AST: ~a~%" obj)
    (format t "      NAME: ~a~%" (name obj))))

(defclass ast-return-stmt ()
  ((expr :initarg :expr :initform nil :accessor expr)))

(defun create-ast-return-stmt (expr)
  (make-instance 'ast-return-stmt :expr expr))
(defmethod dump-ast ((obj ast-return-stmt))
  (format t "AST: ~a~%" obj)
  (dump-ast (expr obj)))

(defclass ast-compound-stmt ()
  ((stmts :initarg :stmts :initform '() :accessor stmts)))

(defmethod create-ast-compound-stmt (stmts)
  (make-instance 'ast-compound-stmt :stmts stmts))
(defmethod dump-ast ((obj ast-compound-stmt))
  (format t "AST: ~a~%" obj)
  (dolist (stmt (stmts obj))
    (dump-ast stmt)))
