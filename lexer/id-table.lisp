(defstruct (id-table
            (:copier nil)) ; not allowed copy
  (hashmap (make-hash-table :test 'equal)))

; todo: may be more simplify.
(defun init (table)
  (setf (gethash "return" table) :kw-return)
  (setf (gethash "void" table) :kw-void)
  (setf (gethash "int" table) :kw-int))

; now just return either a token kind or a keyword kind
(defun lookup (table name)
  (gethash name (table-hashmap table)))
