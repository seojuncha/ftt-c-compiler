;; source manager has source infos
;; 


(defstruct (source-manager)
  entries) ; source-info vector? or map?


(defstruct (source-info)
  id ; identification (how to create random?)
  filename ; string type
  content ; memory-buffer
  line
  pos)

; load the source content to the buffer
(defstruct (memory-buffer))


(defun open-file (manager path)
  ; todo: parsing path, now only use a file name
  (let* ((membuf (make-memory-buffer))
         (info (make-source-info)))
    (setf (source-manager-entries manager) info)))