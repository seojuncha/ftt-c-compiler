(asdf:defsystem "ftt-cc"
  :license "MIT"
  :description "C Compiler"
  :serial t
  :components ((:file "package")
               (:file "token")
               (:file "lexer")
               (:file "parser")
               (:file "ast")
               (:file "main")))