(asdf:defsystem "zorm"
  :description "ORM for PostgreSQL"
  :version "0.1.0"
  :author "Chaitanya Gupta <mail@chaitanyagupta.com>"
  :license "BSD-3-Clause"
  :serial t
  :depends-on ("postmodern" "closer-mop")
  :components ((:file "zorm")))

(asdf:defsystem "zorm/test"
  :depends-on ("zorm" "fiveam")
  :perform (test-op (o s)
                    (uiop:symbol-call :zorm-test '#:run-tests
                                      (uiop:find-symbol* '#:run! :fiveam)
                                      (uiop:find-symbol* '#:all-tests :zorm-test)))
  :components ((:file "zorm-test")))
