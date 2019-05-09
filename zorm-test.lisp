(in-package #:cl-user)

(defpackage #:zorm-test
  (:use #:closer-common-lisp #:zorm #:fiveam ))

(in-package #:zorm-test)

(def-suite all-tests)

;;; dao-class-tests

(def-suite dao-class-tests :in all-tests)

(defclass c1 ()
  ())

(defclass dc1 ()
  ((id :column t))
  (:metaclass dao-class)
  (:primary-key id))

(zorm::ensure-class-is-finalized 'dc1)

(defclass dc2 (c1 dc1)
  ()
  (:metaclass dao-class))

(zorm::ensure-class-is-finalized 'dc2)

(def-test test-superclasses (:suite dao-class-tests)
  (is (equal (list (find-class 'dao))
             (class-direct-superclasses (find-class 'dc1))))
  (is (equal (list (find-class 'c1) (find-class 'dc1))
             (class-direct-superclasses (find-class 'dc2)))))

(def-test test-primary-key (:suite dao-class-tests)
  (is (equal (primary-key (find-class 'dc1)) (list 'id)))
  (is (equal (primary-key (find-class 'dc2)) (list 'id))))

;;; database tests

(def-suite db-tests :in all-tests)

(defparameter *ddl-statements* nil)

(defun add-ddl-statement (statement)
  (pushnew statement *ddl-statements* :test #'string-equal))

(def-fixture transaction-fixture ()
  (postmodern:with-transaction (transaction)
    (&body)
    (postmodern:abort-transaction transaction)))

(defmacro def-transaction-test (name (&rest keywords) &body body)
  `(def-test ,name (,@keywords)
     (with-fixture transaction-fixture ()
       ,@body)))

(defclass employee ()
  ((employee-id :column t :reader employee-id)
   (name :column t :initarg :name :accessor employee-name)
   (full-time-p :column t :initarg :full-time-p :type boolean :accessor employee-full-time-p)
   (address :column t :initarg :address :accessor employee-address :default nil))
  (:metaclass dao-class)
  (:table-name employees)
  (:primary-key employee-id))

(add-ddl-statement "
CREATE TABLE employees (
  employee_id serial PRIMARY KEY,
  name text not null,
  full_time_p boolean,
  address text
)")

(defun insert-employee-raw (name full-time-p address)
  (postmodern:execute (format nil "INSERT INTO employees (name, full_time_p, address) VALUES (~A, ~A, ~A)"
                              (s-sql:sql-escape name)
                              (s-sql:sql-escape full-time-p)
                              (s-sql:sql-escape address))))

(def-test test-select-dao-empty-set (:suite db-tests)
  (is (null (select-dao 'employee))))

(def-transaction-test test-select-dao-non-empty-set (:suite db-tests)
  (insert-employee-raw "foo" t nil)
  (insert-employee-raw "bar" t "xyz")
  (insert-employee-raw "baz" nil :null)
  (let ((results (select-dao 'employee :order-by "employee_id"))
        (employee))
    (is (= 3 (length results)))

    (setf employee (pop results))
    (is (equal "foo" (employee-name employee)))
    (is-true (employee-full-time-p employee))

    (setf employee (pop results))
    (is (equal "bar" (employee-name employee)))
    (is-true (employee-full-time-p employee))
    (is (equal "xyz" (employee-address employee)))

    (setf employee (pop results))
    (is (equal "baz" (employee-name employee)))
    (is-false (employee-full-time-p employee))
    (is-false (employee-address employee))
    (is-true (db-null-p employee 'address))))

(def-transaction-test test-select-dao-columns (:suite db-tests)
  (flet ((dao-slot-boundp-fn (dao)
           (lambda (slot-name)
             (slot-boundp dao slot-name))))
    (insert-employee-raw "foo" t "abc")
    (let ((employee))
      (setf employee (first (select-dao 'employee :columns :default)))
      (is-true (every (dao-slot-boundp-fn employee) '(employee-id name full-time-p)))
      (is-false (slot-boundp employee 'address))

      (setf employee (first (select-dao 'employee :columns :all)))
      (is-true (every (dao-slot-boundp-fn employee) '(employee-id name full-time-p address)))

      (setf employee (first (select-dao 'employee :columns '(name))))
      (is-true (every (dao-slot-boundp-fn employee) '(employee-id name)))
      (is-true (notany (dao-slot-boundp-fn employee) '(full-time-p address))))))

(def-transaction-test test-null-value-write-to-db (:suite db-tests)
  (let ((employee (make-instance 'employee :name "foo")))
    (setf (db-null-p employee 'full-time-p) t)
    (insert-dao employee))
  (is (eql :null (postmodern:query "SELECT full_time_p FROM employees" :single))))

(def-transaction-test test-null-value-read-from-db (:suite db-tests)
  (insert-employee-raw "quux" :null :null)
  (let ((employee (first (select-dao 'employee))))
    (is-false (employee-full-time-p employee))
    (is-true (db-null-p employee 'full-time-p))))

(def-transaction-test test-insert-dao (:suite db-tests)
  (let* ((employee (insert-dao (make-instance 'employee :name "some name")))
         (select-result (select-dao 'employee)))
    (is (= 1 (length select-result)))
    (let ((result-employee (first select-result)))
      (is (= (employee-id result-employee) (employee-id employee)))
      (is (string= (employee-name result-employee) (employee-name employee))))))

(def-transaction-test test-delete-dao (:suite db-tests)
  (let ((employee (insert-dao (make-instance 'employee :name "some name"))))
    (delete-dao employee)
    (is (null (select-dao 'employee)))))

(def-transaction-test test-update-dao (:suite db-tests)
  (let ((employee (insert-dao (make-instance 'employee :name "some name"))))
    (setf (employee-name employee) "some other name")
    (update-dao employee)
    (let* ((select-result (select-dao 'employee))
           (result-employee (first select-result)))
      (is (= (employee-id result-employee) (employee-id employee)))
      (is (string= (employee-name result-employee) (employee-name employee))))))

(def-transaction-test test-refresh-dao (:suite db-tests)
  (let ((employee (insert-dao (make-instance 'employee :name "some name"))))
    (postmodern:execute (format nil
                                "UPDATE employees SET name = 'some other name' WHERE employee_id = ~A"
                                (employee-id employee)))
    (refresh-dao employee)
    (is (string= "some other name" (employee-name employee)))))

;;; utils

(defun symbol-value-if-exists (package-name symbol-name)
  (let* ((package (find-package package-name))
         (symbol (when package
                   (find-symbol symbol-name package))))
    (when (and symbol (boundp symbol))
      (symbol-value symbol))))

(defun run-tests (run-fn &rest args)
  (let ((db-name (or (symbol-value-if-exists "CL-USER" "*ZORM-TEST-DB-NAME*") "zorm_test_db"))
        (maintenance-db-name (or (symbol-value-if-exists "CL-USER" "*ZORM-TEST-MAINTENANCE-DB-NAME*")
                                 "postgres"))
        (username (or (symbol-value-if-exists "CL-USER" "*ZORM-TEST-USERNAME*") "zorm_test"))
        (password (or (symbol-value-if-exists "CL-USER" "*ZORM-TEST-PASSWORD*") "zorm_test"))
        (host (or (symbol-value-if-exists "CL-USER" "*ZORM-TEST-HOST*") "localhost")))
    (postmodern:with-connection (list maintenance-db-name username password host)
      (postmodern:execute (format nil "DROP DATABASE IF EXISTS ~A" db-name))
      (postmodern:execute (format nil "CREATE DATABASE ~A" db-name))
      (unwind-protect
           (postmodern:with-connection (list db-name username password host)
             (dolist (statement (reverse *ddl-statements*))
               (postmodern:execute statement))
             (apply run-fn args))
        (postmodern:execute (format nil "DROP DATABASE ~A" db-name))))))
