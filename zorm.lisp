(defpackage #:zorm
  (:use #:cl-postgres #:closer-common-lisp #:s-sql)
  (:import-from #:postmodern #:*database*)
  (:export #:dao-class
           #:dao
           #:primary-key
           #:dao-exists-p
           #:query-dao
           #:do-query-dao
           #:select-dao
           #:get-dao
           #:do-select-dao
           #:*ignore-unknown-columns*
           #:insert-dao
           #:update-dao
           #:refresh-dao
           #:save-dao
           #:delete-dao
           #:dao-table-name
           #:with-column-writers
           #:db-null-p
           #:dao-dirty-slot-names))

(in-package #:zorm)

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun find-class-if-symbol (class)
  (if (symbolp class)
      (find-class class)
      class))

(defun ensure-class-is-finalized (class)
  (let ((class (find-class-if-symbol class)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))))

(defclass dao-class (standard-class)
  ((direct-primary-key :initarg :primary-key :initform nil :reader direct-primary-key)
   (effective-primary-key :initform nil :reader effective-primary-key)
   (table-name)
   (column-map :reader dao-column-map))
  (:documentation "Metaclass for database-access-object classes."))

(defmethod validate-superclass ((class dao-class) (super-class standard-class))
  t)

(defmethod class-direct-superclasses ((class dao-class))
  (let* ((standard-object-class (find-class 'standard-object))
         (direct-superclasses (call-next-method))
         (dao-root-class (find-class 'dao nil)))
    (if (and dao-root-class
             (not (eql class dao-root-class))
             (null (remove standard-object-class direct-superclasses)))
        (list dao-root-class)
        direct-superclasses)))

(defmethod compute-class-precedence-list ((class dao-class))
  (let ((precedence-list (call-next-method))
        (dao-root-class (find-class 'dao nil)))
    (if (and dao-root-class
             (not (eql class dao-root-class))
             (not (find dao-root-class precedence-list)))
        (error "DAO not in the class precedent list for: ~A" class)
        precedence-list)))

(defclass dao-slot (standard-slot-definition)
  ((columnp :accessor slot-column-p :initarg :column :initform nil)
   (sql-name :accessor slot-sql-name)
   (default-column-p :initarg :default :initform t :accessor slot-default-column-p)))

(defclass direct-dao-slot (standard-direct-slot-definition dao-slot)
  ()
  (:documentation "Type of slots that refer to database columns."))

(defmethod shared-initialize :after ((slot direct-dao-slot) slot-names
                                     &key (col-name nil col-name-p) &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-sql-name slot) (to-sql-name
                              (if col-name-p
                                  col-name
                                  (slot-definition-name slot))
                              s-sql:*escape-sql-names-p* t)))

(defmethod direct-slot-definition-class ((class dao-class) &key &allow-other-keys)
  "Slots that have a :column option are column-slots."
  (find-class 'direct-dao-slot))

(defclass effective-dao-slot (standard-effective-slot-definition dao-slot)
  ())

(defmethod effective-slot-definition-class ((class dao-class) &key &allow-other-keys)
  (find-class 'effective-dao-slot))

(defmethod compute-effective-slot-definition ((class dao-class) name direct-slot-definitions)
  (declare (ignore name))
  (let* ((direct-slot (first direct-slot-definitions))
         (effective-slot (call-next-method)))
    (when (slot-column-p direct-slot)
      (setf (slot-column-p effective-slot) t
            (slot-sql-name effective-slot) (slot-sql-name direct-slot)
            (slot-default-column-p effective-slot) (slot-default-column-p direct-slot)))
    effective-slot))

(defgeneric primary-key (class)
  (:documentation "Returns list of slot names that are the primary key of DAO
class. This is likely interesting if you have primary keys which are composed
of more than one slot. Pay careful attention to situations where the primary
key not only has more than one column, but they are actually in a different
order than they are in the database table itself. You can check this with the
find-primary-key-info function."))

(defmethod primary-key ((class-name symbol))
  (primary-key (find-class class-name)))

(defmethod primary-key ((class dao-class))
  (ensure-class-is-finalized class)
  (effective-primary-key class))

(defclass dao ()
  ((db-null-slot-names :initform nil)
   (dirty-slot-names :initform nil :accessor dao-dirty-slot-names))
  (:metaclass dao-class))

(defmethod primary-key ((dao dao))
  (mapcar #'(lambda (slot-name)
              (slot-value dao slot-name))
          (primary-key (class-of dao))))

(defun dao-table-name (class)
  (when (symbolp class)
    (setf class (find-class class)))
  (if (slot-boundp class 'table-name)
      (slot-value class 'table-name)
      (class-name class)))

(defmethod shared-initialize :before ((class dao-class) slot-names
                                      &key table-name &allow-other-keys)
  (declare (ignore slot-names))
  (if table-name
      (setf (slot-value class 'table-name)
            (if (symbolp (car table-name)) (car table-name) (intern (car table-name))))
      (slot-makunbound class 'table-name)))

(defun dao-superclasses (class)
  "Build a list of superclasses of a given class that are DAO
  classes."
  (let ((found ()))
    (labels ((explore (class)
               (when (typep class 'dao-class)
                 (pushnew class found))
               (mapc #'explore (class-direct-superclasses class))))
      (explore class)
      found)))

(defvar *class-finalize-lock* (bt:make-lock))

(defmethod finalize-inheritance :around ((class dao-class))
  #+zorm-thread-safe
  (let ((previously-finalized-p (class-finalized-p class)))
    (bordeaux-threads:with-lock-held (*class-finalize-lock*)
      (unless (and (class-finalized-p class)
                   (not previously-finalized-p))
        (call-next-method))))
  #-zorm-thread-safe
  (call-next-method))

(defmethod finalize-inheritance :after ((class dao-class))
  "Building a row reader and a set of methods can only be done after
  inheritance has been finalised."
  ;; The effective set of keys of a class is the union of its keys and
  ;; the keys of all its superclasses.
  (setf (slot-value class 'effective-primary-key)
        (or (direct-primary-key class)
            (some #'primary-key (rest (dao-superclasses class)))))
  (unless (every (lambda (x) (member x (dao-column-slot-names class))) (primary-key class))
    (error "Class ~A has a key that is not also a slot." (class-name class)))
  (setf (slot-value class 'column-map)
        (mapcar (lambda (s) (cons (slot-sql-name s) (slot-definition-name s))) (dao-column-slots class))))

(defun dao-column-slots (class)
  "Enumerate the slots in a class that refer to table rows."
  (remove-if-not #'slot-column-p (class-slots class)))

(defun dao-column-slot-names (class)
  (mapcar 'slot-definition-name (dao-column-slots class)))

(defun dao-column-default-slots (class)
  (remove-if-not #'slot-default-column-p (dao-column-slots class)))

(defun dao-column-sql-name (class slot-name)
  (make-symbol (car (find slot-name (slot-value class 'column-map) :key #'cdr :test #'eql))))

(defmethod (setf slot-value-using-class) :after (new-value (class dao-class) dao (slot effective-dao-slot))
  (when (slot-column-p slot)
    (pushnew (slot-definition-name slot) (dao-dirty-slot-names dao))
    (with-slots (db-null-slot-names)
        dao
      (let ((slot-name (slot-definition-name slot)))
        (cond
          (new-value
           (setf db-null-slot-names (remove slot-name db-null-slot-names)))
          ((not (eql (slot-definition-type slot) 'boolean))
           (pushnew slot-name db-null-slot-names))
          (t nil))))))

(defun db-null-p (dao slot-name)
  (and (null (slot-value dao slot-name))
       (find slot-name (slot-value dao 'db-null-slot-names))
       t))

(defun (setf db-null-p) (flag dao slot-name)
  (if flag
      (progn
        (pushnew slot-name (slot-value dao 'db-null-slot-names))
        (setf (slot-value dao slot-name) nil))
      (error "To set the slot column as not-null, provide a slot-value"))
  flag)

(defmethod slot-unbound ((class dao-class) dao slot-name)
  (let ((slot (find slot-name (class-slots class) :key #'slot-definition-name)))
    (if (and (every (lambda (slot-name)
                      (slot-boundp dao slot-name))
                    (primary-key class))
             (slot-column-p slot))
        (let* ((sql-name (dao-column-sql-name class slot-name))
               (sql (sql-compile
                     `(:select ,sql-name :from ,(dao-table-name class)
                               :where (:and ,@(loop :for slot-name in (primary-key class)
                                                 :collect (list :=
                                                                (dao-column-sql-name class slot-name)
                                                                (slot-value dao slot-name)))))))
               (value (postmodern:query sql :single)))
          (if (eql :null value)
              (progn
                (setf (db-null-p dao slot-name) t)
                nil)
              (setf (slot-value dao slot-name) value)))
        (call-next-method))))

(defparameter *ignore-unknown-columns* t)

(defun update-dao-from-fields (dao query-fields result-next-field-generator-fn)
  (let* ((class (class-of dao))
         (column-map (dao-column-map class)))
    (loop
       :for field :across query-fields
       :for writer := (cdr (assoc (field-name field) column-map :test #'string=))
       :do (etypecase writer
             (null (if *ignore-unknown-columns*
                       (funcall result-next-field-generator-fn field)
                       (error "No slot named ~a in class ~a. DAO out of sync with table, or incorrect query used."
                              (field-name field) (class-name class))))
             (symbol (let ((value (funcall result-next-field-generator-fn field)))
                       (if (eql :null value)
                           (setf (db-null-p dao writer) t)
                           (setf (slot-value dao writer) value))))
             (function (funcall writer dao (funcall result-next-field-generator-fn field)))))))

(defun dao-from-fields (class query-fields result-next-field-generator-fn)
  (let ((instance (allocate-instance class)))
    (setf (slot-value instance 'db-null-slot-names) nil)
    (setf (slot-value instance 'dirty-slot-names) nil)
    (update-dao-from-fields instance query-fields result-next-field-generator-fn)
    (setf (slot-value instance 'dirty-slot-names) nil)
    (initialize-instance instance)
    instance))

(defun dao-row-reader (class)
  "Defines a row-reader for objects of a given class."
  (row-reader (query-fields)
    (loop :while (next-row)
       :collect (dao-from-fields class query-fields #'next-field))))

(defun dao-update-reader (dao)
  (row-reader (fields)
    (unless (next-row)
      (error "No row returned in response"))
    (update-dao-from-fields dao fields #'next-field)
    (when (next-row)
      (error "More than one row returned in response"))))

(defun save-dao (dao)
  "Try to insert the content of a DAO. If this leads to a unique key
violation, update it instead."
  (let ((class (class-of dao)))
    (if (every (lambda (slot-name)
                 (slot-boundp dao slot-name))
               (primary-key class))
        (update-dao dao)
        (insert-dao dao))))

(defgeneric query-dao (class query &key reader))

(defmethod query-dao ((class dao-class) query &key reader)
  (ensure-class-is-finalized class)
  (let ((reader (or reader #'dao-row-reader)))
    (exec-query *database* query (funcall reader class))))

(defmethod query-dao ((class-name symbol) query &rest keys)
  (apply #'query-dao (find-class class-name) query keys))

(defun dao-function-row-reader (fn)
  (lambda (class)
    (row-reader (query-fields)
      (loop :while (next-row)
         :do (funcall fn (dao-from-fields class query-fields #'next-field))))))

(defmacro do-query-dao ((ivar class query) &body body)
  `(query-dao ,class ,query
              :reader (dao-function-row-reader (lambda (,ivar)
                                                 ,@body))))

(defun generate-select-query (class &key (columns :default) where order-by limit offset)
  (let ((class (find-class-if-symbol class)))
    (with-output-to-string (out)
      (write-string "SELECT" out)
      (format out " ~{~A~^, ~}"
              (mapcar (lambda (column-name)
                        (s-sql:to-sql-name (dao-column-sql-name class column-name)))
                      (union (or (when (listp columns)
                                   columns)
                                 (mapcar #'slot-definition-name
                                         (ecase columns
                                           (:all (dao-column-slots class))
                                           (:default (dao-column-default-slots class))
                                           (t (error "Unknown value for COLUMNS")))))
                             (primary-key class))))
      (format out " FROM ~A" (s-sql:to-sql-name (dao-table-name class)))
      (when where
        (format out " WHERE ~A" where))
      (when order-by
        (format out " ORDER BY ~A" order-by))
      (when limit
        (format out " LIMIT ~A" limit))
      (when offset
        (format out " OFFSET ~A" offset)))))

(defgeneric select-dao (class &key columns where order-by limit offset reader))

(defmethod select-dao ((class-name symbol) &rest keywords)
  (apply #'select-dao (find-class class-name) keywords))

(defmethod select-dao ((class dao-class) &key (columns :default) where order-by limit offset reader)
  (ensure-class-is-finalized class)
  (let ((query (generate-select-query class
                                      :columns columns
                                      :where where
                                      :order-by order-by
                                      :limit limit
                                      :offset offset)))
    (query-dao class query :reader reader)))

(defgeneric dao-test-sql-string (thing &key &allow-other-keys))

(defmethod dao-test-sql-string ((class dao-class) &key pk-values &allow-other-keys)
  (with-output-to-string (out)
    (loop
       :for remaining-keys :on (primary-key class)
       :for key := (first remaining-keys)
       :for value :in pk-values
       :do (format out "~A = ~A"
                   (s-sql:to-sql-name (dao-column-sql-name class key))
                   (s-sql:sql-escape value))
       :when (rest remaining-keys)
       :do (write-string " AND " out))))

(defmethod dao-test-sql-string ((dao dao) &key &allow-other-keys)
  (when (every (lambda (slot-name)
                 (slot-boundp dao slot-name))
               (primary-key (class-of dao)))
    (dao-test-sql-string (class-of dao) :pk-values (primary-key dao))))

(defun dao-exists-p (dao)
  (let* ((class (class-of dao))
         (where-sql (dao-test-sql-string dao))
         (exists-query (when where-sql
                         (format nil "SELECT EXISTS (SELECT true FROM ~A WHERE ~A)"
                                 (s-sql:to-sql-name (dao-table-name class))
                                 where-sql))))
    (when exists-query
      (postmodern:query exists-query :single))))

(defgeneric get-dao (class pk-values &key columns))

(defmethod get-dao ((class-name symbol) pk-values &rest keywords)
  (apply #'get-dao (find-class class-name) pk-values keywords))

(defmethod get-dao ((class dao-class) pk-values &key (columns :default))
  (ensure-class-is-finalized class)
  (let ((class (find-class-if-symbol class))
        (pk-values (ensure-list pk-values)))
    (assert (= (length (primary-key class)) (length pk-values)))
    (let* ((where (dao-test-sql-string class :pk-values pk-values)))
      (first (select-dao class :columns columns :where where)))))

(defgeneric insert-dao (dao)
  (:documentation "Insert the given object into the database."))

(defmethod insert-dao ((dao dao))
  (let* ((class (class-of dao))
         (table-name (dao-table-name class))
         (column-slot-names (dao-column-slot-names class))
         bound
         unbound)
    (loop :for slot-name :in column-slot-names
       :do (if (slot-boundp dao slot-name)
               (push slot-name bound)
               (push slot-name unbound)))
    (flet ((column-sql-name (slot-name)
             (dao-column-sql-name class slot-name)))
      (let* ((values (mapcar (lambda (x)
                               (list (s-sql:to-sql-name (column-sql-name x))
                                     (if (db-null-p dao x)
                                         :null
                                         (s-sql:sql-escape (slot-value dao x)))))
                             bound))
             (query (with-output-to-string (out)
                      (format out "INSERT INTO ~A (~{~A~^, ~}) VALUES (~{~A~^, ~})"
                              (s-sql:to-sql-name table-name)
                              (mapcar #'first values)
                              (mapcar #'second values))
                      (when unbound
                        (format out " RETURNING ~{~A~^, ~}" (mapcar #'column-sql-name unbound))))))
        (exec-query *database* query (dao-update-reader dao))
        (setf (slot-value dao 'dirty-slot-names) nil))))
  dao)

(defgeneric update-dao (dao)
  (:documentation "Update the object's representation in the database
  with the values in the given instance."))

(defmethod update-dao ((dao dao))
  (let* ((class (class-of dao))
         (column-slot-names (remove-if-not (lambda (slot-name)
                                             (member slot-name (dao-dirty-slot-names dao)))
                                           (dao-column-slot-names class))))
    (when column-slot-names
      (let* ((values (mapcan (lambda (slot-name)
                               (list (s-sql:to-sql-name (dao-column-sql-name class slot-name))
                                     (if (db-null-p dao slot-name)
                                         :null
                                         (s-sql:sql-escape (slot-value dao slot-name)))))
                             column-slot-names))
             (query (format nil "UPDATE ~A SET ~{~A = ~A~^, ~} WHERE ~A"
                            (s-sql:to-sql-name (dao-table-name class))
                            values
                            (dao-test-sql-string dao))))
        (exec-query *database* query)
        (setf (dao-dirty-slot-names dao) nil)))))

(defgeneric refresh-dao (dao &key discard-dirty columns))

(defmethod refresh-dao ((dao dao) &key discard-dirty (columns :bound))
  (when (and (dao-dirty-slot-names dao) (not discard-dirty))
    (error "Cannot refresh dao when it has dirty slots: ~S" (dao-dirty-slot-names dao)))
  (let* ((class (class-of dao))
         (refresh-columns (if (eql columns :bound)
                              (remove-if-not (lambda (slot-name)
                                               (slot-boundp dao slot-name))
                                             (dao-column-slot-names class))
                              columns))
         (query (generate-select-query (class-of dao)
                                       :columns refresh-columns
                                       :where (dao-test-sql-string dao))))
    (mapcar (lambda (slot-name)
              (slot-makunbound dao slot-name))
            (dao-column-slot-names class))
    (exec-query *database* query (dao-update-reader dao))
    (setf (dao-dirty-slot-names dao) nil)
    dao))

(defgeneric delete-dao (dao)
  (:documentation "Delete the given dao from the database."))

(defmethod delete-dao ((dao dao))
  (let ((class (class-of dao)))
    (exec-query *database* (format nil "DELETE FROM ~A WHERE ~A"
                                   (s-sql:to-sql-name (dao-table-name class))
                                   (dao-test-sql-string dao)))))
