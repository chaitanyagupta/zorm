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

(defclass dao-column-definition ()
  ((sql-name :initarg :sql-name
             :initform (error "SQL-NAME of column must be provided")
             :reader dao-column-definition-sql-name)
   (lazyp :initarg :lazy
          :initform t
          :reader dao-column-definition-lazyp)))

(defclass dao-reference-definition ()
  ((class :initarg :class :reader dao-reference-definition-class)
   (key :initarg :key :initform nil :reader %dao-reference-definition-key)))

(defmethod dao-reference-definition-key ((reference dao-reference-definition))
  (or (%dao-reference-definition-key reference)
      (primary-key (dao-reference-definition-class reference))))

(defclass dao-reverse-reference-definition ()
  ((class :initarg :class :reader dao-reverse-reference-definition-class)
   (key :initarg :key :initform nil :reader dao-reverse-reference-definition-key)
   (many :initarg :many :reader dao-reverse-reference-definition-many)))

(defclass dao-slot-mixin ()
  ((column :reader dao-slot-definition-column :initform nil)
   (reference :reader dao-slot-definition-reference :initform nil)
   (reverse-reference :reader dao-slot-definition-reverse-reference :initform nil)))

(defclass dao-direct-slot-definition (standard-direct-slot-definition dao-slot-mixin)
  ()
  (:documentation "Type of slots that refer to database columns."))

(defmethod shared-initialize :after ((slot dao-direct-slot-definition) slot-names
                                     &key
                                       column lazy (col-name nil col-name-p)
                                       references key reverse reverse-key (many t)
                                       &allow-other-keys)
  (declare (ignore slot-names))
  (when (and column references)
    (error "Both COLUMN and REFERENCES can't be given for a dao-slot"))
  (when column
    (let ((column-definition (make-instance 'dao-column-definition
                                            :sql-name (to-sql-name (if col-name-p
                                                                       col-name
                                                                       (slot-definition-name slot))
                                                                   *escape-sql-names-p* t)
                                            :lazy lazy)))
      (setf (slot-value slot 'column) column-definition)))
  (when references
    (when (and key (or reverse reverse-key))
      (error "KEY and (REVERSE or REVERSE-KEY) can't be given for a references slot"))
    (cond
      ((or reverse reverse-key)
       (let ((reverse-reference-definition (make-instance 'dao-reverse-reference-definition
                                                          :class references
                                                          :key (ensure-list reverse-key)
                                                          :many many)))
         (setf (slot-value slot 'reverse-reference) reverse-reference-definition)))
      (t
       (let ((reference-definition (make-instance 'dao-reference-definition
                                                  :class references
                                                  :key (ensure-list key))))
         (setf (slot-value slot 'reference) reference-definition))))))

(defmethod direct-slot-definition-class ((class dao-class) &key &allow-other-keys)
  "Slots that have a :column option are column-slots."
  (find-class 'dao-direct-slot-definition))

(defclass dao-effective-slot-definition (standard-effective-slot-definition dao-slot-mixin)
  ())

(defmethod effective-slot-definition-class ((class dao-class) &key &allow-other-keys)
  (find-class 'dao-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class dao-class) name direct-slot-definitions)
  (declare (ignore name))
  (let* ((direct-slot (first direct-slot-definitions))
         (effective-slot (call-next-method)))
    (setf (slot-value effective-slot 'column) (slot-value direct-slot 'column)
          (slot-value effective-slot 'reference) (slot-value direct-slot 'reference)
          (slot-value effective-slot 'reverse-reference) (slot-value direct-slot 'reverse-reference))
    effective-slot))

(defun find-dao-slot (x slot-name &optional errorp)
  (if (typep x 'dao-class)
      (or (find slot-name (class-slots x) :key #'slot-definition-name)
          (when errorp
            (error "SLOT named ~A missing for ~A" slot-name (class-of x))))
      (find-dao-slot (class-of x) slot-name errorp)))

(defun dao-column-slot-p (dao slot-name)
  (and (dao-slot-definition-column (find-dao-slot dao slot-name t)) t))

(defun dao-class-column-slots (class)
  "Enumerate the slots in a class that refer to table rows."
  (remove-if-not #'dao-slot-definition-column (class-slots class)))

(defun dao-column-slot-sql-name (dao slot-name)
  (dao-column-definition-sql-name
   (dao-slot-definition-column (find-dao-slot dao slot-name t))))

(defun dao-column-slot-lazyp (dao slot-name)
  (dao-column-definition-lazyp
   (dao-slot-definition-column (find-dao-slot dao slot-name t))))

(defun dao-reference-slot-p (dao slot-name)
  (and (dao-slot-definition-reference (find-dao-slot dao slot-name t)) t))

(defun dao-class-reference-slots (class)
  (remove-if-not #'dao-slot-definition-reference (class-slots class)))

(defun dao-reverse-reference-slot-p (dao slot-name)
  (and (dao-slot-definition-reverse-reference (find-dao-slot dao slot-name t)) t))

(defun dao-class-reverse-reference-slots (class)
  (remove-if-not #'dao-slot-definition-reverse-reference (class-slots class)))

(defun dao-class-column-slot-names (class)
  (mapcar 'slot-definition-name (dao-class-column-slots class)))

(defun dao-class-column-default-slots (class)
  (remove-if (lambda (slot)
               (dao-column-definition-lazyp (dao-slot-definition-column slot)))
             (dao-class-column-slots class)))

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

(defmethod finalize-inheritance ((class dao-class))
  (flet
      ((finalize-unsafe ()
         (call-next-method)

         ;; set the primary key
         (setf (slot-value class 'effective-primary-key)
               (or (direct-primary-key class)
                   (some #'primary-key (rest (dao-superclasses class)))))
         (unless (every (lambda (x) (member x (dao-class-column-slot-names class))) (primary-key class))
           (error "Class ~A has a key that is not also a slot." (class-name class)))

         ;; set up column name->slot mapping
         (setf (slot-value class 'column-map)
               (mapcar (lambda (s)
                         (cons (dao-column-definition-sql-name
                                (dao-slot-definition-column s))
                               (slot-definition-name s)))
                       (dao-class-column-slots class)))
         (values)))
    #+thread-support
    (let ((previously-finalized-p (class-finalized-p class)))
      ;; previously-finalized-p helps us distinguish between the case where we are
      ;; trying to re-finalize the class v/s race conditions
      (bt:with-recursive-lock-held (*class-finalize-lock*)
        (when (or (not (class-finalized-p class)) previously-finalized-p)
          (finalize-unsafe))))
    #-thread-support
    (finalize-unsafe)))

(defvar *reference-slot-under-update* nil)
(defvar *setting-unbound-reference-slot* nil)

(defmethod (setf slot-value-using-class) :after (new-value (class dao-class) dao (slot dao-effective-slot-definition))
  ;; Column slots
  (when (dao-slot-definition-column slot)
    ;; Mark slot as dirty
    (pushnew (slot-definition-name slot) (dao-dirty-slot-names dao))
    ;; Handle NIL
    (with-slots (db-null-slot-names)
        dao
      (let ((slot-name (slot-definition-name slot)))
        (cond
          (new-value
           (setf db-null-slot-names (remove slot-name db-null-slot-names)))
          ((not (eql (slot-definition-type slot) 'boolean))
           (pushnew slot-name db-null-slot-names))
          (t nil))))
    ;; If this slot is a key in a reference slot, make the latter slot
    ;; unbound. So that the next it is fetched, we get the updated value from
    ;; the database.
    (loop
       :for reference-slot :in (dao-class-reference-slots class)
       :for reference = (dao-slot-definition-reference reference-slot)
       :when (and (member (slot-definition-name slot) (dao-reference-definition-key reference))
                  (not (eql reference-slot *reference-slot-under-update*)))
       :do (slot-makunbound dao (slot-definition-name reference-slot))))
  ;; Reference slots
  (let ((reference (dao-slot-definition-reference slot)))
    (when (and reference (not *setting-unbound-reference-slot*))
      (let* ((referenced-class (dao-reference-definition-class reference))
             (key (dao-reference-definition-key reference))
             (*reference-slot-under-update* slot))
        (loop
           :for referenced-pk-slot-name :in (primary-key referenced-class)
           :for referenced-pk-value :in (primary-key new-value)
           :for key-slot-name :in key
           :do (setf (slot-value dao key-slot-name) referenced-pk-value))))))

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
  (let ((slot (find-dao-slot dao slot-name t)))
    (when (every (lambda (slot-name)
                   (slot-boundp dao slot-name))
                 (primary-key class))
      (cond
        ((dao-slot-definition-column slot)
         (let* ((column-name (dao-column-slot-sql-name class slot-name))
                (sql (format nil "SELECT ~A FROM ~A WHERE ~A"
                             column-name
                             (to-sql-name (dao-table-name class))
                             (dao-test-sql-string dao)))
                (value (postmodern:query sql :single)))
           (if (eql :null value)
               (progn
                 (setf (db-null-p dao slot-name) t)
                 nil)
               (setf (slot-value dao slot-name) value))))
        ((dao-slot-definition-reference slot)
         (with-slots ((referenced-class class) key)
             (dao-slot-definition-reference slot)
           (let* ((key (or key (primary-key referenced-class)))
                  (fk-values (mapcar (lambda (slot-name)
                                       (slot-value dao slot-name))
                                     key))
                  (*setting-unbound-reference-slot* t))
             (setf (slot-value dao slot-name) (get-dao referenced-class fk-values)))))
        ((dao-slot-definition-reverse-reference slot)
         (with-slots ((referenced-class class) key many)
             (dao-slot-definition-reverse-reference slot)
           (let ((key (or key (primary-key class)))
                 (pk-values (primary-key dao)))
             (setf (slot-value dao slot-name)
                   (select-dao referenced-class
                               :where (dao-test-sql-string referenced-class
                                                           :columns key
                                                           :values pk-values)
                               :limit (when (not many) 1))))))
        (t
         (call-next-method))))))

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
                        (dao-column-slot-sql-name class column-name))
                      (union (or (when (listp columns)
                                   columns)
                                 (mapcar #'slot-definition-name
                                         (ecase columns
                                           (:all (dao-class-column-slots class))
                                           (:default (dao-class-column-default-slots class))
                                           ((nil) nil)
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

(defmethod dao-test-sql-string ((class-name symbol) &rest keywords)
  (apply #'dao-test-sql-string (find-class class-name) keywords))

(defmethod dao-test-sql-string ((class dao-class)
                                &key
                                  (columns (primary-key class))
                                  values)
 (with-output-to-string (out)
    (loop
       :for remaining-columns :on columns
       :for column := (first remaining-columns)
       :for value :in values
       :do (format out "~A = ~A"
                   (dao-column-slot-sql-name class column)
                   (s-sql:sql-escape value))
       :when (rest remaining-columns)
       :do (write-string " AND " out))))

(defmethod dao-test-sql-string ((dao dao) &key &allow-other-keys)
  (when (every (lambda (slot-name)
                 (slot-boundp dao slot-name))
               (primary-key (class-of dao)))
    (dao-test-sql-string (class-of dao) :values (primary-key dao))))

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
    (let* ((where (dao-test-sql-string class :values pk-values)))
      (first (select-dao class :columns columns :where where)))))

(defgeneric insert-dao (dao)
  (:documentation "Insert the given object into the database."))

(defmethod insert-dao ((dao dao))
  (let* ((class (class-of dao))
         (table-name (dao-table-name class))
         (column-slot-names (dao-class-column-slot-names class))
         bound
         unbound)
    (loop :for slot-name :in column-slot-names
       :do (if (slot-boundp dao slot-name)
               (push slot-name bound)
               (push slot-name unbound)))
    (flet ((column-name (slot-name)
             (dao-column-slot-sql-name class slot-name)))
      (let* ((values (mapcar (lambda (x)
                               (list (column-name x)
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
                        (format out " RETURNING ~{~A~^, ~}" (mapcar #'column-name unbound))))))
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
                                           (dao-class-column-slot-names class))))
    (when column-slot-names
      (let* ((values (mapcan (lambda (slot-name)
                               (list (dao-column-slot-sql-name class slot-name)
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
                                             (dao-class-column-slot-names class))
                              columns))
         (query (generate-select-query (class-of dao)
                                       :columns refresh-columns
                                       :where (dao-test-sql-string dao))))
    (mapcar (lambda (slot-name)
              (slot-makunbound dao slot-name))
            (mapcar #'slot-definition-name
                    (append (dao-class-column-slots class)
                            (dao-class-reference-slots class)
                            (dao-class-reverse-reference-slots class))))
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
