# zorm

_zorm_ is a Common Lisp based ORM for PostgreSQL. Built on top of [Postmodern][]
and [CL-postgres][], it's salient features are:

* Support for relations
* Composite primary and foreign key support
* On-demand (lazy) fetching of columns

[Postmodern]: http://marijnhaverbeke.nl/postmodern/
[CL-postgres]: http://marijnhaverbeke.nl/postmodern/cl-postgres.html

## Table of Contents

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Installation](#installation)
- [Tutorial](#tutorial)
  - [Class definitions](#class-definitions)
  - [Insertion](#insertion)
  - [Querying](#querying)
  - [Updates](#updates)
  - [Deletion](#deletion)
  - [Refreshes](#refreshes)
  - [Handling NULL values](#handling-null-values)
  - [Relations](#relations)
    - [Direct references](#direct-references)
    - [Reverse references](#reverse-references)
    - [Non-matching reference keys](#non-matching-reference-keys)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Installation

Until the project is added to quicklisp, the easiest way to install it is to
clone the repo, create a symlink to the project directory inside
`~/quicklisp/local-projects/`, then finally run:

```cl
(ql:quickload "zorm")
```

## Tutorial

The following examples illustrate usage and features of zorm.

Make sure you have PostgreSQL (10 or above) installed, then create a test
database:

```
$ createdb zorm-examples
```

Create a few tables in the `zorm-examples` database by loading this file:

```sql
-- zorm-examples.sql

CREATE TABLE organizations (
  organization_id serial PRIMARY KEY,
  name text NOT NULL,
  address text,
  non_profit_p boolean
);

CREATE TABLE employees (
  organization_id integer NOT NULL REFERENCES organizations,
  employee_id integer NOT NULL,
  name text NOT NULL,
  PRIMARY KEY (organization_id, employee_id)
);
```

Now load zorm and connect to the database from Lisp:

```cl
(postmodern:connect-toplevel "zorm-examples" "<username>" "<password>" "<db-host>")
```

Ensure that you are using the `ZORM` package, or make a new package that uses it.

We also enable cl-postgres's SQL query logging so that we can inspect the
queries zorm makes as we go through the tutorial.

```cl
(setf cl-postgres:*query-log* t)
```

### Class definitions

Rows in a database table are parsed into data access objects (DAOs). One row is
parsed into a single DAO. We define an `ORGANIZATION` class to represent rows in
the `organizations` database table.

```cl
(defclass organization ()
  ((organization-id :column t :reader organization-id)
   (name :column t :initarg :name)
   (address :column t :initarg :address)
   (non-profit-p :column t :type boolean :initarg :non-profit-p))
  (:metaclass dao-class)
  (:table-name organizations)
  (:primary-key organization-id))

(defmethod print-object ((organization organization) stream)
  (print-unreadable-object (organization stream :type t :identity t)
    (write-string (slot-value organization 'name) stream)))
```

A few things to note here:

1. The metaclass is `DAO-CLASS`; this is what enables most of the functionality
   of this library.
2. The `TABLE-NAME` and `PRIMARY-KEY` options provide the name of the
   corresponding table and its primary key in the database.
3. A slot that corresponds to a table column should set the `COLUMN` option.

### Insertion

Let's create an organization and insert it in the database.

```cl
(defparameter *org* (insert-dao (make-instance 'organization :name "acme")))
; CL-POSTGRES query (5ms): INSERT INTO organizations (name) VALUES (E'acme') RETURNING non_profit_p, address, organization_id
=> *ORG*

(describe *org*)
; #<ORGANIZATION acme {1004077AD3}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   DB-NULL-SLOT-NAMES             = (ADDRESS NON-PROFIT-P)
;   DIRTY-SLOT-NAMES               = NIL
;   ORGANIZATION-ID                = 1
;   NAME                           = "acme"
;   ADDRESS                        = NIL
;   NON-PROFIT-P                   = NIL
```

Here we did not provide `ORGANIZATION-ID`, `ADDRESS` and `NON-PROFIT-P`, however
`INSERT-DAO` still works: it fetches the default value assigned to their
respective columns by the database, and sets those values in the relevant
slots. This behaviour is, in fact, required when you have a primary key column
with a `serial` type.

(Note that `DESCRIBE` also outputs a couple of book-keeping slots, however we
have snipped them here. Their names are internal to the `ZORM` package so they
should cause not cause any conflicts in your code)

Before we go further, let's insert a few more organizations:

```cl
(insert-dao (make-instance 'organization :name "asdf"))
(insert-dao (make-instance 'organization :name "qwerty"))
(insert-dao (make-instance 'organization :name "zxcv"))
(insert-dao (make-instance 'organization :name "hjkl"))
```

### Querying

The main querying function is `SELECT-DAO`.

```cl
(select-dao 'organization)
; CL-POSTGRES query (6ms): SELECT non_profit_p, address, name, organization_id FROM organizations
=> (#<ORGANIZATION acme {10040F56D3}> #<ORGANIZATION asdf {10040F59A3}>
    #<ORGANIZATION qwerty {10040F5C73}> #<ORGANIZATION zxcv {10040F5F43}>
    #<ORGANIZATION hjkl {10040F6213}>), 5
```

`SELECT-DAO` takes a number of keyword arguments, including `WHERE`, `ORDER-BY`,
`LIMIT`, AND `OFFSET`.

```cl
(select-dao 'organization :where "name = 'acme'")
; CL-POSTGRES query (2ms): SELECT non_profit_p, address, name, organization_id FROM organizations WHERE name = 'acme'
=> (#<ORGANIZATION acme {10040F9EC3}>), 1


(select-dao 'organization :order-by "organization_id DESC" :limit 3)
; CL-POSTGRES query (1ms): SELECT non_profit_p, address, name, organization_id FROM organizations ORDER BY organization_id DESC LIMIT 3
=> (#<ORGANIZATION hjkl {10042B5353}> #<ORGANIZATION zxcv {10042B5623}>
    #<ORGANIZATION qwerty {10042B58F3}>),
   3
```

Another querying function is `GET-DAO`, which returns a single object using a
primary key lookup.

```cl
(get-dao 'organization 1)
; CL-POSTGRES query (1ms): SELECT non_profit_p, address, name, organization_id FROM organizations WHERE organization_id = 1
=> #<ORGANIZATION acme {10042B91F3}>
```

### Updates

Use `UPDATE-DAO` to save an updated object in the database.

```cl
(setf (slot-value *org* 'name) "acme2")
=> "acme2"

(update-dao *org*)
; CL-POSTGRES query (4ms): UPDATE organizations SET name = E'acme2' WHERE organization_id = 1
=> NIL
```

zorm is smart enough to update only the changed slots in the database.

Another related function is `SAVE-DAO`. This will try to insert a new object in
the database if the primary keys are not set, else it will try to update the
object.

```cl
(save-dao (make-instance 'organization :name "uiop"))
; CL-POSTGRES query (2ms): INSERT INTO organizations (name) VALUES (E'uiop') RETURNING non_profit_p, address, organization_id
=> #<ORGANIZATION uiop {10042BF5B3}>

(setf (slot-value *org* 'name) "acme")
=> "acme"

(save-dao *org*)
; CL-POSTGRES query (3ms): UPDATE organizations SET name = E'acme' WHERE organization_id = 1
=> NIL
```

### Deletion

This is done using `DELETE-DAO`.

```cl
(delete-dao (get-dao 'organization 2))
; CL-POSTGRES query (1ms): SELECT created_on, name, organization_id FROM organizations WHERE organization_id = 2
; CL-POSTGRES query (3ms): DELETE FROM organizations WHERE organization_id = 2
=> NIL
```

### Refreshes

A DAO can also be refreshed i.e. it's column values are refetched from the
database. This is useful when another process updates a row in the database, and
we want its updated values.

```cl
(refresh-dao *org*)
; CL-POSTGRES query (0ms): SELECT non_profit_p, address, name, organization_id FROM organizations WHERE organization_id = 1
=> #<ORGANIZATION acme {1004077AD3}>
```

### Handling NULL values

The database's `NULL` translates to CL's `NIL` and vice-versa, with one
exception: if the type of a column slot is `BOOLEAN`, then `NIL` translates to
`false` in the database. To explicitly set the a boolean slot to `NULL` in the
database, use `(SETF DB-NULL-P)`.

Notice the values set in the database in the printed SQL query in examples
below (remember that the type for the slot `NON-PROFIT-P` is `BOOLEAN`):

```cl
(defparameter *org2* (insert-dao (make-instance 'organization :name "abc" :non-profit-p t :address "pluto")))
; CL-POSTGRES query (2ms): INSERT INTO organizations (non_profit_p, address, name) VALUES (true, E'pluto', E'abc') RETURNING organization_id
=> *ORG2*

(progn
  (setf (slot-value *org2* 'address) nil
        (slot-value *org2* 'non-profit-p) nil)
  (update-dao *org2*))
; CL-POSTGRES query (3ms): UPDATE organizations SET address = NULL, non_profit_p = false WHERE organization_id = 7
=> NIL

(db-null-p *org2* 'address)
=> T

(db-null-p *org2* 'non-profit-p)
=> NIL

(progn
  (setf (db-null-p *org2* 'non-profit-p) t)
  (update-dao *org2*))
; CL-POSTGRES query (3ms): UPDATE organizations SET non_profit_p = NULL WHERE organization_id = 7
=> NIL

(db-null-p *org2* 'non-profit-p)
=> T
```

### Relations

#### Direct references

Let's define an `EMPLOYEE` class to represent rows in the `employees` table.

```cl
(defclass employee ()
  ((organization-id :column t :initarg :organization-id :reader organization-id)
   (employee-id :column t :initarg :employee-id :reader employee-id)
   (name :column t :initarg :name)
   (organization :references organization :initarg :organization))
  (:metaclass dao-class)
  (:table-name employees)
  (:primary-key organization-id employee-id))

(defmethod print-object ((employee employee) stream)
  (print-unreadable-object (employee stream :type t :identity t)
    (write-string (slot-value employee 'name) stream)))
```

Take note of the `ORGANIZATION` slot -- this is known as a reference slot since
it references another DAO class (`ORGANIZATION` in this case).

Whenever the slot `ORGANIZATION` is set (either during object initialization or
later), the primary key slot names of the referenced class -- `ORGANIZATION-ID`
in this case -- are looked up in the referencing class and set.

This is similar to how the `employees` table in the db relates to the
`organizations` table via the `organization_id` foreign key column.

```cl
(defparameter *alice* (insert-dao (make-instance 'employee
                                                 :name "alice"
                                                 :organization *org*
                                                 :employee-id 1)))
; CL-POSTGRES query (3ms): INSERT INTO employees (name, employee_id, organization_id) VALUES (E'alice', 1, 1)
=> *ALICE*

(describe *ALICE*)
; #<EMPLOYEE alice {1003F64933}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   DB-NULL-SLOT-NAMES             = NIL
;   DIRTY-SLOT-NAMES               = NIL
;   ORGANIZATION-ID                = 1
;   EMPLOYEE-ID                    = 1
;   NAME                           = "alice"
;   ORGANIZATION                   = #<ORGANIZATION acme {1005FF83E3}>
```

You can also update the key slot of a reference directly. This will cause all
the reference slots using it to become unbound. Fetching the reference slot next
time will return a fresh object.

```cl
(setf (slot-value *alice* 'organization-id) 3)
=> 3

(describe *alice*)
; #<EMPLOYEE alice {1003F64933}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   DB-NULL-SLOT-NAMES             = NIL
;   DIRTY-SLOT-NAMES               = (ORGANIZATION-ID)
;   ORGANIZATION-ID                = 3
;   EMPLOYEE-ID                    = 1
;   NAME                           = "alice"
;   ORGANIZATION                   = #<unbound slot>
;
;; No value

(slot-value *alice* 'organization)
; CL-POSTGRES query (1ms): SELECT non_profit_p, address, name, organization_id FROM organizations WHERE organization_id = 3
=> #<ORGANIZATION qwerty {1003F64933}>
```

Next we will describe reverse references, but before going further, let's define
a few more employees.

```cl
(insert-dao (make-instance 'employee
                           :name "bob"
                           :organization *org*
                           :employee-id 2))

(insert-dao (make-instance 'employee
                           :name "eve"
                           :organization *org*
                           :employee-id 3))
```

#### Reverse references

A reverse reference, as the name indicates, refers to the reverse of a direct
reference. So if an `EMPLOYEE` references an `ORGANIZATION`, an `ORGANIZATION`
has a list of `EMPLOYEES`.

Here's our redefined `ORGANIZATION` class with the reverse reference in place:

```cl
(defclass organization ()
  ((organization-id :column t :reader organization-id)
   (name :column t :initarg :name)
   (address :column t :initarg :address)
   (non-profit-p :column t :type boolean :initarg :non-profit-p)
   (employees :references employee :reverse t))
  (:metaclass dao-class)
  (:table-name organizations)
  (:primary-key organization-id))
```

`EMPLOYEES` is the reverse reference, notice the presence of `:REVERSE T` in its
options.

When this slot value is accessed, a SELECT query is made to fetch the list of
employees belonging to this organization.

```cl
(slot-value *org* 'employees)
; CL-POSTGRES query (1ms): SELECT name, organization_id, employee_id FROM employees WHERE organization_id = 1
=> (#<EMPLOYEE alice {1003F64933}> #<EMPLOYEE eve {1003F64BD3}>
    #<EMPLOYEE bob {1003F64E73}>)
```

It is important to note that the value of a reverse reference slot is cached
after the first time it is fetched for an object. You can refresh this value by
either using `REFRESH-DAO` or by making the slot unbound via `SLOT-MAKUNBOUND`.

#### Non-matching reference keys

If the name of the key in the referencing class differs from the primary key in
the referenced class, additional options need to be provided.

The following two classes are variants of the `ORGANIZATION` and `EMPLOYEE`
respectively. Instead of an `ORGANIZATION-ID`, the `EMPLOYEE2` class has a
`MY-ORG-ID` slot. This requires us to provide the `:KEY` and `:REVERSE-KEY`
options while defining the reference slots.

```cl
(defclass organization2 ()
  ((organization-id :column t :reader organization-id)
   (name :column t :initarg :name)
   (address :column t :initarg :address)
   (non-profit-p :column t :type boolean :initarg :non-profit-p)
   (employees :references employee2 :reverse t :reverse-key my-org-id))
  (:metaclass dao-class)
  (:table-name organizations)
  (:primary-key organization-id))

(defclass employee2 ()
  ((my-org-id :column t :col-name organization-id :initarg :my-org-id)
   (employee-id :column t :initarg :employee-id :reader employee-id)
   (name :column t :initarg :name)
   (organization :references organization2 :key my-org-id :initarg :organization))
  (:metaclass dao-class)
  (:table-name employees)
  (:primary-key my-org-id employee-id))
```
