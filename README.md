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
- [Usage (tutorial)](#usage-tutorial)
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
  - [Composite keys](#composite-keys)
  - [Lazy slots](#lazy-slots)
- [Running tests](#running-tests)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Installation

Until the project is added to quicklisp, the easiest way to install it is to
clone the repo, create a symlink to the project directory inside
`~/quicklisp/local-projects/`, then finally run:

```cl
(ql:quickload "zorm")
```

## Usage (tutorial)

The following examples illustrate usage and features of zorm in tutorial form.

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
  address text,
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
   (address :column t :initarg :address)
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
                                                 :employee-id 1
                                                 :address "1st street")))
; CL-POSTGRES query (2ms): INSERT INTO employees (address, name, employee_id, organization_id) VALUES (E'1st street', E'alice', 1, 1)
=> *ALICE*

(describe *alice*)
; #<EMPLOYEE alice {1003798C83}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   DB-NULL-SLOT-NAMES             = NIL
;   DIRTY-SLOT-NAMES               = NIL
;   ORGANIZATION-ID                = 1
;   EMPLOYEE-ID                    = 1
;   NAME                           = "alice"
;   ADDRESS                        = "1st street"
;   ORGANIZATION                   = #<ORGANIZATION acme {1002615DC3}>
```

You can also set the foreign key column slot directly instead of the reference
slot.

```cl
(defparameter *bob* (insert-dao (make-instance 'employee
                                               :name "bob"
                                               :organization-id 1
                                               :employee-id 2
                                               :address "2nd street")))
; CL-POSTGRES query (2ms): INSERT INTO employees (address, name, employee_id, organization_id) VALUES (E'2nd street', E'bob', 2, 1)
=> *BOB*
```

In this case the reference slot, `ORGANIZATION`, will be unbound
initially. However it is fetched on demand when `SLOT-VALUE` is called.

```cl
(describe *bob*)
; #<EMPLOYEE bob {1004D16FF3}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   DB-NULL-SLOT-NAMES             = NIL
;   DIRTY-SLOT-NAMES               = NIL
;   ORGANIZATION-ID                = 1
;   EMPLOYEE-ID                    = 2
;   NAME                           = "bob"
;   ADDRESS                        = "2nd street"
;   ORGANIZATION                   = #<unbound slot>
;; No value

(slot-value *bob* 'organization)
; CL-POSTGRES query (1ms): SELECT non_profit_p, address, name, organization_id FROM organizations WHERE organization_id = 1
=> #<ORGANIZATION acme {1004D1E783}>
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

When this slot is read, a SELECT query is made to fetch the list of employees
belonging to this organization.

```cl
(slot-value *org* 'employees)
; CL-POSTGRES query (1ms): SELECT name, organization_id, employee_id FROM employees WHERE organization_id = 1
=> (#<EMPLOYEE alice {100524A753}> #<EMPLOYEE bob {100524ABA3}>)
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

### Composite keys

The `employees` table has a composite primary key made up of two columns:
`organization_id` and `employee_id`.

Similarly, in the definition of `EMPLOYEE`, the primary key consists of
`ORGANIZATION-ID` and `EMPLOYEE-ID`.

How do we lookup an object with a composite primary key using `GET-DAO`? Simply
provide a list of key values, in the same order in which they are declared in
the class definition:

```cl
(get-dao 'employee (list 1 1))
; CL-POSTGRES query (6ms): SELECT name, organization_id, employee_id FROM employees WHERE organization_id = 1 AND employee_id = 1
=> #<EMPLOYEE alice {1005E20213}>
```

Similarly, in a reference slot, if you need to specify the `:KEY` or
`:REVERSE-KEY`, provide a list of slot names instead of a single name for a
composite primary key.

### Lazy slots

All direct and reverse reference slots are lazy. That is, when you run
`SELECT-DAO`, only column slots are bound and reference slots are left
untouched. When you run `SLOT-VALUE` on a reference slot name, it's value is
fetched on demand.

You can also mark one or more column slots as lazy -- simply set the `:LAZY`
option in the slot definition.

Let's do this for the `ADDRESS` slot of `EMPLOYEE`:

```cl
(defclass employee ()
  ((organization-id :column t :initarg :organization-id :reader organization-id)
   (employee-id :column t :initarg :employee-id :reader employee-id)
   (name :column t :initarg :name)
   (address :column t :initarg :address :lazy t)
   (organization :references organization :initarg :organization))
  (:metaclass dao-class)
  (:table-name employees)
  (:primary-key organization-id employee-id))
```

Now when we fetch employees (via `SELECT-DAO` or `GET-DAO`), the `address`
column by default will not be included.

```cl
(defparameter *employees* (select-dao 'employee))
; CL-POSTGRES query (1ms): SELECT name, organization_id, employee_id FROM employees
=> *EMPLOYEES*

(describe (first *employees*))
; #<EMPLOYEE alice {100562DA33}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   DB-NULL-SLOT-NAMES             = NIL
;   DIRTY-SLOT-NAMES               = NIL
;   ORGANIZATION-ID                = 1
;   EMPLOYEE-ID                    = 1
;   NAME                           = "alice"
;   ADDRESS                        = #<unbound slot>
;   ORGANIZATION                   = #<unbound slot>
```

When we try to read the `ADDRESS` slot, it will be fetched on demand.

```cl
(slot-value (first *employees*) 'address)
; CL-POSTGRES query (1ms): SELECT address FROM employees WHERE organization_id = 1 AND employee_id = 1
=> "1st street"
```

You can also specify the list of column slots to be fetched directly in the call
to `SELECT-DAO` or `GET-DAO`. If a lazy column is included in this list, it will
always be fetched; columns not included will not be fetched. However, primary
key values are always fetched.

```cl
(get-dao 'organization 1 :columns (list 'name))
; CL-POSTGRES query (0ms): SELECT name, organization_id FROM organizations WHERE organization_id = 1
=> #<ORGANIZATION acme {10036CA7F3}>
```

Instead of a list, you can also provide the keywords `:ALL` (all columns should
be fetched, regardless of whether they are lazy or not), or `:DEFAULT` (executes
the default behaviour).

## Running tests

The library ships with a bunch of unit tests which can be run as follows (do
this after loading zorm):

```cl
(asdf:test-system "zorm/test")
```

The test system requires PostgreSQL too, and, by default, assumes the following
Postgres credentials are in place:

* username: `zorm_test` (must have the `CREATEDB` role)
* password: `zorm_test`
* host: `localhost`

Before every test run, a test database is created (by default `zorm_test_db`),
and after every run it is discarded. In order to create and drop the test
database, a maintenance db is assumed to exist (by default `postgres`).

To override any of the defaults above, you can set these variables in the
`CL-USER` package before running the tests:

* `*ZORM-TEST-USERNAME*`
* `*ZORM-TEST-PASSWORD*`
* `*ZORM-TEST-HOST*`
* `*ZORM-TEST-DB-NAME*`
* `*ZORM-TEST-MAINTENANCE-DB-NAME*`
