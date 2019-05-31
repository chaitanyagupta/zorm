# zorm

_zorm_ is a Common Lisp based ORM for PostgreSQL. Built on top of [Postmodern][]
and [CL-postgres][], it's salient features are:

* Support for relations
* Composite primary and foreign key support
* On-demand (lazy) fetching of columns

[Postmodern]: http://marijnhaverbeke.nl/postmodern/
[CL-postgres]: http://marijnhaverbeke.nl/postmodern/cl-postgres.html

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
parsed into a single DAO. We define an `ORGANIZATION` class to represent the
`organizations` database table.

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
3. Every slot that corresponds to a table column should set the `COLUMN` option.

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
