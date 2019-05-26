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

```lisp
(ql:quickload "zorm")
```

## Usage

The following examples illustrate usage and features of zorm.

Make sure you have PostgreSQL (9 or above) installed, then create a test
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
  created_on date NOT NULL DEFAULT CURRENT_DATE
);

CREATE TABLE employeees (
  organization_id integer NOT NULL REFERENCES organizations,
  employee_id integer NOT NULL,
  name text NOT NULL,
  joined_on date NOT NULL DEFAULT CURRENT_DATE,
  PRIMARY KEY (organization_id, employee_id)
);
```

Now load zorm and connect to the database from Lisp:

```lisp
(postmodern:connect-toplevel "zorm-examples" "<username>" "<password>" "<db-host>")
```

Ensure that you are using the `ZORM` package, or make a new package that uses it.

### Class definition

Rows in a database table are parsed into data access objects (DAOs). One row is
parsed into a single DAO object. We define an `ORGANIZATION` class to represent
the `organizations` database table.

```lisp
(defclass organization ()
  ((organization-id :column t :reader organization-id)
   (name :column t :initarg :name :accessor organization-name)
   (created-on :column t :accessor created-on))
  (:metaclass dao-class)
  (:table-name organizations)
  (:primary-key organization-id))
```

A few things to note here:

1. The metaclass is `DAO-CLASS`; this is what enables most of the functionality
   of this library.
2. The `TABLE-NAME` and `PRIMARY-KEY` options provide the name of the
   corresponding table and its primary key in the database.
3. Every slot that corresponds to a table column should set the `COLUMN` option.

### Insertion

Let's create an organization and insert it in the database.

```lisp
(defparameter *org* (insert-dao (make-instance 'organization :name "acme")))
; CL-POSTGRES query (14ms): INSERT INTO organizations (name) VALUES (E'acme') RETURNING created_on, organization_id
=> *ORG*

(describe *org*)
; #<ORGANIZATION {100402B933}>
;   [standard-object]
;
; Slots with :INSTANCE allocation:
;   ...
;   ORGANIZATION-ID                = 1
;   NAME                           = "acme"
;   CREATED-ON                     = 3767817600
```

Here we did not provide `ORGANIZATION-ID` and `CREATED-ON`, however `INSERT-DAO`
still works: it fetches the default value assigned to their respective columns
by the database, and sets those values in the relevant slots. This behaviour is,
in fact, required when you have a primary key column with a `serial` type.

(Note that `DESCRIBE` also outputs a couple of book-keeping slots, however we
have snipped them here. Their names are internal to the `ZORM` package so they
should cause not cause any conflicts in your code)

Before we go further, let's insert a few more organizations:

```lisp
(insert-dao (make-instance 'organization :name "asdf"))
(insert-dao (make-instance 'organization :name "qwerty"))
(insert-dao (make-instance 'organization :name "zxcv"))
(insert-dao (make-instance 'organization :name "hjkl"))
```

### Querying

The main querying function is `SELECT-DAO`.

```lisp
(select-dao 'organization)
; CL-POSTGRES query (1ms): SELECT created_on, name, organization_id FROM organizations
=> (#<ORGANIZATION {100197C483}> #<ORGANIZATION {100197C6E3}>
    #<ORGANIZATION {100197C943}> #<ORGANIZATION {100197CBA3}>
    #<ORGANIZATION {100197CE03}>)

```

`SELECT-DAO` takes a number of keyword arguments, including `WHERE`, `ORDER-BY`,
`LIMIT`, AND `OFFSET`.

```lisp
(select-dao 'organization :where "name = 'acme'")
; CL-POSTGRES query (16ms): SELECT created_on, name, organization_id FROM organizations WHERE name = 'acme'
=> (#<ORGANIZATION {10019CDD03}>)

(select-dao 'organization :order-by "created_on ASC" :limit 3)
; CL-POSTGRES query (5ms): SELECT created_on, name, organization_id FROM organizations ORDER BY created_on ASC LIMIT 3
=> (#<ORGANIZATION {1001BE8503}> #<ORGANIZATION {1001BE8763}>
    #<ORGANIZATION {1001BE89C3}>)
```

Another querying function is `GET-DAO`, which returns a single object using a
primary key lookup.

```lisp
(get-dao 'organization 1)
; CL-POSTGRES query (1ms): SELECT created_on, name, organization_id FROM organizations WHERE organization_id = 1
#<ORGANIZATION {1001D35643}>
```

### Updates

Use `UPDATE-DAO` to save an updated object in the database.

```lisp
(setf (organization-name *org*) "acme2")
=> "acme2"

(update-dao *org*)
; CL-POSTGRES query (4ms): UPDATE organizations SET name = E'acme2' WHERE organization_id = 1
=> NIL
```

zorm is smart enough to update only the changed slots in the database.

Another related function is `SAVE-DAO`. This will try to insert a new object in
the database if the primary keys are not set, else it will try to update the
object.

```lisp
(save-dao (make-instance 'organization :name "uiop"))
; CL-POSTGRES query (3ms): INSERT INTO organizations (name) VALUES (E'uiop') RETURNING created_on, organization_id
=> #<ORGANIZATION {1001F40C43}>

(save-dao *org*)
; CL-POSTGRES query (3ms): UPDATE organizations SET name = E'acme' WHERE organization_id = 1
=> NIL
```

### Deletion

This is done using `DELETE-DAO`.

```lisp
(delete-dao (get-dao 'organization 2))
; CL-POSTGRES query (1ms): SELECT created_on, name, organization_id FROM organizations WHERE organization_id = 2
; CL-POSTGRES query (3ms): DELETE FROM organizations WHERE organization_id = 2
=> NIL
```
