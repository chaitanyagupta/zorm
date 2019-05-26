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

First create a few tables in the `zorm-examples` database.

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

### Basics

Rows in a database table are parsed into data-access objects (DAOs). One row is
parsed into a single dao object. We define an `ORGANIZATION` class to represent
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

1. The metaclass is `DAO-CLASS`, which is what enables most of the functionality
   of this library.
2. The `:TABLE-NAME` and `:PRIMARY-KEY` options provide the name of the
   corresponding table and its primary key in the database.
3. Every slot that corresponds to a table column should set the `:COLUMN` option.

Let's create an organization and insert it in the database.

```lisp
(insert-dao (make-instance 'organization :name "acme"))
;; CL-POSTGRES query (14ms): INSERT INTO organizations (name) VALUES (E'acme') RETURNING created_on, organization_id
=> #<ORGANIZATION {100377B8A3}>
```
