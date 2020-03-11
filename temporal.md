table with (label: Type) using transaction(transaction_from, transaction_to) from db;
table with (label: Type) using valid(transaction_from, transaction_to) from db;
table with (label: Type) using bitemporal(transaction_from, transaction_to, valid_from, valid_to) from db;



OK, so TableHandle types are of the form
  TableHandle(R, W, N)
where R contains the fields that can be read; W contains the fields that can be
written; and N contains the fields that must be specified in an update

I propose we extend this with temporal metadata T:
  TableHandle(R, W, N, T)

where T is:

  T ::=
    | Current
    | ValidTime(from_field: string, to_field: string)
    | TransactionTime(from_field: string, to_field: string)
    | Bitemporal(transaction_from_field: string, transaction_to_field: string,
        valid_from_field: string, valid_to_field: string)

I don't think this will affect the fields in the DB.

# Transaction-time metadata implementation

Transaction-time metadata is simply a record. We can implement it as an abstract
type, with the underlying implementation as a record, and associated accessor
functions.


  TransactionTime(R)

  ttData  : TransactionTime(R) -> (R)
  ttStart : TransactionTime(R) -> DateTime
  ttEnd   : TransactionTime(R) -> DateTime

What we would need to do, however, is ensure that it is correctly constructed.

Table accessors:


  G |- M : TableHandle(R, _, __, Transaction(tf, tt))    G, x : TransactionTime(R) |- M : A
  ----------------------------------------------------------------------------------------------
  G |- for (x <-- t) M : [A]


Queries can reasonably straightforwardly construct TransactionTime(R) metadata
by just injecting into a record. Not entirely sure how it would fit with
shredding, but could have a go.

In the provenance translation paper, we have something like:

  [| TTable(R) |] = (Table(<|R|>), () -> [ [| TransactionTime(R) |]  ])

where <|R|> is an erasure function to remove provenance information, and the
second projection is a function which adds where provenance to all records.

This is good in some respects: having a function like this is in keeping with
the narrowing / demotion functions I'll want later on. At the same time, it's
_very_ global, in that every access (even for plain tables) then requires a projection.

So, what's the alternative? I think it's as follows. Why don't we augment the
runtime representation with the temporal metadata used in the type (maybe with
some further information)? Then, we could insert the predicate into the
`Iteration` evaluation case before normalisation.

How would this work with insertions and deletions? Similarly, I think?

Essentially we'd need to translate transaction-time metadata into the relevant
accessors, but other than that, since we're not allowing nested updates or
deletions, we wouldn't need to do anything special.


# Time constants

  - now : () -> DateTime

we have getServerTime, but I don't think this quite cuts it, as getServerTime is
impure. What might be worth doing is having a pure function `now ()`, and
compiling it to `localtime` on the DB.

  - forever : DateTime

this can be implemented straightforwardly as a date such as '3000-01-01 23:59:59'

# Helpers

Things we might want to do:

  currentSnapshot : TableHandle(R, W, N, TransactionTime(...)) -> TableHandle(R, W, N, Current)

demotes a transaction-time table to a current table. We'd need to endow the
runtime representation of table handles with knowledge that it was actually a
demoted table, meaning queries would need to be elaborated to include the
current time predicate

  snapshotAt : (TableHandle(R, W, N, TransactionTime(...)), DateTime) -> TableHandle(R, W, N, Current)

again, demotes a transaction-time table to a current table, but specifies the
instant at which the table should be valid.

(Probably worthwhile putting some time-narrowing ones in, too -- I imagine we
could probably define a monoid(?) of time-narrowing combinators and optimise
down to a single query).



# Action Plan

  1. Temporal metadata:
    - Abstract type definition (DONE)
    - Accessor functions (DONE -- kinda).
    - Translation of accessor functions (DONE -- added to evaluator).

  2. Extension of table term and TableHandle type to incorporate temporal nature
     of tables:

    - ASTs
    - Parser
    - Typechecker

  3. Implementation of semantics

