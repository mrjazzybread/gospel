# Bird's eye view

The Gospel type checker uses the OCaml parser to create an AST of the
OCaml file.  We then traverse the generated AST, parse all Gospel
attributes and create an untyped Gospel AST, which is essentially a
fragment of the original OCaml AST combined with special Gospel nodes.

This AST is then fed into the type checker which leverages the `Inferno`
library to perform type inference.  Before using the utilities exposed
by `Inferno`, however, we perform name resolution, type arity checks and
other tasks that that are not covered by `Inferno`.  This process
generates another intermediate AST very similar to the previous Gospel
AST, the main difference being that each variable has been uniquely
tagged with an identifier.

Finally, each signature in this intermediate AST that contains a
Gospel term will be fed into the `Inferno` solver for type
inference.  Assuming that there are no typing errors, a final typed AST
is produced.  This AST is similar to the previous, except that every
term and every variable has a type associated with it.

# Code map

## `Uparser`

Parses Gospel attributes written in the OCaml interface file. Note
that this parser does not concern itself with parsing OCaml
signatures, which is done by the OCaml parser provided by `ppx_lib`.

## `Uast`

Defines two parse trees; the first is the untyped parse tree generated
by the `Uattr2spec` module which describes an OCaml program annotated
with Gospel attributes.  Naturally, this AST contains nodes similar to
those found in `Parsetree` module from `Ppx_lib`.  This AST also defines
the structure of Gospel terms, which is effectively a fragment of
OCaml expressions augmented with quantifiers and other logical
expressions.

The second AST `IdUast` is extremely similar to the previous with a
few minor differences, the most important being that identifiers are
fully resolved, uniquely tagged.  If they are top level names, they
are also usually coupled with any typing information that Inferno does
not have direct access to.

Note that both ASTs are very similar to one another; if a new
construct is added to the language, it will almost always imply adding
the exact same node to both ASTs.

There are several reasons we need two ASTs, the most glaring being
that Inferno has no way of knowing if two type names when multiple
types can have the same name or have aliases.  This means that we need
an intermediate AST where some type checking tasks have already been
done.

## Ident

The `Ident` module defines unique identifiers generated during name
resolution.  These carry a unique tag that allows one to differentiate
between values with the same name.  This unique tag is only accessed
while running the Inferno solver when we want to check if two type
names are equal.  Inferno also technically uses it to check if two
local variables are the same, however this could be done with the raw
string value since Inferno has a notion of scopes and shadowing.  This
means that if we have `let x = 0 in let x = 1 in x`, Inferno will know
that `x` refers to the inner most `x`.  Since both are equivalent,
both in terms of functionality and code complexity, we use the unique
identifiers since it is a bit more resistant to any future changes in
the code base.

## `Typing`

The `Typing` module, as the name implies, takes an untyped Gospel AST
and checks if it is a valid OCaml interface and if the Gospel
specifications are well typed.  To do this, it uses the `Namespace`
module to store the information associated with each name in scope.
The specific information varies depending on the type of name (type,
record field, etc...) but it always includes a unique identifier.
Additionally, it uses the `Namespace` module to obtain the unique
identifiers necessary to build the typed AST.  When we have a Gospel
structure that includes a Gospel term, we must first build an
intermediate Gospel AST with unique identifiers and use one of the
functions exposed by `Solver` to type check the program and produce a
typed AST.

# Tests

The tests are almost all expect tests; each test is an OCaml interface
file where the expected error message is at the bottom in an OCaml
comment (if no error is expected, no comment is found).  If a change
to the type checker changes an error message, then either there is a
bug (most likely) or the expected error message has changed; in the
latter case run `dune promote` after `dune test` to update the error
message.

To add a new expect test, navigate to the directory you want to add
the test to and create the `.mli` file with your test.  You may then
run `dune test --auto-promote` to generate the dune rules for your new
test and then `dune test` again to see if the test produces the output
you expect.  If so, you may run `dune promote` again to generate the
OCaml comment with the expected output.  Running `dune test` again
should succeed with no errors.

If you wish to create your tests in a new directory, the steps are the
same as in the previous paragraph, but you must create a `dune` file
where you include the following stanzas `(include path) (include
dune.inc)` where `path` is replaced with the relative path to the file
`dune.common`.  Additionally, you must create an empty `dune.inc`
file.

If you wish to add tests whose behaviour cannot be defined with an
expect test, we recommend creating a directory test (more info in
<https://dune.readthedocs.io/en/stable/tests.html>)

# Strong Recommendations

If you wish to contribute to Gospel, you should ensure that in each
commit your code is correctly formatted (to do this, run `dune fmt`)
and that each test still passes.  One way of ensuring this is creating
a
[pre-commit](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
hook with the command `dune fmt --preview && dune test`.  This way,
invalid commits are caught before they are pushed (note that hooks can
always be disabled if needed).
