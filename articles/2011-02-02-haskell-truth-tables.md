---
title: Truth tables in Haskell
description: Hatt is a command-line program and Haskell library for parsing expressions in classical propositional logic and printing their truth tables.
---

There are few languages as simple yet as widely used as classical propositional
logic---perhaps only elementary arithmetic can claim a similar status. The
[propositional calculus][propcalc], as it is also known, is a staple of
first-year university logic courses.

As I recall from when I did just such a course, one of the most tedious parts
was calculating truth tables for various complex logical expressions. Once one
has grasped the basic concept, the rest is purely mechanical, and while it
might be useful in the exam, beyond that... well, that's what computers are
for.

[Hatt] is a command-line program for doing just that: parsing expressions of
the propositional calculus and printing their truth tables. Assuming you have
the [Haskell platform][platform] installed, just run the following to install
Hatt.

    cabal install hatt

Then just run `hatt` to enter the interactive mode. Here's an example session.

    > (A -> (B | ~C))
    A B C | (A -> (B | ~C))
    -----------------------
    T T T | F
    T T F | F
    T F T | F
    T F F | F
    F T T | F
    F T F | F
    F F T | T
    F F F | F
    > pretty
    Enabling pretty-printing.
    > (P <-> (~Q & (R -> S)))
    P Q R S | (P ↔ (¬Q ∧ (R → S)))
    ------------------------------
    T T T T | F
    T T T F | F
    T T F T | F
    T T F F | F
    T F T T | F
    T F T F | F
    T F F T | F
    T F F F | T
    F T T T | T
    F T T F | T
    F T F T | T
    F T F F | T
    F F T T | T
    F F T F | T
    F F F T | T
    F F F F | F

The `hatt` binary isn't the only thing you get when you install the package.
There's also the `Data.Logic.Propositional` module, which allows you to plug
this functionality---parsing, pretty-printing, truth tables, some simple
checks like whether an expression is a tautology or a contradiction---into any
Haskell program.

The [API] isn't terribly extensive, but it's enough to play around with, and
should be really easy to extend if you wanted, for example, to add the facility
to express proofs. I'm always amazed by how much one can accomplish with how
little in Haskell: Hatt is under 300 lines of code, and a lot of that is the
command-line program which is bloated by help text and the like.

This was, admittedly, a fairly trivial project, but it was a good way to
improve my familiarity with Haskell, particularly [Parsec]. Picking a small,
manageable project with precise goals proved to be a fun way to keep my hand in
and pick up a few new skills, and I highly recommend it as a technique. If
you're interested in taking a look at the source code, the entire project is
[available on GitHub][github] under the BSD license.

[propcalc]: http://en.wikipedia.org/wiki/Propositional_calculus
[Hatt]:     /projects/hatt
[platform]: http://hackage.haskell.org/platform/
[API]:      http://hackage.haskell.org/packages/archive/hatt/latest/doc/html/Data-Logic-Propositional.html
[Parsec]:   http://www.haskell.org/haskellwiki/Parsec
[github]:   https://github.com/beastaugh/hatt
