---
layout: article
title:  Hatt
---

[Hatt] is a command-line program which prints truth tables for expressions in
classical propositional logic, and a library allowing its parser, evaluator and
truth table generator to be used in other programs.

It's written in Haskell, and is available from [Hackage], so if you have the
`cabal-install` program it's very easy to install; from the terminal you can
just run the following commands.

    $ cabal update
    $ cabal install hatt

Then you can use the `hatt` binary to evaluate expressions. Use the `--help`
flag to see a list of available commands.

    $ hatt --evaluate="(A <-> (B | ~C))" --pretty
    A B C | (A ↔ (B ∨ ¬C))
    ----------------------
    T T T | T
    T T F | T
    T F T | F
    T F F | T
    F T T | F
    F T F | F
    F F T | T
    F F F | F

The following are all valid expression forms which can be parsed by Hatt, where
ϕ and ψ are metalinguistic variables standing in for any valid expression.

* Variables: `P`, `Q`, `R` etc.---basically anything in the character class
  `[A-Z]`
* Negation: `~ϕ`
* Conjunction: `(ϕ & ψ)`
* Disjunction: `(ϕ | ψ)`
* Conditional: `(ϕ -> ψ)`
* Biconditional: `(ϕ <-> ψ)`

The parser isn't as smart about parentheses as it could be, so you have to
follow these rules quite precisely. This shouldn't be a great hardship, but it
does mean that, for example, while `(A -> B)` is a valid expression, `A -> B`
isn't.

[Hatt]:    https://github.com/beastaugh/hatt
[Hackage]: http://hackage.haskell.org/package/hatt
