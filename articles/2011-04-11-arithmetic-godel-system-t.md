---
title: Developing arithmetic in Gödel's system T
description: The modern formulation of Gödel's system T is a typed λ-calculus which can represent the natural numbers. This article illustrates the development of arithmetic functions within T.
---

This year I've been running a reading group on type theory. Our primary
text is [_Proofs and Types_] by Jean-Yves Girard, Yves Lafont and Paul
Taylor. The focus of the book is the development of the typed λ-calculus, with
a strong proof-theoretic slant, these two perspectives being unified by the
[Curry-Howard correspondence]. It begins by exploring the connections between
[intuitionistic logic] and the [simply typed lambda calculus], and works up to
a fairly in-depth study of [System F].

One of the intermediate steps is the system _T_, originally developed by Kurt
Gödel in his [Dialectica interpretation], and reformulated by Tait in 1967. I
won't try to cover the history here---there are plenty of articles out there if
you're interested. (The names of the articles alluded to are given in the
references at the end.)

The modern formulation of _T_ is as a typed λ-calculus with natural number and
boolean types. In chapter 7 of _Proofs and Types_, Girard shows how the natural
numbers can be represented in _T_, and provides a definition of addition based
on the recursion operator $\mathsf{R}$. He goes on to say that

> Among easy exercises in this style, one can amuse oneself by defining
> multiplication, exponenential, predecessor _etc._

So, let's amuse ourselves! Thinking about these simple examples is helpful
because it gives a working demonstration of how the recursion operator adds
power back to the language which was lost when the restrictive type system was
introduced.

For example, the untyped λ-calculus allows one to use [Church encoding] to
represent the natural numbers, but in the simply typed lambda calculus one is
barred from forming the relevant terms by the requirement that every term be
well-typed.

During the reading group meeting where we discussed this chapter, we managed to
assemble a working definition for multiplication, which I'll present below
along with the other arithmetical functions Girard suggests. However, before we
get onto that, I'm going to briefly sketch how the type of natural numbers
$\mathsf{Nat}$ is defined in _T_; the introduction and elimination rules for
terms of that type, on which everything else is built; and how an addition
function can be defined in terms of those rules.

A small niggle: Girard actually talks about an _integer type_ $\mathsf{Int}$.
When one examines the nature of the type it becomes clear that what's intended
is a natural number type, so I'm going to talk solely about natural numbers and
call the type in question $\mathsf{Nat}$, not $\mathsf{Int}$.

[_Proofs and Types_]: http://www.PaulTaylor.EU/stable/Proofs+Types.html
[Curry-Howard correspondence]: http://en.wikipedia.org/wiki/Curry–Howard_correspondence
[intuitionistic logic]: http://en.wikipedia.org/wiki/Intuitionistic_logic
[simply typed lambda calculus]: http://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
[System F]: http://en.wikipedia.org/wiki/System_F
[Dialectica interpretation]: http://en.wikipedia.org/wiki/Dialectica_interpretation
[Church encoding]: http://en.wikipedia.org/wiki/Church_encoding


Natural numbers in _T_
----------------------

System _T_ is a variant of the simply typed lambda calculus, so it has three
sorts of rules: those stating which _types_ exist; those stating which _terms_
of those types can be formed; and reduction rules stating how some terms can be
reduced to others. These are spelled out in §3.1 of [_Proofs and Types_], so if
any of the following doesn't make sense, I recommend checking that out (the
book is freely available from the link above).

For system _T_, there are also two constant types, $\mathsf{Nat}$ and
$\mathsf{Bool}$; here we shall only be concerned with $\mathsf{Nat}$ so I shall
omit the rules mentioning $\mathsf{Bool}$. The additional term formation rules
for system _T_ specify how one can introduce and eliminate terms of
$\mathsf{Nat}$ and $\mathsf{Bool}$. The introduction rules are:

* $\mathsf{O}$ is a (constant) term of type $\mathsf{Nat}$;
* If $t$ is a term of $\mathsf{Nat}$, then $\mathsf{S}t$ is a term of type
  $\mathsf{Nat}$.

These mirror their equivalents in Peano Arithmetic almost exactly---that is,
they represent zero and the successor function. There is also the elimination
rule for $\mathsf{Nat}$, which introduces the recursion operator $\mathsf{R}$:

* If $u$, $v$, $t$ are of types respectively $U$, $U \rightarrow (\mathsf{Nat}
  \rightarrow U)$ and $\mathsf{Nat}$, then $\mathsf{R}uvt$ is of type $U$.

The recursion operator allows us to combine the introduction rules to give
recursive definitions of arithmetic operations such as addition and
multiplication in a form not too far removed from their counterparts in Peano
Arithmetic. Its reduction rule is given by two cases:

* $\mathsf{R}uv\mathsf{O}$ reduces to $u$.
* $\mathsf{R}uv(\mathsf{S}x)$ reduces to $v(\mathsf{R}uvx)x$.

The first is the _base case_, which ensures that the recursion is well-founded.
The second rule gives, for the recursive case, a way of eliminating an instance
of the recursion operator. The term it reduces to looks more complex than the
one we started with, but it's clear that eventually we're going to run out of
successors and the reduction process will terminate (a proof of this is given
in §7.2 of _Proofs and Types_).


Addition
--------

With the preamble out of the way, we're finally ready to start defining the
usual arithmetic functions within _T_. The easiest place to start is with the
definition of addition.

> $x + 0 = x$
>
> $x + S(y) = S(x + y)$

There's no recursion operator in sight in this definition, but of course there
is recursion occurring. The first equation gives the base case, defining the
result when the addition function is used to combine a number $x$ with $0$. The
second equation defines the recursive case, giving the result when we add the
successor of some number $y$ to some number $x$. The recursion operator
$\mathsf{R}$ has three arguments, and the first two map to these cases: the
value of the base case (here it would be $x$) and the function to apply in the
recursive case (here, the successor function).

With this in mind, we can formulate an addition function in _T_ pretty
straightforwardly. The variables bound by lambda abstractions have been given
type annotations, just to make clear how the types of these terms relate to
those specified in the introduction and elimination rules for terms of type
$\mathsf{Nat}$. I shall omit these annotations from the definitions in the rest
of the article.

> $\mathsf{add} = \lambda{}x^{\mathsf{Nat}}. \lambda{}y^{\mathsf{Nat}}.
> \mathsf{R} x (\lambda{}z^{\mathsf{Nat}}. \lambda{}w^{\mathsf{Nat}}.
> \mathsf{S}z) y$

The second term given to $\mathsf{R}$ is just a double lambda abstraction over
$\mathsf{S}x$, representing a two-argument function; the second argument (the
numbers accumulating on the right of the reduction) will always be thrown away.
The type of the term must be, by the definition we gave earlier, $U \rightarrow
(\mathsf{Nat} \rightarrow U)$. In this case we're saying that the type $U$ is
in fact the constant type $\mathsf{Nat}$, so the term has a type of
$\mathsf{Nat} \rightarrow (\mathsf{Nat} \rightarrow \mathsf{Nat})$.

I should note that there is no mechanism in _T_ for giving names to definitions
(i.e. let binding), so equations like the one given above should simply be seen
as expressing abbreviations, not as expressions in the language of _T_,
although the lambda term on the right hand side certainly is.


Multiplication
--------------

Again, let's begin with the definition of multiplication we find in Peano
Arithmetic.

> $x \cdot 0 = 0$
>
> $x \cdot S(y) = (x \cdot y) + y$

Here we can see that in the base case, when we combine $0$ with $x$ the result
is $0$. In contrast, in the base case for addition when we combine $0$ with $x$
the result is $x$. This is reflected in the following definition of
multiplication in _T_, with $\mathsf{O}$ as the base case, and _addition_ as
the operation applied in the successor case.

> $\mathsf{mult} = \lambda{}x. \lambda{}y. \mathsf{RO} (\lambda{}z. \lambda{}w.
> (\mathsf{add}) x z) y$

Expanding the definition of $\mathsf{add}$ in $\mathsf{mult}$ (with some
trivial α-conversion and β-reduction), it's easy to see the double recursion
that we also see in the $\mathsf{PA}$ definitions.

> $\mathsf{mult} = \lambda{}x.\lambda{}y. \mathsf{RO}(\lambda{}z.\lambda{}w.
> \mathsf{R}x (\lambda{}u.\lambda{}v. \mathsf{S}u)z)y$


Exponentiation
--------------

Just as multiplication is built on addition, adding an extra layer of
recursion, so exponentiation is built on multiplication. The $\mathsf{PA}$
equations are

> $x^0 = 1$
>
> $x^S(y) = x^y \cdot y$

Again, all we need to do to define this new function is state the base case
($\mathsf{SO}$, corresponding to the rule that any number raised to exponent 0
is 1) and the function to apply in the recursive case (multiplication).

> $\mathsf{exp} = \lambda{}x. \lambda{}y. \mathsf{R} \mathsf{SO} (\lambda{}z.
> \lambda{}w. ((\mathsf{mult}) x z)) y$


Predecessor
-----------

The predecessor function is given by the following equations.

> $Pred(0) = 0$
>
> $Pred( S(x) ) = x$

Compared to the nested recursions given above, the definition of the
predecessor function in _T_ is strikingly simple.

> $\mathsf{pred} = \lambda{}x. \mathsf{RO} (\lambda{}y. \lambda{}z. z) x$

The insight which allows one to derive it is similarly simple. An application
of the predecessor function to some successor (in the case of $\mathsf{O}$ the
predecessor will simply be $\mathsf{O}$ too) will be of this form: $\mathsf{RO}
(\lambda{}y. \lambda{}z. z) (\mathsf{S}x)$. When we look at the reduced term
after one reduction, we have something like $(\lambda{}y. \lambda{}z. z)
(\mathsf{RO} (\lambda{}y. \lambda{}z. z) x) x$.

Ignore the new occurrence of the recursion operator; just consider it as
another term. Instead, look at the whole formula as an application. Clearly,
the first argument to the function on the left will always be discarded: the
whole term will always reduce simply to $x$, the predecessor of $\mathsf{S}x$,
our initial argument.


References & further reading
----------------------------

* Girard, Lafont and Taylor's [_Proofs and Types_] is the primary reference for
  this article.
* System _T_ was first introduced by Kurt Gödel in his 1958 _Dialectia_ article
  'Über eine bisher noch nicht benützte Erweiterung des finiten Standpunktes',
  which is reproduced (with an English translation) in volume II of his
  [_Collected Works_], edited by Feferman et al.
* The more modern version of _T_ which Girard et al. work from was first given
  by William Tait in his 1967 [_JSL_] article, 'Intensional interpretation of
  functionals of finite type I'.
* From a programming languages perspective, Benjamin Pierce provides a good
  introduction to the simply typed lambda calculus in chapter 9 of his book
  [_Types and Programming Languages_].
* A more general introduction to the λ-calculus is Hindley and Seldin's
  [_Lambda-Calculus and Combinators_], while Barendregt's [_The Lambda
  Calculus: Its Syntax and Semantics_] is the definitive reference work in the
  field.

[_Collected Works_]: http://ukcatalogue.oup.com/product/9780195147216.do
[_JSL_]: http://www.aslonline.org/journals-journal.html
[_Types and Programming Languages_]: http://www.cis.upenn.edu/~bcpierce/tapl/
[_Lambda-Calculus and Combinators_]: http://www.cambridge.org/gb/knowledge/isbn/item1175709/
[_The Lambda Calculus: Its Syntax and Semantics_]: http://mathgate.info/cebrown/notes/barendregt.php
