---
title: Approaches to currying in JavaScript
description: Due to its dynamic nature, no single currying technique is universally efficacious in JavaScript programming. In this article, various approaches to currying in JavaScript are discussed, together with their associated advantages and disadvantages, and some conclusions are drawn.
---

JavaScript's dynamic nature makes it hard to straightforwardly apply many
functional programming idioms. One example of this is [currying][currying]: any
function may be passed an arbitrary number of arguments, making it impossible
to write a truly general currying function.

To recap, currying is a technique for transforming a function which accepts _n_
parameters into a nest of partially applicable functions. Consider the
function _f = λxyz.M_, which has three parameters, _x_, _y_ and _z_. By
currying, we obtain a new function _f* = λx.(λy.(λz.M))_.

One simple example is currying an `add` function which accepts 2 parameters and
returns the result of adding them together.

~~~ {.JavaScript}
var add = function(a, b) {
    return a + b;
};

var curriedAdd = function(a) {
    return function(b) {
        return a + b;
    };
};
~~~

A function which returns the result of evaluating a quadratic expression
demonstrates more clearly the 'nesting' of functions which currying produces.

~~~ {.JavaScript}
var quadratic = function(a, b, c, x) {
    return a * x * x + b * x + c;
};

var curriedQuadratic = function(a) {
    return function(b) {
        return function(c) {
            return function(x) {
                return a * x * x + b * x + c;
            };
        };
    };
};
~~~

Given a pattern like this, the obvious question is how to generalise it.
Ideally, we would write a `curry` function to automatically transform functions
like `quadratic` into ones like `curriedQuadratic`. The simplest approach is to
make curried functions always return a single wrapping function:

~~~{.JavaScript}
var naiveCurry = function(f) {
    var args = Array.prototype.slice.call(arguments, 1);
    
    return function() {
        var largs = Array.prototype.slice.call(arguments, 0);
        return f.apply(this, args.concat(largs));
    };
};
~~~

Clearly this is not true currying, except for functions of arity 2. We cannot
use it to perform the transformation from `quadratic` to `curriedQuadratic`.

A cleverer approach would be to detect the arity of the function we wish to
curry. To do this, we can use the length property of the function, which
returns the number of named arguments the function accepts. `Math.tan.length`
is 1, while `parseInt.length` is 2.

~~~{.JavaScript}
var curryByLength = function(f) {
    var arity = f.length,
        args  = Array.prototype.slice.call(arguments, 1),
    
    accumulator = function() {
        var largs = args;
        
        if (arguments.length > 0) {
            // We must be careful to copy the `args` array with `concat` rather
            // than mutate it; otherwise, executing curried functions can have
            // strange non-local effects on other curried functions.
            largs = largs.concat(Array.prototype.slice.call(arguments, 0));
        }
        
        if (largs.length >= arity) {
            return f.apply(this, largs);
        } else {
            return curryByLength.apply(this, [f].concat(largs));
        }
    };
    
    return args.length >= arity ? accumulator() : accumulator;
};
~~~

However, the length property of any given JavaScript function can easily
mislead. To begin with, we often find it useful to define functions with
optional parameters.

~~~{.JavaScript}
var someFunction = function(a, flag) {
    if (flag) {
        // Some computation involving a
    } else {
        // Some other computation involving a
    }
};
~~~

Now consider a variadic function, like `Math.max`, which returns the largest
number amongst its arguments. Despite the fact that it can in fact be called
with any number of arguments, including 0 and 1, it has a length property of 2.
Consequently, our 'smarter' curry function will only work with `Math.max` up to
a point. This will throw a type error, even though `Math.max` will accept three
arguments quite happily:

~~~{.JavaScript}
curryByLength(Math.max)(1)(2)(3);
~~~

Currying `Math.max` limits its utility to discriminating between two numbers,
not _n_ numbers. We can easily think of similar examples---other variadic
functions, functions with optional arguments, and similarly clever abuses of
JavaScript's dynamic arguments to create complex APIs. jQuery's `bind` method
could be considered [an example][jqbind] of this: the event handler can be
passed to the method as either the second or the third argument, depending on
whether the user wishes to use the optional `eventData` parameter or not.

It is easy to see that there is no general way of resolving this issue:
currying is essentially at odds with variadic functions and the ability to
change the number of arguments a function accepts at runtime. However, one's
choices are not limited simply to the approaches discussed above; there are
alternatives, even if they do not fully dispose of the problem of dynamic
arity.

Firstly, one can simply leave things as they are, with the `curry` function
having a known limitation around functions with dynamic arity. The burden is
placed on the user to ensure they take care when currying.

Alternatively, one could make the arity an explicit component of the `curry`
function. This differs from the implicit detection of the arity via the curried
function's length property (however, the implementation is almost identical).

~~~{.JavaScript}
var curryWithExplicitArity = function(f, n) {
    var args = Array.prototype.slice.call(arguments, 2),
    
    accumulator = function() {
        var largs = args;
        
        if (arguments.length > 0) {
            largs = largs.concat(Array.prototype.slice.call(arguments, 0));
        }
        
        if (largs.length >= n) {
            return f.apply(this, largs);
        } else {
            return curryByLength.apply(this, [f].concat(largs));
        }
    };
    
    return args.length >= n ? accumulator() : accumulator;
};
~~~

Finally, one could have entirely different `curry` functions for each arity.
This has the benefit of being explicit, and while it doesn't solve the problem
of functions with dynamic arity, it does mean that one doesn't have to specify
the arity of the function one wishes to curry each time as an additional
parameter. Instead of writing `curry(f, 3)`, one can simply write `curry3(f)`.

In fact, there is a way to combine these last two approaches, by writing a
function which generates curry functions for any given arity.

~~~{.JavaScript}
var ncurry = function(n) {
    return function(f) {
        var args = Array.prototype.slice.call(arguments, 1),
        
        accumulator = function() {
            var largs = args.concat(Array.prototype.slice.call(arguments, 0));
            
            if (largs.length < n) {
                largs.unshift(f);
                args = largs;
                return accumulator;
            } else {
                return f.apply(null, largs);
            }
        };
        
        return accumulator;
    };
};
~~~

For common use cases such as functions which accept 2 or 3 arguments, one can
write simple aliases using `ncurry`, while one can always use `ncurry` 'inline'
where necessary.

~~~{.JavaScript}
var curry  = ncurry(2),
    curry3 = ncurry(3);

// Presumably `f7` is a function which accepts 7 arguments
var fc7 = ncurry(7)(f7);
~~~

However, oftentimes something along the lines of `curryByLength` is preferable.
If the library of functions one is working with consists of a set of functions
with well-defined lists of parameters, then implicit rather than explicit
conversion can be more convenient and more natural; it is, after all, rather
nicer to be able to write `curry(f)` than `curry(f, n)` or even `ncurry(n)(f)`.

Ultimately which approach one decides to take must be based on understanding of
the properties of the functions one is working with. A choice of currying
function will then arise naturally---and after all, one can always use several.
Both of these approaches are [available in Udon][udon], my library for
functional programming in JavaScript, as `Udon.curry` and `Udon.ncurry`.


Further reading & prior art
---------------------------

* [Curried JavaScript functions][curriedjs] by Svend Tofte;
* [Partial Application in JavaScript][partialjs] by John Resig;
* [Cleaner Callbacks With Partial Application][callbacks] by Dan Webb;
* [Functional JavaScript][functionaljs], a library by Oliver Steele;
* [Currying][functional] (from the comp.lang.functional FAQ);
* [Currying Schönfinkelling][c2currying] (from the c2 wiki);
* [Currying in Haskell][curryinghaskell] (from the Haskell wiki);
* [Curried Scheme][curriedscheme] by Edward Kmett.

[currying]:        http://en.wikipedia.org/wiki/Currying
[jqbind]:          http://api.jquery.com/bind/
[udon]:            http://github.com/beastaugh/udon
[curriedjs]:       http://www.svendtofte.com/code/curried_javascript/
[partialjs]:       http://ejohn.org/blog/partial-functions-in-javascript/
[callbacks]:       http://www.danwebb.net/2006/11/3/from-the-archives-cleaner-callbacks-with-partial-application
[functionaljs]:    http://osteele.com/sources/javascript/functional/
[functional]:      http://www.cs.nott.ac.uk/~gmh/faq.html#currying
[c2currying]:      http://c2.com/cgi/wiki?CurryingSchonfinkelling
[curryinghaskell]: http://www.haskell.org/haskellwiki/Currying
[curriedscheme]:   http://comonad.com/reader/2009/curried-scheme/
