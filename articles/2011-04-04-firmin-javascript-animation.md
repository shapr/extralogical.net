---
title: Firmin, a JavaScript animation library
description: Firmin is a JavaScript animation library that uses CSS transforms and transitions to create smooth, hardware-accelerated animations. This article discusses the development of the library, and explains the underlying machinery of transforms and transitions.
---

[Firmin] is a JavaScript animation library that uses CSS transforms and
transitions to create smooth, hardware-accelerated animations.

Firmin is fundamentally very simple: all it does is parse descriptions of
animations and then execute them by manipulating the `style` property of the
animated element. Here's an example: moving an element right 200 pixels and
down 100 pixels in half a second, while rotating it clockwise through 45&deg;.

~~~{.JavaScript}
var elem = document.getElementById("an-element");

Firmin.animate(elem, {
    translate: {x: 200, y: 100},
    rotate:    45
}, 0.5);
~~~

Firmin was partly born out of a desire to write a lightweight, standalone
animation library which didn't rely on an existing DOM framework like
[jQuery] or [YUI]. The current version is still missing a number of features
and only works on a limited subset of modern browsers, but if you're developing
applications for WebKit-based browsers it's ready to be used in anger.

There's a reasonably complete set of [documentation][Firmin] available, which
introduces the framework and provides a fairly comprehensive set of [API docs].
If you just want to know how to use Firmin, that's the place to go.

For the remainder of this article I'm going to discuss using CSS transforms and
transitions to create animations, explain some of the problems I'm trying to
solve, and address a few of the issues I faced while developing Firmin. If you
you like what you see (or think I'm missing a trick somewhere), please
[fork the project on GitHub] or just [report an issue].

[Firmin]: /projects/firmin/
[jQuery]: http://jquery.com
[YUI]: http://developer.yahoo.com/yui/
[API docs]: /projects/firmin/api.html
[fork the project on GitHub]: https://github.com/beastaugh/firmin
[report an issue]: https://github.com/beastaugh/firmin/issues


Animating the browser
---------------------

Animation in HTML documents has always been the preserve of JavaScript, purely
because there was no other way to accomplish it. The [CSS Transitions Module]
is an attempt to alter that state of affairs, by providing native animation
primitives through CSS. One can write rules like the following to make an
element animate when its `opacity` property changes:

~~~{.CSS}
.animated {
    transition-property: opacity;
    transition-duration: 0.5s;
}
~~~

Since the animation will only run when the `opacity` property is changed,
JavaScript will still usually be needed to initiate the transition, although
there are some pseudo-class selectors like `:hover` which can trigger
transitions.

Furthermore, one has to write these rules in advance, and are few abstractions
available to make such code as flexible as one might like. While it might work
for small sites with limited functionality, this is not an approach likely to
scale to larger endeavours.

By modifying the DOM properties of the elements to be animated with JavaScript,
we can combine the advantages of speed and simplicity offered by the CSS
Transitions Module with the reusability and expressive power of a traditional
animation framework.

[CSS Transitions Module]: http://www.w3.org/TR/css3-transitions/


Creating animations with CSS Transitions
----------------------------------------

Animation hasn't tended to perform well in web browsers. This is partly because
of how it's had to be implemented, as a function which fires every hundredth of
a second or so to change the state of the animated element to the next 'frame'
of the animation. This is not easy for browser engines to optimise: a more
promising approach would be built-in primitives for animation which could then
be optimised at a lower level, closer to the hardware.

This is what the CSS transitions API promises, and when used in concert with
CSS transforms, a number of WebKit-based browsers (such as Safari on the Mac,
and Mobile Safari on the iPhone and iPad) will actually hardware accelerate the
animation. This results in noticeably smoother animations which don't slow down
other operations in the way that traditional animations do.

The animation of transform properties can be hardware accelerated because
modifying them doesn't force a [reflow] of the page; the same is true of the
`opacity` property, which is why these are the only properties whose animation
can currently be hardware accelerated. I used this trick on the [Paul Smith]
homepage to improve the performance of a full-page fade in browsers which
supported CSS transitions. We actually published the code in question under a
BSD license, so you can have a look at the [source code] if you're interested.

Transitions are just CSS properties, which hands an immense amount of power
directly to developers, who can start writing quite complex animation routines
with barely any JavaScript. One example I saw recently was this [fantastic
slideshow], which basically works by changing the `className` of the slides.
The minimal amount of code needed to get it off the ground is extremely
impressive, and a great demonstration of some of the possibilities which this
combination of transforms and transitions provides.

There are four transition properties, which I'll go through briefly before
explaining how they can be used to create animations. All the details can be
found in the W3C specification, [§2]. The first is the `transition-property`
property, which specifies those animatable CSS properties of the DOM element in
question which are to be animated.

~~~{.CSS}
.animated1 {
    /* Only the opacity and background-color properties will be animated. */
    transition-property: opacity, background-color;
}

.animated2 {
    /* All of the element's animatable properties will be animated. */
    transition-property: all;
}
~~~

Then there are the `transition-duration` and `transition-delay` properties,
which specify how long the animation should last, and how long to wait before
starting it.

~~~{.CSS}
.animated3 {
    /* Animations should last for 2 seconds. */
    transition-duration: 2s;
}

.animated4 {
    /* Animations should begin after 147 milliseconds. */
    transition-delay: 147ms;
}
~~~

Finally, there's the `transition-timing-function` property, which specifies a
function describing the evolution of the animatable properties over time. In
other words, they give the _rate of change_ of the animatable properties. Some
of them start slow and end fast; others are fast at the beginning and slow down
towards the end. Timing functions are specified using [cubic Bézier curves]:
there are five named functions one can use (`ease`, `linear`, `ease-in`,
`ease-out` and `ease-in-out`), as well as a `cubic-bezier` function that can
be used to create one's own timing functions by specifying four control points.

~~~{.CSS}
.animated5 {
    /* The ease-in-out function starts slow, speeds up, then slows back down
       towards the end of the animation. */
    transition-timing-function: ease-in-out;
}

.animated5 {
    /* The timing function specified here is actually equivalent to the
       ease-in function. */
    transition-timing-function: cubic-bezier(0.42, 0, 1.0, 1.0);
}
~~~

Of course, the downside to writing all one's animation code in CSS is that
everything must be specified in advance: nothing can be calculated at runtime.
Fortunately, there is a DOM interface to the way any element is styled: its
`style` property. Manipulating this property is how all existing JavaScript
animation frameworks work, and Firmin is no different. Let's consider a simple
example: making an element transparent ("fading it out"). Using Firmin, this is
done as follows:

~~~{.JavaScript}
Firmin.animate(elem, {
    opacity: 0
}, "1s");
~~~

All this does is give the element an opacity of 0, and set its
`transitionDuration` property to `"1s"`. Let's translate it into CSS to see how
it would work there, assuming we gave the element in question the `fade-out`
class at an appropriate moment.

~~~{.CSS}
.fade-out {
    opacity:             0;
    transition-duration: 1s;
}
~~~

I wrote at the beginning of this article that Firmin is fundamentally very
simple, and here we can see just how simple. In the library there's a method,
[Firmin.Animation#exec], which takes the properties to be animated and the
transition properties that will animate them, loops over them, and applies them
to the relevant element. Since it's so straightforward, I'm going to show the
entire function source, with a few comments added to explain what's going on.

~~~{.JavaScript}
Firmin.Animation.prototype.exec = function(element) {
    // An instance of the Animation prototype represents a single animation:
    // its style property carries any property, such as opacity, which is
    // neither a transition nor a transform. It's aliased as the properties
    // variable for convenience.
    var properties = this.style, property;
    
    // The properties object is modified by passing it to the transition and
    // transform builder methods, which basically just translate the internal
    // representations of those properties into something that can be copied
    // onto the DOM element being animated.
    if (this.transition) properties = this.transition.build(properties);
    if (this.transform)  properties = this.transform.build(properties);
    
    // Then this loop simply adds each specified property on the element in
    // question. This is, in the end, all that the execution of the animation
    // consists of.
    for (property in properties) {
        element.style[property] = properties[property];
    }
};
~~~

I'm going to discuss CSS Transforms in more detail later on, so for now I'll
just mention that they modify the shape and position of elements, and the
nature of this modification makes them well-suited to creating certain sorts of
animations. In particular, they give the web developer an arsenal of effects
that can be deployed to great effect in the creation of web applications which
retain much of the feel and responsiveness of native applications on mobile
devices, and as such also form a natural replacement for some Flash-based
sites.

One of the best things about [Ojay] is the simplicity of executing actions
sequentially: animate this element here, then hide it, then run this function,
then do this other thing, and so on. Firmin lets you chain animations in much
the same way, although it doesn't have Ojay's generality (in Ojay you can
change the object which methods are applied to partway through a chain of
method calls, and do all the other nifty things [MethodChain] supports).

~~~{.JavaScript}
Firmin.translateY(elem, 200, 1)
      .rotate(45, 0.75, function(el) {
          el.innerHTML = "Something something";
      })
      .animate({opacity: 0.1}, 0.5);
~~~

Two things are on show in this example: animation chaining, and callbacks.
Implementing them was very simple, but they add a lot of power to the library.
Animation chaining basically just involves building up a queue of animations
with each method call, then firing them in order. For the gory details, have a
look at [the source code]---it's all commented pretty thoroughly.

It's worth mentioning that animations are run on a timer: every time an
animation is run, the library calls `setTimeout` with a callback function that
will run the next animation (if there is one) once the first animation
finishes. Originally I added a listener to the animated element for the
`transitionend` event (see [§5] of the specification), but it's rather patchily
supported, and some people have reported bugs when using it, so in the end I
went with a simple timeout.

Every animation function and method that Firmin has accepts a callback function
as an optional last argument, so you can run arbitrary code once an animation
finishes. With this in mind, let's run through the example above, this time
with comments.

~~~{.JavaScript}
// First, the element is moved 200 pixels down, the animation taking 1 second.
Firmin.translateY(elem, 200, 1)
      // Then it's rotated through 45 degrees, taking 0.75 seconds.
      .rotate(45, 0.75, function(el) {
          // Once that animation finishes, the content of the element is set to
          // an arbitrary string.
          el.innerHTML = "Something something";
      })
      // Finally, it's faded to 10% opacity in 0.5 seconds.
      .animate({opacity: 0.1}, 0.5);
~~~

The important lesson here is that these actions are executed _sequentially_,
each animation only being run once the previous one is complete.

From everything discussed above, one might get the impression that transitions
are great, and a natural replacement for existing JavaScript animation
technology. I'm going to end this section with some reasons why this isn't the
case, at least not yet.

The obvious point is that they're not widely supported, and won't supplant the
current way of doing things until they are. This is fair enough, although I
will say that if you know you don't need to support older browsers---for
instance, if you're writing web applications targeted at iOS devices---then
this won't be a problem.

There are also some real technical limitations to transitions as an animation
mechanism, at least at present. Because the tick-by-tick modification of the
DOM elements involved is removed from the animation framework (since it's being
done by the browser, based on the transition properties given to the element),
certain sorts of fine-grained control of animations are no longer possible.

Firstly, one can't create a completely arbitrary timing function: only those
specifiable by four control points as a cubic Bézier curve are allowed.
Relatedly, one can't stop an animation halfway through. Doing this would
require direct access to the intermediate states of the animation---something
the transitions API doesn't give one.

In order to add something like [jQuery's `stop` method], one would have to
write a function to calculate all those intermediate stages, which would need
to produce exactly the same output as the browser's. Apart from being an
unnecessary headache, it's the sort of low-level work that misses the point of
_having_ these new animation primitives in the first place.

[reflow]: http://www.phpied.com/rendering-repaint-reflowrelayout-restyle/
[Paul Smith]: http://www.paulsmith.co.uk/
[source code]: https://github.com/othermedia/edgecase/blob/master/source/edge-gallery.js#L191
[fantastic slideshow]: http://hakim.se/inc/components/slideshow/
[§2]: http://www.w3.org/TR/css3-transitions/#transitions-
[cubic Bézier curves]: http://en.wikipedia.org/wiki/Bézier_curve#Cubic_B.C3.A9zier_curves
[Firmin.Animation#exec]: https://github.com/beastaugh/firmin/blob/1.0.0/src/firmin.js#L815-830
[Ojay]: http://ojay.othermedia.org/
[MethodChain]: http://jsclass.jcoglan.com/methodchain.html
[the source code]: https://github.com/beastaugh/firmin/blob/1.0.0/src/firmin.js
[§5]: http://www.w3.org/TR/css3-transitions/#transition-events-
[jQuery's `stop` method]: http://api.jquery.com/category/effects/


Working with CSS Transforms
---------------------------

Complementing the Transitions module are the CSS [2D] and [3D] Transforms
Modules. Until now the facilities available to transform the position and shape
of an element have been limited to changing the values of various aspects of
the box model: its height, width, margins, padding, and position relative to
some container.

The Transforms modules offer a range of pure CSS functions to rotate, skew,
scale and translate DOM elements, based on the extensive work done on the SVG
specification. Scaling an element to twice its initial width and rotating it
45&deg; around its top left-hand corner is simple:

~~~{.CSS}
.transformed {
    transform:        scaleX(2) rotate(45deg);
    transform-origin: 0 0;
}
~~~

Using a transform function creates a new local [coordinate system] for a given
element and its descendants. All transform functions (rotate, skew, translate,
scale etc.) are defined in terms of a [transformation matrix]. Firmin
translates each use of these API-level transform functions into an equivalent
matrix transformation and then concatenates them to determine the final value.
By performing these operations internally rather than deferring them to the
browser, it is possible to introduce stateful transforms, where each new state
of an element is based on its current state.

Both the CSS Transforms specifications include a requirement to expose an
implementation of an object following the [CSSMatrix interface]. This is
important for anyone wanting to write an animation library like Firmin, since
it provides a way of accomplishing two key tasks. Firstly, it exposes the
internal representation of the transformation state of a DOM element. This
enables library authors to deal with all transforms in a simple, unified way
rather than having to do messy hacking around with multiple transform
properties. Secondly, it provides a way to calculate the effect of combining
different transformation states. This is essential to developing support for
stateful transforms.

Thus far, only WebKit-based browsers such as Chrome and Safari expose a
transformation matrix class to the user, [WebKitCSSMatrix]. Firefox, despite
some limited support for 2D transforms, doesn't provide an implementation of
CSSMatrix. Neither does Opera: a CSSMatrix class was added in [Opera Presto
2.5], but it appears to have been removed from more recent versions of their
layout engine.

It's my hope that Firmin can eventually become a genuinely cross-browser
library, so in that spirit I've written [FirminCSSMatrix], an implmentation of
the CSSMatrix interface in JavaScript. Using this I'm hoping to work around the
limitations of Firefox and Opera's support for CSS transforms, and provide at
least some of Firmin's functionality in those browsers. It's based on WebKit's
matrix code, and supports both 2D and 3D transformations. However, it's still a
little buggy, so I've removed it from the 1.0 release until I have time to
resolve the remaining issues.

[2D]: http://www.w3.org/TR/css3-2d-transforms/
[3D]: http://www.w3.org/TR/css3-3d-transforms/
[coordinate system]: http://www.w3.org/TR/SVG/coords.html
[transformation matrix]: http://www.mathamazement.com/Lessons/Pre-Calculus/08_Matrices-and-Determinants/coordinate-transformation-matrices.html
[CSSMatrix interface]: http://www.w3.org/TR/css3-3d-transforms/#cssmatrix-interface
[WebKitCSSMatrix]: http://developer.apple.com/safari/library/documentation/AudioVideo/Reference/WebKitCSSMatrixClassReference/WebKitCSSMatrix/WebKitCSSMatrix.html
[Opera Presto 2.5]: http://www.opera.com/docs/specs/presto25/css/transforms/#css
[FirminCSSMatrix]: https://github.com/beastaugh/firmin/blob/8c14d35c11566053e8baf1b8178a15f9236ffafe/src/matrix.js


Stateful transforms
-------------------

When the `transform` property of a DOM element is updated, the new transform
has no relation to the old one. In a sense this is intuitive---one CSS property
is simply being changed to another. However, for many of the things one might
want to do with Firmin, it makes less sense. Think about moving a DOM element
around with the `translate` functions. To move an element 400 pixels to
the right, call `translateX` and pass in the element and the argument `400`.

~~~{.JavaScript}
Firmin.translateX(el, 400, 1);
~~~

Then call `translateY` and pass in `200` to move it down 200 pixels. So far, so
good.

~~~{.JavaScript}
Firmin.translateY(el, 200, 1);
~~~

Now consider moving it back to its starting point. We've come 400 pixels along
and 200 down, so we should move it 400 to the left and 200 up. But since
transforms are carried out relative not to their _current_ transformation state
but to their _initial_ transformation state, we can't just call `translateX`
`translateY` again with `-400` and `-200` as the arguments: it would end up
moving the element 400 pixels to the _left_ and 200 pixels _above_ its original
position. Instead, we write this:

~~~{.JavaScript}
Firmin.translate(el, {
    x: 0,
    y: 0
}, 1);
~~~

Clearly under many circumstances this won't be a problem, but working in an
absolute coordinate system when a relative one is more appropriate is liable to
lead to unnecessary confusion, and there will be circumstances where keeping
track of an element's position will introduce unnecessary complexity into the
code.

To overcome this annoyance, Firmin comes with support for _stateful
transforms_. The chaining object returned by all animation methods actually
stores the previous transforms, so any new transforms applied to the element
being animated can be based on its current transformation matrix. This lets us
rewrite the above code to better accord with our intuitions for this situation.

~~~{.JavaScript}
// Move the element right 400px and bind the anim variable to the current
// transformation state.
var anim = Firmin.translateX(el, 400, 1);

// Move the element down 200px relative to its current position.
anim.translateYR(200, 1);

// Move the element back to its origin.
anim.translateR({x: -400, y: -200}, 1);
~~~

Note the **R** suffixes: these distinguish the _relative transform functions_
from their absolute counterparts. Every normal transform function, which
transforms an element relative to its initial position before any transforms
are applied, has a relative version which transforms the element relative to
its current transformation state.

Of course, the example above can be rewritten in a chained style.

~~~{.JavaScript}
Firmin.translateX(el, 400, 1)
      .translateYR(200, 1)
      .translateR({x: -400, y: -200}, 1);
~~~


3D transforms
-------------

3D transforms are also not yet widely supported---like the CSSMatrix interface,
they're only available in WebKit-based browsers like Chrome and Safari.
Since Firmin itself is currently only targeted at those platforms, it already
has quite a lot of 3D transform support. In particular, it includes all the 3D
transform methods (with one omission, discussed below), so elements can be
translated, scaled and rotated in three dimensions.

I won't spend a lot of time explaining 3D transforms, since there are plenty of
articles out there which introduce them in a more comprehensive way than I can
manage here. Probably the best place to start is the [WebKit blog post] which
first introduced them, while the [Apple developer documentation] is also good,
and includes some helpful diagrams. [This Art of Web article] might be helpful,
and I have a longer list of [related articles] saved on Pinboard.

The major missing piece in Firmin's support of 3D transformations is an
interface for manipulating the `perspective` and `perspective-origin`
properties, which control the "camera position" for the whole page, and the
`perspective` transformation function, which modifies that perspective for a
single element. These are vital to a really effective use of 3D transforms, so
once I figure out a nice way to introduce them and explain their use, they will
be added to Firmin. Good documentation is always important, but when the
subject matter is slightly obscure and in all likelihood outside most
developers' experience, it's even more vital.

Apart from full support for 3D transforms, and potentially adding some more
convenience functions, the one major thing Firmin is missing is some great
demos. Nothing communicates the power of a library or piece of functionality
as well as a demonstration of what can be accomplished with it. Unfortunately,
this is not a task I have time for at the moment, but if you build something
cool, [do get in touch](mailto:benedict@eastaugh.net).

[WebKit blog post]: http://www.webkit.org/blog/386/3d-transforms/
[Apple developer documentation]: http://developer.apple.com/library/safari/#documentation/InternetWeb/Conceptual/SafariVisualEffectsProgGuide/Transforms/Transforms.html
[This Art of Web article]: http://www.the-art-of-web.com/css/3d-transforms/
[related articles]: http://pinboard.in/u:beastaugh/t:csstransforms/
