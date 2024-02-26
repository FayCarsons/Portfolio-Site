Date: February 20, 2024

# Developing a functional creative coding library in OCaml

I have taken a unique path during my short time as a developer. Beginning with
Clojure, with which I mostly wrote [Shadertoy](https://shadertoy.com) style shader
sketches, then moving onto performance-focused image processing apps in Rust,
and now MLs: I have felt lost, confounded, or otherwise confused at many points.

The resources for absolute beginners in these spaces are few and far between.
It's generally accepted that these are 'hard' languages (which I don't agree
with) and that people who are brand new to computer science should start with
imperative languages like Python or Javascript. This has always been frustrating
to me, as I feel it is likely responsible for these languages achieving ubiquity
while functional languages, for the most part, have not.

When I was offered the opportunity to intern for [Tarides](https://tarides.com), building a creative 
coding library that would be designed with computer science education in mind, 
I was very excited to explore this.

## Joy

That library is [Joy](https://github.com/Sudha247/ocaml-joy), inspired by the [Python library](https://github.com/fossunited/joy) of the same name. An SVG
rendering library with a focus on simplicity, composability, and elegance.
While the original Python library had a functional style to it, translating it
to OCaml gave me a unique opportunity to demonstrate the degree to which FP
suits generative art.

There was only bare-bone scaffolding in place when I joined the team. A few
geometric types, a `shape` variant type for polymorphism, and a simple
`Stdlib.Graphics` backend. This period of development offered some challenges.

The `Graphics` backend used the [X window system](https://en.wikipedia.org/wiki/X_Window_System),
which had known vulnerabilities (with some being 20+ years old!), had an awkward
interface (resolution had to be passed as a string??), and, most bothersome of all,
the entire window process had to be started and then killed and restarted every time
you wanted to change your sketch and view the new output. This made iteration
awkward and much more time-consuming than it needed to be.

Regardless, I quickly began working through the roadmap adding new primitives,
transformations like `rotate`, and essential features like variable canvas size
and the ability to render the axes for debugging. The recursion-heavy
style that OCaml steers you towards was, initially, disorienting for me. The
lack of utilities like Clojure's `range`, `take`, and `comp` was confusing and
I did have to lean on ChatGPT to give me a starting point to work from on more
than one occasion.

I trudged on, putting in plenty of PRs and generally enjoying a level of productivity
I found very satisfying. I would have discussions with my roommate, a Clojure dev,
"Can you believe OCaml doesn't have *feature*!? Why would you do *x* in *y* way??".
This continued for maybe the first month of my internship until one day, I
stumbled upon [99 OCaml Problems](https://v2.ocaml.org/learn/tutorials/99problems.html) and everything clicked for me.
The terse, tail-call optimized recursive style sort of revealed itself and I felt like I understood it all.

By the time we had cemented the final API and the library had the ability to
handle more complex algorithms like [circle packing](https://en.wikipedia.org/wiki/Circle_packing) and [flow-fields](https://en.wikipedia.org/wiki/Vector_field), I noticed
something else I found interesting, there was *no mutability* in the entire library. 
Aside from some behind-the-scenes state management that (I assume) was taking place in
the `Graphics` module, which we hadn't touched, there were no `ref` types, or
internally mutable data structures, just pure functions mapping from one
primitive to another.

This realization led me to a couple of conclusions: that functional programming
is particularly well suited to generative art, and that it could be an excellent
way to introduce computer science students to the paradigm. Most generative art
algorithms are a pure mapping from either a set of static constants, a random 
seed, or a user input like mouse position, to a list of primitives that get 
passed on to the rendering engine. While many may use mutability in realizing 
their art, it is very rarely *absolutely necessary*.

More importantly, more so than almost any area of computer science, generative 
art is immediate. A beginner can write a simple program, a circle made of circles, 
a spiral of colors, and get a result they can put on the fridge, so to speak, 
right away. This is crucial to ensuring that would-be developers form some
self-motivation and momentum, there is very little that keeps you in that
positive feedback loop like having a pretty picture to show off to your friends.
Doubly so for children. Free from the complexity of web or game development, the
confounding "undefined is not a function" errors of dynamic languages, and the
intimidating syntax of lisps or Rust, students can focus on learning algorithms.

With this realization I went on to develop the rest of the library, consistently 
finding satisfyingly elegant implementations or optimizations. Transformations 
are a 6 case match block, one for every shape, that returns a shape. Colors
could be handled by adding an optional tuple field to our color types, and 
rendering them could be optimized with `Option.iter`. The function that adds a 
`stroke` or `fill` color to a shape is a pure function that takes a color and 
shape, and returns a shape, not imperatively setting attributes of an object. 

Writing the circle packing example, circle collision checking could be short 
circuited by using a fold-like recursive function as opposed to the `reduce` or 
`fold` I had initially used (both in my first OCaml draft and Clojure). The 
quadtree example was made very simple with OCaml's variant types, something I 
had no experience with prior, and am now in love with. Writing tree algorithms in 
Clojure is a nightmare of `NullPointerException`, and a constant fight with the 
borrow checker in Rust. OCaml turned that into a single match block of 
`Node | Leaf`. The flowfield example is a mapping from an array of smooth noise 
values to an array of lines.

Mutability was necessary in one place: handling the rendering context. Even in 
this case, a global mutable singleton, OCaml provided an elegant and, more 
importantly, safe solution with `option`. Either the context has been initialized
and operations like rendering or fetching the canvas dimensions could be performed 
or it hadn't and an error is raised. There's no need to touch the inner value, 
compare it to a null literal, or do any guesswork in this regard. `Option.is_some`
handles that for you. When writing the HTML canvas backend this was an absolute 
godsend. I think of my early days of learning to code with `Quil`, a ClojureScript
wrapper for p5.js, and how much frustration this would have saved me.

Coming into this project with no experience in OCaml or MLs as a whole, I wasn't 
sure what to expect, but in retrospect I am very much grateful that I was able 
to do this work in OCaml specifically. I look back at much of my self-education
and remember all the frustration dynamic languages have given me, the unhelpful 
errors and runtime crashes, as well as the mind-twisting frustration that learning
Rust caused me, and the conclusion I come to is this. Functional programming is 
good, doubly so if its typed, and if we want new programmers to benefit from that 
good then I think OCaml is our best bet. Additionally, that immediacy that is so 
necessary for newcomers can be provided by creative coding. And luckily, FP suits 
creative coding to a truly impressive degree. So it follows that the right OCaml 
creative coding library could be an invaluable tool for teaching FP to a new
generation of programmers. This is my goal with `Joy`.
