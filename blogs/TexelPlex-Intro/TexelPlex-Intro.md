--- 
title: Introducing TexelPlex
date: 06-18-2025
tags: Design, TexelPlex, Graphics, Shaders, Haskell, Effect, FounderMode
---

Shaders were my introduction to programming. My first non-trivial programs were written in ClojureScript using a [library](https://github.com/Ella-Hoeppner/hollow) which compiled data structures to GLSL at runtime and handled all the boilerplate. With this came a lot of quality of life improvements over raw GLSL, macros which create blur kernels, arithmetic functions with arbitrary arity, the ability to splice acceleration structures produced by Clojure code into GLSL. It was a dramatically more pleasant experience than writing WebGL, even with libraries like TWGL abstracting away much of the complexity.

It was a wonderful way to learn how to code. High-level and quick to generate a tangible product you can "put on the fridge" so-to-speak, but with the option to drop down into the fine details when needed. The work I created with this framework was featured in gallery shows in London and the US, published in generative art publications like Right Click Save, and led to my first meaningful contributions to open source as a co-author of the library itself.

# The Creative Coding landscape 

Most creative coding tools fall short for modern developers. [ShaderToy](https://www.shadertoy.com/), while foundational and impactful to a degree one can only aspire to, hasn't seen meaningful updates in years. Its interface feels dated, sharing workflows are clunky or non-existent, conventions restrictive, and lacking many of the features modern developers have come to expect. 

Other alternatives have their own limitations, [p5.js](https://p5js.org/) is opinionated and lacks the ability to drop down to a lower-level without adopting an awkward set of primitives. [Max MSP](https://cycling74.com/) has other issues, many of which are inherent to visual editors, which make it difficult to approach for beginners and awkward to use for the experienced. Other libraries and platforms lack the immediacy that I want in my tools. The time from idea to pixels on the screen should be minimized, and without forcing you into paradigms which feel orthogonal to what you're trying to achieve.

# Why this matters

Shaders are the ideal introduction to computer science concepts. Immediate visual feedback, non-trivial signal flow, performance optimization, and a tangible result you can show off on social media. A lot of current tools have the right idea in this regard, but none execute it in a way that feels natural and intuitive to me.

For seasoned graphics programmers this shows up as friction. Friction in expressing ideas, in discussing them with others, and in sharing their final product with the world. Something as deeply necessary as graphics (which, every application is presented to you via shaders one way or another) should not be a second class citizen in the world of computer science.

# Introducing TexelPlex

TexelPlex is my solution to these problems. I'm building a modern shader development platform with the core principles:

- Fast and intuitive workflows make for better programs
- Art is meant to be shared - extensive social features are a must
- Writing code is fun! and tooling should not stand in the way of that
  
Our initial release will include: 

- **Zero boilerplate** minimal time to realize an idea 
- **Intuitive configuration** with sensible defaults and the ability to override for power users 
- **Multi-language support** from GLSL to experimental new languages - we plan to support them all
- **Custom uniforms** with drag-and-drop GUI generation 
- **One-click sharing** to social platforms with embedded previews 
- **Core functionality is free** paid features will be convenience upgrades, not requirements

Future releases will add the ability to export shaders as standalone apps, JavaScript support for uniform management, more shading languages, extended social features, as well as refinements that beta users work with us to design. 

# What's next

We're currently still in development, but we plan to have a private beta out in the coming weeks, with a public beta following three months after that. 

I'll be documenting the development process here on my blog. You'll see how we're solving technical challenges like shader transpilation, designing intuitive interfaces, managing state on the frontend in novel ways, and building social features that enable creators to connect and share knowledge. 

If you're interested in graphics, generative art, functional programming in production, or curious about this platform, follow along as we share our progress! There are a lot of interesting, novel opportunities here and I'm excited to share them.