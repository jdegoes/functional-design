# Functional Design

## Getting Started

```
sbt
compile
```

You can open the project from Visual Studio Code with Metals and it should build without issue.

## About Functional Design

Although functional programming theory is useful, most day-to-day functional programming does not require any category theory or even any type classes. Most problems can benefit from a domain-specific model, constructed using immutable data types and operators. Such functionally-oriented solutions are simple, composable, type-safe, and testable.

In this course, developers will learn how to write simple functional solutions to everyday business problems, without jargon and without type classes. Developers will learn how to construct type-safe and composable solutions to domain-specific problems, and how the single responsibility principle of object-oriented programming translates into orthogonality. When developers leave, they’ll have newfound ability to directly benefit from functional programming techniques across their whole application, in a way that’s highly accessible to the whole team and new hires.

* Functional domain modeling
* The essence of composability
* Measuring the orthogonality of operators
* Achieving both minimalism and expressiveness
* Using types to enforce business constraints
* Generalized algebraic data types for advanced modeling
* Principle of least power applied to functional design
* Design techniques used in the ZIO library and broader ecosystem
* Functional approaches to internal domain-specific languages
* Translating between different domain-specific language