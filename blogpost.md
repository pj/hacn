# Introduction

Hacn is a dsl for creating react components using 

For people familiar with functional programming languages like Haskell and Scala you can think of it as an attempt to write a React monad, though it probably isn't technically a monad.

It draws inspiration from React Hooks, algebraic effects, Redux sagas and is similar in concept to crank js.

# Computation expressions

Hacn is implemented an f# language feature called computation expressions,  

For people familiar with other functional programming languages computation expressions perform a similar role to do notation in Haskell and 

Computation expressions subsume a wide variety of language features that in other languages are separate features, in F# computation expressions are used to handle async, generators, for comprehensions and LINQ.

# architecture

It's probably best to think of Hacn as a combination of async with the ability to goto a previous async statement.

While on the surface Hacn code looks like typical sequential/async code there 

Each operation has an associated 

## Dispose and state control

Every effect function can return a dispose function to clean up the effect

In general disposers are run every time 

# Operations

## Render and capture

## 

## Props

## State

The state operation looks very similar 

## Timeouts

# Future

This is my first time writing F#, so the code quality isn't that great and there's a bunch of stuff I'd like to improve.

Performance might be a problem, since the way it's designed leads to a lot of rerenders



- Get conditionals working properly
- Implement error handling
- Make internals type safe.