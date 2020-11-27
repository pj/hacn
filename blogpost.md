# Introduction

Hacn is a dsl for creating React components using Fable, an F# to Javascript compiler.

For people familiar with functional programming languages like Haskell and Scala you can think of it as an attempt to write a React monad, though it probably isn't technically a monad.

It draws inspiration from React Hooks, algebraic effects, Redux sagas and is similar in concept to crank js.

# Computation expressions

Hacn is implemented an f# language feature called computation expressions and subsume a wide variety of language features that in other languages are separate features. In F# computation expressions are used to handle async, generators, for comprehensions and LINQ.

For comparisons sake 

For people familiar with other functional programming languages computation expressions perform a similar role to do notation in Haskell and for comprehensions in scala.

# Architecture

Although Hacn can do similar things to async e.g. waiting on things like timeouts and http requests to complete



e.g. props or state changing will cause the entire sequence of operations and code to rerun

It's probably best to think of Hacn as a combination of async with the ability to goto a previous async statement.

While on the surface Hacn code looks like typical sequential/async code there 

Each operation has a state associated with it thats passed into 

Hacn rerenders

## Example

Let's go through a more complicated example to show some of the features



## Dispose and state control

Every effect function can return a dispose function to clean up the effect after it completes

In general disposers are run every time 

# Operations

## Render and capture

## 

## Props

## State

The state operation works very similarly to the useState hook, except it 

## Timeouts

## Wait2



# Future

This is my first time writing F#, so the code quality isn't that great and there's a bunch of stuff I'd like to improve.

Performance might be a problem, since the way it's designed leads to a lot of rerenders and capturing results requires a rerender to process the result.

- Get conditionals working properly.
- Implement error handling.
- Make operation state type safe.