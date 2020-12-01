# Introduction

Hacn is a DSL for creating React components using Fable, an F# to Javascript compiler and F# computation expressions.

For people familiar with functional programming languages like Haskell and Scala you can think of it as an attempt to write a React monad, though it probably isn't technically a monad.

It draws inspiration from React Hooks, algebraic effects, Redux sagas and is similar in concept to crank js.

It's written on top of react to make it possible to easily integrate with existing components and potentially to integrate into existing projects.

# Computation expressions

To understand how this works it's useful to understand a bit about computation expressions, an F# feature that is used to implement things such as async, generators, for comprehenshions and LINQ. A detailed tutorial is here.

For people familiar with other functional programming languages computation expressions perform a similar role to do notation in Haskell and for comprehensions in scala.

You can think of computation expressions as being similar to async/await in javascript and other languages. For example the following is some async code in F# and node js:

```fsharp
let printTotalFileBytes path =
    async {
        let! bytes = File.ReadAllBytesAsync(path) |> Async.AwaitTask
        let fileName = Path.GetFileName(path)
        printfn "File %s has %d bytes" fileName bytes.Length
    }
```

Node js:

```javascript
async function printTotalFileBytes(path) {
  const bytes = await fs.promises.readFile(path);
  const filename = path.basename(path);
  constole.log(`File ${fileName} has ${bytes.length}`);
}
```

The `async { //... }` block syntax declares an asynchronous computation, similar to how the `async` keyword makes a function asynchronous.

The `let! x = ...` syntax is similar to `const x = await ...`, note the exclamation mark which roughly speaking does the same thing that the await keyword does.

Hacn 

Infa

For comparisons sake the `let! x = ...` syntax is similar to the await operation in javascript `const x = await`


(`let y = ...` is the fsharp syntax for assignment, similar to how it works in javascript)


The `do! ...` is for operations that don't return unit i.e. that don't return a value. 

Hacn is implemented in an f# language feature called computation expressions which subsume a wide variety of language features that in other languages are implemented as separate constructs. In F# computation expressions are used to handle async, generators, for comprehensions and LINQ.



# Example

As 

Finally to return a value from the flow of operations you can use the Call Operation


Regular function components can be mixed in with 

and it should be possible to mix 

## Example

Let's go through a more complicated example to show some of the features

Refs work like the useRef hook in react.

Rendering an element doesn't only


# Architecture

Although Hacn can do similar things to async e.g. waiting on things like timeouts and http requests to complete, it has a number of differences.

First of all it can rerun parts of the code if an operation signals that something has changed e.g. props changing or a fetch completing will cause the sequence of operations after the signalling operation to rerun. This is basically like goto or an async operation with a "rewind" feature. 

For example 

````
type ElementProps = 
  {
    Message: string
  }

let Element = 
  react {
    let! props = Props
    do! Render(
      Html.div [
        prop.text props.Message
      ]
    )
  }
````

A component 

Every time props changes 

Rerunning from an operation is triggered a number of ways:

1. Props is largely built into 

Secondly, each operation has a state associated with it that's passed when getting the result

in most cases this state is disposed after

Ever



It's probably best to think of Hacn as a combination of async with the ability to goto a previous async statement.

While on the surface Hacn code looks like typical sequential/async code there 

Each operation has a state associated with it thats passed into 

Hacn rerenders


Elements are rendered as a side effect of running operations, rather than being returned from the 



# Writing operations

The interface for wrting operations is defined by the `Operation` type in `Hacn.Types`, 
The `Control` and `ControlProps`
The primary way to write an option is to implement the Perform record interface.

The GetResult 

The PreProcess function is mainly for operations that wrap hooks and therefore need to be run every time 

Operations that 



## Dispose and state control

Every effect function can return a dispose function to clean up the effect after it completes

In general disposers are run every time 

# Operations

## Render and capture

## Props

## State

The state operation works very similarly to the useState hook, except it returns an operation that can be used 


# Future

This is my first time writing F#, so the code quality isn't that great and there's a bunch of stuff I'd like to improve.

Performance might be a problem, since the way it's designed leads to a lot of rerenders and capturing results requires a rerender to process the result.

- Get conditionals working properly.
- Implement error handling.
- Make operation state type safe.

# Notes

The builder returns a `ReactElement` rather than an `Operation`, unlike other computation expressions, which usually return 