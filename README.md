# Introduction

Hacn is a DSL for creating React components using Fable, an F# to Javascript compiler and F# computation expressions. It's intended to make it easy to write complex interactive components without using callbacks.

If you're familiar with functional programming languages like Haskell and Scala you can think of it as an attempt to write a React monad, though it probably isn't technically a monad. It draws inspiration from React Hooks, algebraic effects, Redux sagas and is similar in concept to crank js.

It's written on top of react to make it possible to easily integrate with existing components and potentially to integrate into existing projects.

## Installation

To install into your F# project:

```
dotnet add package Hacn
```

## Usage

Hacn is written using F# computation expressions, if you're not familar with how they work it might be helpful to read a [tutorial](https://fsharpforfunandprofit.com/posts/concurrency-async-and-parallel/) on async programming since Hacn shares some of the same concepts.

To create a component you use the `react { ... }` expression builder syntax. Hacn components can be included in regular Fable React components:

```fsharp
module User

open Hacn.Core
open Hacn.Operations

let Element = 
  react {
    let! props = Props
    do! Render(
      Html.div [
        prop.text props.Message
      ]
    )
  }

let User =
  React.functionComponent(
    fun props -> 
      Html.div [
        Element {Message = "Hello"}
      ]
  ) 
```

`Props` and `Render` are operations and handle rendering, capturing results, props, state, timeouts and eventually things like fetches. They implement the `Perform` interface from the `Operation` typed union, see the section on writing operations below.

## Operations

### Props 

The `Props` operation handles react props, it restarts the sequence of operations from the point where `Props` is used when props changes. Another way of thinking about it is as a stream of changes to the props.

```fsharp
module Element

open Hacn.Core
open Hacn.Operations

type ElementProps = [
  Message: string
]

let Element = 
  react {
    let! props = Props
  }
```

Props uses a shallow comparison to compare fields, you can perform your own comparison by implementing an operation that uses the `PerformProps` interface. See the section on writing operations below.

### Rendering and Capturing Values.

Hacn uses a library called Feliz for html, basic rendering is handled using the `Render` operation:

```fsharp
module Element

open Hacn.Core
open Hacn.Operations

let Element = 
  react {
    do! Render(
      Html.div [
        prop.text "Hello World!"
      ]
    )
  }
```

It's also possible to capture dom events and "return" them from the `RenderCapture` operation into the sequence of events:

```fsharp
module Element

open Hacn.Core
open Hacn.Operations

let Element = 
  react {
    let! newValue = RenderCapture(
      fun capture -> 
        Html.div [
          prop.text value
          prop.children [
            Html.input [
              prop.onChange (
                fun (keyEvent: Browser.Types.Event) -> 
                  let inputElement = box keyEvent.target :?> HTMLInputElement
                  capture (inputElement.value)
                )
            ]
          ]
        ]
    )
    console.log newValue
  }
```

Hacn always rerenders the react element that was rendered, so after capturing the results the Html.input returning onChange events will be rendered again.

### State and capturing values

The `State` operation works similarly to the `useState` react hook, except it returns a function to create an operation which updates the state:

```fsharp
module Element

open Hacn.Core
open Hacn.Operations

let Element = 
  react {
    let! value, updateValue = State "Start"
    let! newValue = RenderCapture(
      fun capture -> 
        Html.div [
          prop.text value
          prop.children [
            Html.input [
              prop.onChange (
                fun (keyEvent: Browser.Types.Event) -> 
                  let inputElement = box keyEvent.target :?> HTMLInputElement
                  capture (inputElement.value)
                )
            ]
          ]
        ]
    )

    if newValue = "End" then
      do! updateValue "Done"
  }
```

NB: setting the state always triggers the sequence to restart from the point that state was used, regardless of whether the value was changed.

### Calling passed in functions

The `Call` operation calls

(Though this is actually 

### Writing operations

The interface for wrting operations is defined by the `Operation` typed union in `Hacn.Types`. The `Perform` record interface is the primary way to write operations that do things like call hooks and wrap functions like the `setTimeout` function. The `PerformProps` interface is used to create operations with custom props change detection logic.

All the other cases are used internally e.g. `End` marks the end of the sequence of operations and `Control` and `ControlProps` are used internally to make typechecking work.

Each operation has an associated state associated with it that is passed into the functions of the `Perform`. The operation state is currently typed as `obj` (basically untyped) so it has to be cast to the type you want it to be before using it.

The `Perform` type case takes a record that contains two functions, `PreProcess` and `GetResult`:

```fsharp
type PerformData<'props, 'returnType when 'props: equality> =
  { 
    PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> PerformResult<'props, 'returnType>;
  }
```

The `PreProcess` function is mainly for operations that wrap hooks and therefore need to be run every time in the same order a Hacn component renders. The function takes the current operation state if it exists and returns an updated state if something has changed. Returning a value causes the Hacn runtime to restart the sequence of operations at that point.

The `GetResult` method is the main 

As an example wrapping the `useRef` hook is defined as follows:

```fsharp
let Ref (initialValue: 'returnType option) =
  Perform({ 
    PreProcess = fun operationState -> 
      let currentRef = Hooks.useRef(initialValue)
      let castOperationState: 'returnType option = unbox operationState
      match castOperationState with
      | Some(_) -> None
      | None -> Some(currentRef :> obj)
    GetResult = fun _ operationState -> 
      let castOperationState: (('returnType option) IRefValue) option = unbox operationState
      match castOperationState with
      | Some(existingRef) -> 
        PerformContinue(
          {
            Element = None
            Effect = None
            LayoutEffect = None
          }, 
          existingRef
        )
      | None -> failwith "should not happen"
  })
```

Operations that 



## Dispose and state control

Every effect function can return a dispose function to clean up the effect after it completes

In general disposers are run every time 



## Roadmap 

- Fully document 
- Implement operations like data fetching.
- Implement operations that use type providers for things like graphql queries.
- Replace the `RenderCapture` operation with props that automatically capture, will require some kind of global state to set the rendering operation.
- Create state control wrapper operations e.g. Once, Memo, Retry etc.
- Implement Combine operation in builder correctly, so that conditionals work properly.
- Create builder that makes operations that can be composed.
- Implement error handling to allow try/catch and to allow returning error as value.
- Implement `for` and `while`.
- See if operation state can be made typesafe.

## Authors

