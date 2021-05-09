---
sidebar_position: 1
---

# How it works

Hacn is a library designed to make it easy to write complex components without lots of callbacks and complex dependencies between hooks.

It's written using Fable, a javascript to F# compiler and makes extensive use of computation expression syntax. Computation expressions are used for a variety of things in F# primarily async, see `` for a tutorial.

A quick example is:

```fsharp 
let! count = Counter props.rate
do! Render Html.div [ prop.text (sprintf "Current count: %d" count) ] 
``` 

which is roughly equivalent to the await statement in typescript:

```typescript
const count = await Counter props.rate
await Render Html.div [ prop.text (sprintf "Current count: %d" count) ]  
```

Hacn code is composed of sequences of the `Operation`, which works somewhat like `Promise`, but with a wider variety of uses and features.

Unlike Promises, Hacn Operations aren't simply run once to completion and then return a value. Instead Operations can signal to the Hacn runtime that something has changed and cause Hacn to re-execute code after the changed operation.

To prevent execution jumping between earlier and later stages, Operations also support automatic disposal e.g. running web requests are cancelled or timers are stopped when execution starts from an earlier point in the flow.

The Feliz library is used for rendering html with some additions to capture dom events and return them as a result from the render Operation.

## Example

To illustrate these features it's helpful to see an example:

```fsharp
type ShowCountProps = { rate: int }

let ShowCount : ShowCountProps -> ReactElement =
  react {
    let! props = Props

    if props.rate <= 0 then
      do! Render Html.div [ prop.text "Pausing Count" ]

    printf "Changing rate: %d" props.rate
    let! count = Counter props.rate
    printf "Count: %d" count
    do! Render Html.div [ prop.text (sprintf "Current count: %d" count) ]
  }

let CountRate : unit -> ReactElement =
  react {
    let! (rate, setRate) = State 1000

    let! newRate =
      Render
        Html.div
        [ prop.children [ Html.div [ prop.text (sprintf "Current rate %d" rate) ]
                          Html.button [ prop.text "Stop"
                                        prop.value "Stop"
                                        prop.captureClick 0 ]
                          Html.button [ prop.value "100"
                                        prop.text "Rate: 100"
                                        prop.captureClick 100 ]
                          Html.button [ prop.value "1000"
                                        prop.text "Rate: 1000"
                                        prop.captureClick 1000 ]
                          ShowCount { rate = rate } ] ]

    do! setRate newRate
  }
```

import {CountRate} from '../../dist/examples/Examples.js'

<blockquote>
  <CountRate />
</blockquote>

The `ShowCount` component demonstrates the re-execution feature - `Props` is an operation that re-executes the rest of the code when the props passed into the element. The `Counter` operation shows both the re-execution of the rest of the steps and disposal, `Counter` resets it's internal count when the rate changes.

It also shows how `if` statements can be used to optionally render things based on things like props.

The `CountRate` component shows returning values from a `Render` operation, the prop.captureClick handles the click event and returns the given value back into the operation flow and assigns it to the `newRate` variable. 

The `State` operation is like the `useState` hook, except to set it returns an operation `setRate`. It also demonstrates re-execution, when `setRate` is used it causes execution to start again after the `State` operation.

## Desugared

Computation expressions are basically syntax sugar for nested function calls, to help understand what Hacn is doing above it might be useful to see it's desugared form:

```fsharp

let ShowCountDesugar : ShowCountProps -> ReactElement =
  react.Run(
    react.Delay
      (fun () ->
        react.Bind(
          Props,
          (fun props ->

            react.Combine(
              react.Delay
                (fun () ->
                  if props.rate <= 0 then
                    Render Html.div [ prop.text "Pausing Count" ]
                  else
                    react.Zero()),
              react.Delay
                (fun () ->
                  printf "Changing rate: %d" props.rate

                  react.Bind(
                    Counter props.rate,
                    fun count ->
                      printf "Count: %d" count

                      react.Bind(
                        Render Html.div [ prop.text (sprintf "Current count: %d" count) ],
                        (fun () -> react.Zero())
                      )
                  ))
            ))
        ))
  )

let CountRateDesugar : unit -> ReactElement =
  react.Run(
    react.Delay
      (fun () ->
        react.Bind(
          State 1000,
          fun (rate, setRate) ->
            react.Bind(
              Render
                Html.div
                [ prop.children [ Html.div [ prop.text (sprintf "Current rate %d" rate) ]
                                  Html.button [ prop.value "Stop"
                                                prop.captureClick 0 ]
                                  Html.button [ prop.value "100"
                                                prop.captureClick 100 ]
                                  Html.button [ prop.value "1000"
                                                prop.captureClick 1000 ]
                                  ShowCount { rate = rate } ] ],
              fun newRate -> react.Bind(setRate newRate, (fun () -> react.Zero()))
            )
        ))
  )
```