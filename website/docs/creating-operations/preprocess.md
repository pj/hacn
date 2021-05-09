---
sidebar_position: 3
---

# PreProcess

The `PreProcess` function of PerformData is called in the same order every time 

The `PreProcess` function is mainly for operations that wrap hooks and therefore need to be run every time in the same order a Hacn component renders. The function takes the current operation state if it exists and returns an updated state if something has changed. Returning a value causes the Hacn runtime to restart the sequence of operations at that point.

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