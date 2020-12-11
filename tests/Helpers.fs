module Helpers

open Hacn.Core
open Hacn.Operations
open Fable.ReactTestingLibrary
open Feliz
open Hacn.Types
open Fable.Mocha
open Browser.Types

let testOperationWithTrigger<'result> () =
  let mutable internalRerender = None
  let callCount = ref 0
  let operation = Perform({
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      callCount := !callCount + 1
      let effectFunc rerender =
        internalRerender <- Some(rerender)
        Some(fun _ -> 
          None)
      match operationState with
      | None -> 
        PerformWait(
          {
            Element = None
            Effect = Some(effectFunc)
            LayoutEffect = None
            OperationState = None
          }
        )
      | Some(result) -> 
        let castResult: 'result = unbox result
        PerformContinue(
          {
            Element = None
            Effect = None
            LayoutEffect = None
            OperationState = None
          },
          castResult
        )
  })

  let rerenderTrigger (value: 'result) =
    match internalRerender with
    | Some(rerender) -> rerender(fun _ -> Some(value :> obj))
    | None -> failwith "Should not happen"
  
  rerenderTrigger, callCount, operation