module Helpers

open Hacn.Core
open Hacn.Operations
open Fable.ReactTestingLibrary
open Feliz
open Hacn.Types
open Fable.Mocha
open Browser.Types
open Browser.Dom

let testOperationWithTrigger<'result> () =
  let mutable internalRerender = None
  let callCount = ref 0
  let operation = Operation({
    Run = fun setResult props ->
      callCount := !callCount + 1
      let effectFunc () =
        internalRerender <- Some(capture)
        Some(fun _ -> Erase)
      // match operationState with
      // | None -> 
      //   PerformWait(
      //     {
      //       Element = None
      //       Effect = Some(effectFunc)
      //       LayoutEffect = None
      //       OperationState = Keep
      //     }
      //   )
      // | Some(result) -> 
      //   let castResult: 'result = unbox result
      //   PerformContinue(
      //     {
      //       Element = None
      //       Effect = None
      //       LayoutEffect = None
      //       OperationState = Keep
      //     },
      //     castResult
      //   )
  })

  let rerenderTrigger (value: 'result) =
    match internalRerender with
    | Some(rerender) -> rerender(fun _ -> Replace(value :> obj))
    | None -> failwith "internalRerender should have been set"
  
  rerenderTrigger, callCount, operation