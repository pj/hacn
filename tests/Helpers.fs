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
    Run = fun _ ->
      callCount := !callCount + 1
      let effectFunc setResult =
        internalRerender <- Some(setResult)
      
      OperationWait ({
        Element = None
        Effect = Some(effectFunc, None)
        LayoutEffect = None
        Hook = None
      })
  })

  let rerenderTrigger (value: 'result) =
    match internalRerender with
    | Some(rerender) -> rerender(value)
    | None -> failwith "internalRerender should have been set"
  
  rerenderTrigger, callCount, operation