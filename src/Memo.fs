module Memo

open Browser.Dom
open Hacn.Types
open Fable.React

// This is mostly for underlying operations that have a simple PerformContinue 
// result, or PerformWait then PerformContinue process
let Memo<'returnType> wrappedOperation (changed: 'state -> bool) = 
  Operation ({ 
    Run = 
      fun setResult props -> 
        let stateHook = Hooks.useState None

        let hook _ =
          Hooks.useState None
          None

        let setMemoResult returnValue =
          stateHook.update (fun _ -> Some (returnValue))
        
        let runChanged () =
          let result = 
            match wrappedOperation with
            | Operation (opContents) -> opContents.Run setMemoResult props
            | _ -> failwith "Only works with Operation types"
          
          let (element, effect, layoutEffect) =
            match result with
            | OperationWait(contents) -> 
              (contents.Element, contents.Effect, contents.LayoutEffect)
            | OperationContinue (contents) -> 
              (contents.Element, contents.Effect, contents.LayoutEffect)
          
          OperationWait ({
            Element = element
            Effect = effect
            LayoutEffect = layoutEffect
            Hook = Some (hook)
          })

        match stateHook.current with
        | Some (state) when changed state -> 
          runChanged ()
        | Some (state) ->
          OperationContinue ({
            ReturnValue = state
            Element = None
            Effect = None
            LayoutEffect = None
            Hook = None
          })
        | None -> 
          runChanged ()
  })

let Once wrappedOperation = Memo wrappedOperation (fun _ -> true)