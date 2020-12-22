module Memo

open Browser.Dom
open Hacn.Types

type MemoStatus =
| Waiting
| Rerendered
| Complete

type MemoState<'returnType> = {
  UnderlyingState: obj option
  ReturnedValue: 'returnType
  Status: MemoStatus
}

// This is mostly for underlying operations that have a simple PerformContinue 
// result, or PerformWait then PerformContinue process
let Memo<'returnType> wrappedOperation (changed: (obj option -> bool) option) = 
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun capture operationState -> 
      let wrapUnderlyingEffect underlyingEffectOpt rerender =
        let wrapRerender stateUpdater =
          rerender (fun rerenderState ->
            let castUnderlyingState: MemoState<'returnType> = unbox rerenderState
            let underlyingState = stateUpdater castUnderlyingState.UnderlyingState
            match underlyingState with
            | Replace(state) ->
              Replace ({
                castUnderlyingState with 
                  UnderlyingState = Some(state); Status = Rerendered
              } :> obj)
            | Erase -> 
              Replace ({
                castUnderlyingState with 
                  UnderlyingState = None; Status = Rerendered
              } :> obj)
            | Keep -> Keep
          ) 
        
        match underlyingEffectOpt with
        | Some(underlyingEffect) ->
          underlyingEffect wrapRerender |> ignore
          None
        | None -> 
          None
      
      let runUnderlying underlyingOperationState = 
        match wrappedOperation with
        | Perform({GetResult = getResult}) ->
          let response = getResult capture underlyingOperationState
          match response with 
          | PerformContinue(operationData, returnValue) -> 
            PerformContinue(
              { 
                operationData with
                  OperationState = 
                    Replace({
                      UnderlyingState = underlyingOperationState
                      ReturnedValue = returnValue
                      Status = Complete
                    } :> obj)
              },
              returnValue
            )
          | PerformWait(operationData) -> 
            PerformWait(
              { 
                operationData with 
                  Effect = Some(wrapUnderlyingEffect operationData.Effect)
                  OperationState = 
                    Replace({
                      UnderlyingState = underlyingOperationState
                      ReturnedValue = null
                      Status = Waiting
                    } :> obj)
              }
            )
        | _ -> failwith "Underlying operation must be Perform"
      
      match operationState with
      | None ->
        runUnderlying None
      | Some(underlyingState) ->
        let castUnderlyingState: MemoState<'returnType> = unbox underlyingState
        match castUnderlyingState.Status with
        | Complete ->
          // Rerun the operation with reset state.
          if changed.IsSome && (changed.Value castUnderlyingState.UnderlyingState) then
            runUnderlying None
          else
            // Continue with the cached value.
            PerformContinue(
              {Element = None; Effect = None; LayoutEffect = None; OperationState = Keep}, 
              castUnderlyingState.ReturnedValue
            )
        | Waiting -> 
          PerformWait(
            {Element = None; Effect = None; LayoutEffect = None; OperationState = Keep}
          )
        | Rerendered ->
          runUnderlying (unbox castUnderlyingState.UnderlyingState)
  })

let Once wrappedOperation = Memo wrappedOperation None