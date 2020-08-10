module Hacn.Operations
open Fable.React
open Hacn.Types

type PropsOperationState<'props>(props: 'props) =
  inherit OperationState()
  member _.Props = props

let Props<'props>() =
  let mutable mutablePrevProps = None;
  { 
    NeedsPreprocess = fun () -> true;
    PreProcess = fun (operationState) -> 
      let castOperationState = operationState :?> PropsOperationState<'props>
      match mutablePrevProps with 
        | None -> 
          mutablePrevProps <- Some(castOperationState.Props)
          Some(operationState)
        | Some(prevProps) ->
          if prevProps <> castOperationState.Props then
            mutablePrevProps <- Some(castOperationState.Props)
            Some(operationState)
          else
            None
    Invoke = fun operationState -> 
      let castOperationState = operationState :?> PropsOperationState<'props>
      (
        {
          NextOperation = None; 
          Element = None;
          UpdatedOperationState = None;
        }, 
        castOperationState.Props
      );
  }

let Render(element) =
  { 
    NeedsPreprocess = fun () -> false;
    PreProcess = fun (operationState) -> None;
    Invoke = fun (refState) -> 
      (
        {
          NextOperation = None; 
          Element = Some(element);
          UpdatedOperationState = None;
        }, 
        ()
      );
  }

let ContextNonPartial (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
  // TODO: figure out how to remove nasty mutable state.
  let mutable mutableReturnType = None
  { 

    NeedsPreprocess = fun () -> true;
    PreProcess = fun operationState -> 
      let nextReturnType = useContext(context)
      if nextReturnType <> mutableReturnType then
        Some()
      returnType <- Some(useContext(context));
    Invoke = fun refState -> 
      let returnVar = 
        match returnType with
        | Some(v) -> v
        | None -> failwith "PreProcess not called before invoking"
      (
        {
          NextOperation = None; 
          Element = None;
          UpdatedOperationState = None;
        }, 
        returnVar
      );
  }

let Context context = ContextNonPartial Hooks.useContext context
