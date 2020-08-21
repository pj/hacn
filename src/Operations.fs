module Hacn.Operations
open Fable.React
open FSharp.Interop.Dynamic

type PropsOperationState<'props> =
  {
    Props: 'props;
    PrevProps: 'props option;
  }

let Props<'props when 'props: equality>() =
  Perform({ 
    OperationType = Some(PropsOperation);
    PreProcess = fun (operationState) -> 
      match operationState with 
      | None -> failwith "Should never happen"
      | Some(propsOpState) ->
        let props: 'props = propsOpState?Props
        let prevPropsOption: 'props option = propsOpState?PrevProps
        match prevPropsOption with
        | None -> operationState
        | Some(prevProps) -> 
          if props <> prevProps then
            operationState
          else
            None
    Invoke = fun operationState -> 
      match operationState with
      | None -> failwith "Should not happen"
      | Some(propsOpState) ->
        let props: 'props = propsOpState?Props
        (
          {
            NextOperation = None; 
            Element = None;
            UpdatedOperationState = None;
            Effect = None;
          }, 
          props
        );
  })

let Render(element) =
  Perform({ 
    OperationType = None;
    PreProcess = fun _ -> None;
    Invoke = fun _ -> 
      (
        {
          NextOperation = None; 
          Element = Some(element);
          UpdatedOperationState = None;
          Effect = None;
        }, 
        ()
      );
  })

type StateContainer<'state> = 
  {
    Updated: bool;
    ComponentState: 'state option;
  }

let Get<'state>() =
  Perform({ 
    OperationType = Some(StateGet);
    PreProcess = fun _ -> None;
    Invoke = fun _ -> 
      (
        {
          NextOperation = None; 
          Element = None;
          UpdatedOperationState = None;
          Effect = None;
        }, 
        ()
      );
  })

let Set<'state>(newState) =
  Perform({
    OperationType = Some(StateSet);
    PreProcess = fun _ -> None;
    Invoke = fun _ -> 
      (
        {
          NextOperation = None; 
          Element = None;
          UpdatedOperationState = None;
          Effect = None;
        }, 
        newState
      );
  })
// let ContextNonPartial (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
//   // TODO: figure out how to remove nasty mutable state.
//   let mutable mutableReturnType = None
//   { 

//     NeedsPreprocess = fun () -> true;
//     PreProcess = fun operationState -> 
//       let nextReturnType = useContext(context)
//       if nextReturnType <> mutableReturnType then
//         Some()
//       returnType <- Some(useContext(context));
//     Invoke = fun refState -> 
//       let returnVar = 
//         match returnType with
//         | Some(v) -> v
//         | None -> failwith "PreProcess not called before invoking"
//       (
//         {
//           NextOperation = None; 
//           Element = None;
//           UpdatedOperationState = None;
//         }, 
//         returnVar
//       );
//   }

// let Context context = ContextNonPartial Hooks.useContext context
