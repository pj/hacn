module Hacn.Operations
open Fable.React

type PropsOperationState<'props>(props: 'props, prevProps: 'props option) =
  inherit OperationState()
  member _.Props = props
  member _.PrevProps = prevProps

let Props<'props when 'props: equality>() =
  Perform({ 
    IsPropsOperation = true;
    PreProcess = fun (operationState) -> 
      match operationState with 
      | None -> failwith "Should never happen"
      | Some(opState) ->
        match opState with
        | :? PropsOperationState<_> as pos -> 
          match pos.Props, pos.PrevProps with
          | (_, None) -> operationState 
          | (props, Some(prevProps)) when props <> prevProps -> operationState
          | _ -> None
        | _ -> failwith "should not happen";
    Invoke = fun operationState -> 
      match operationState with
      | None -> failwith "Should not happen"
      | Some(propsOpState) ->
        match propsOpState with
        | :? PropsOperationState<_> as pos ->
          (
            {
              NextOperation = None; 
              Element = None;
              UpdatedOperationState = None;
              Effect = None;
            }, 
            pos.Props
          );
        | _ -> failwith "Runtime passed incorrect operation state subclass into props operation invoke"
  })

let Render(element) =
  Perform({ 
    IsPropsOperation = false;
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
