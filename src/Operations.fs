module Hacn.Operations
open Fable.React
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Dyn

type PropsOperationState<'props> =
  {
    Props: 'props;
    PrevProps: 'props option;
  }

let Props<'props when 'props: equality>() =
  Perform({ 
    OperationType = PropsOperation;
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
        Result(props)
  })

let Render(element) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    Invoke = fun _ -> 
      Render(element)
  })

type StateContainer<'state> = 
  {
    Updated: bool;
    ComponentState: 'state option;
  }

let Get<'state>(initialState: 'state option) =
  Perform({ 
    OperationType = StateGet;
    PreProcess = fun operationState -> 
      let castOperationState: StateContainer<'state> option = explicitConvert operationState
      match castOperationState with 
      | None -> 
        Some(
          {
            Updated = false; 
            ComponentState = initialState;
          } :> obj
        )
      | Some(currentState) -> 
        if currentState.Updated then
          Some({currentState with Updated = false} :> obj)
        else 
          None
    Invoke = fun operationState -> 
      let stateCast: 'state option = operationState?State
      match stateCast with
      | Some(state) -> 
        Result(state)
      | None -> failwith "Please set state before calling Get()"
  })

let Set<'state>(newState: 'state) =
  Perform({
    OperationType = StateSet;
    PreProcess = fun _ -> None;
    Invoke = fun _ -> 
      Effect(
        fun _ render -> 
          render(
            Some(
              {
                Updated = true;
                ComponentState = Some(newState)
              } :> obj
            )
          )
      )
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
