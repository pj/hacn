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
    Run = fun operationState -> 
      match operationState with
      | None -> failwith "Should not happen"
      | Some(propsOpState) ->
        let props: 'props = propsOpState?Props
        InvokeReturn(props)
  })

let Render(element) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    Run = fun _ -> 
      InvokeRender(element, [])
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
    Run = fun operationState -> 
      let stateCast: 'state option = operationState?State
      match stateCast with
      | Some(state) -> InvokeReturn(state)
      | None -> failwith "Please set state before calling Get()"
  })

let Set<'state>(newState: 'state) =
  Perform({
    OperationType = StateSet;
    PreProcess = fun _ -> None;
    Run = fun _ -> 
      InvokeEffect(
        [
          fun control -> 
            control.Rerender(
              Some(
                {
                  Updated = true;
                  ComponentState = Some(newState)
                } :> obj
              )
            )
        ]
      )
  })

let ContextCore<'returnType when 'returnType : equality> (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun operationState -> 
      let currentContext = useContext(context)
      let castOperationState: 'returnType option = explicitConvert operationState
      match castOperationState with
      | Some(existingContext) -> 
        if existingContext <> currentContext then
          Some(currentContext :> obj)
        else 
          None
      | None -> Some(currentContext :> obj)
    Run = fun operationState -> 
      let castOperationState: 'returnType option = explicitConvert operationState
      match castOperationState with
      | Some(existingContext) -> InvokeReturn(existingContext)
      | None -> failwith "should not happen"
  })

let Context context = ContextCore Hooks.useContext context

let Continue operation = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    Run = fun _ -> 
      InvokeReturn(operation)
  })

let Wait operations = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    Run = fun _ -> 
      InvokeReturn(operations)
  })

let WaitAny = End
// Return a series of suspended effect results as a stream
let Next = End
// Return a series of suspended effect results as a stream (in any order)
let NextAny = End
let Timeout = End
let Interval = End
let Fetch = End
let Ref = End

// Call a function passed in through props in an effect.
let Call = End

// Merging results

// Error handling

// Handling events