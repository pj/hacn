module Hacn
open Fable.React
open Hacn.Types

type OpTreeNode<'props> =
  | OpState of OperationState * Operation<'props, unit>

type RefState<'props> =
  {
    PrevProps: 'props option;
    CurrentProps: 'props;
    Element: ReactElement option;
    NextOperation: OpTreeNode<'props> option;
    PreProcessOperations: OpTreeNode<'props> list;
    OpTree: OpTreeNode<'props> list;
  }

type PreProcessResult<'props> = 
  {
    UpdatedPreProcessOperations: OpTreeNode<'props> list;
    ChangedPreProcessOperation: OpTreeNode<'props> option;
  }

let bind operation f = 
  {
    NeedsPreprocess = fun () -> operation.NeedsPreprocess();
    PreProcess = fun (operationState) -> operation.PreProcess(operationState);
    Invoke = 
      fun (refState) ->
        let (operationResult, returnType) = operation.Invoke(refState)
        let nextOperation = f(returnType)
        ({
          operationResult with 
            NextOperation = Some(nextOperation); 
        }, ());
  }

let zero() =
  { 
    NeedsPreprocess = fun () -> false;
    PreProcess = fun (_) -> None;
    Invoke = fun (operationState) -> 
      (({
        NextOperation = None; 
        Element = None;
        UpdatedOperationState = None;
      }, ()));
  } 

// Preprocess operations e.g. props, context, refs
let preprocessOperations state =
  let preProcessResult = 
    let updateState state item =
      let changed, updatedOpState = 
        match item with
          | OpState(opState, op) ->
            let result = op.PreProcess(Some(opState));
            match result with
              | Some(newOpState) -> 
                (true, OpState(newOpState, op))
              | None -> 
                (false, OpState(opState, op))

      let updatedOperations = List.append state.UpdatedPreProcessOperations [updatedOpState]
      let changedOp =
        match state with
          | {ChangedPreProcessOperation = None} -> 
            if changed then
              Some(updatedOpState)
            else
              None
          | _ -> state.ChangedPreProcessOperation
      {UpdatedPreProcessOperations = updatedOperations; ChangedPreProcessOperation = changedOp}

    List.fold 
      updateState 
      {UpdatedPreProcessOperations = []; ChangedPreProcessOperation = None}
      state.PreProcessOperations

  match preProcessResult.ChangedPreProcessOperation with
    | Some(op) -> 
      {
        state with 
          PreProcessOperations = preProcessResult.UpdatedPreProcessOperations;
          NextOperation = Some(op)
        }
    | _ -> 
      {state with PreProcessOperations = preProcessResult.UpdatedPreProcessOperations}


let render (useRef: RefState<'props> -> IRefValue<RefState<'props>>) delayedFunc props = 
  let refState = useRef({
    PrevProps = None;
    CurrentProps = props;
    Element = None;
    PreProcessOperations = [];
    NextOperation = None;
    OpTree = [];
  })

  refState.current <- 
    match refState.current.NextOperation with
    | Some(_) -> refState.current
    | None -> 
      let firstOperation = delayedFunc()
      {refState.current with NextOperation = firstOperation}
      // let firstState = 
      //   if firstOperation.IsProps then 
      //     Some(Hacn.Operations.PropsOperationState(props) :> OperationState)
      //   else 
      //     None
      // let invokeResult, _ = firstOperation.Invoke(firstState)

  refState.current <- preprocessOperations refState.current

  match refState.current.Element with
    | Some(element) -> element
    | None -> null

type HacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(operation, f) = bind operation f
  member this.Zero() = zero()
  member this.Delay(f) = f
  member this.Run(f) =
    fun props children -> 
      ofFunction 
        (render useRef f)
        props 
        children

let hacn<'a> = HacnBuilder<'a>(Hooks.useRef)