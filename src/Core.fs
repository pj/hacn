module Hacn.Core
open Fable.React
open FSharp.Interop.Dynamic.Dyn

type OpTreeNode<'props> =
  {
    // State storage for the operation.
    State: obj option;

    // Operation at this index.
    Operation: Operation<'props, unit>;

    // Index of the operation.
    Index: int;

    Disposer: Dispose;
  }

type RefState<'props, 'state> =
  {
    // Last element rendered.
    Element: ReactElement option;

    // list of current operations.
    Operations: OpTreeNode<'props> array;

    // 'current' operation to perform, get's updated as operations change it
    // e.g. props changes
    OperationIndex: int;

    // Component state object
    ComponentState: obj option;
  }

let bind underlyingOperation f = 
  Control(
    {
      OperationType = 
        match underlyingOperation with
         | Perform(opData) -> opData.OperationType; 
         | _ -> failwith "Underlying must be perform Operation"
      PreProcess = 
        fun operationState -> 
          match underlyingOperation with
           | Perform(opData) -> opData.PreProcess(operationState)
           | _ -> failwith "Underlying must be perform Operation"
      GetResult = 
        fun captureFunc operationState ->
          match underlyingOperation with
          | Perform(operationData) -> 
            let operationResult = operationData.GetResult captureFunc operationState
            // printf "Result: %A\n" operationResult

            match operationResult with
            | InvokeContinue(element, effect, result) ->
              let nextOperation = f(result)
              ControlNext(element, effect, nextOperation)
            | InvokeWait(element, effect) -> ControlWait(element, effect) 
          | Control(_) -> failwith "Control passed as operation"
          | End -> failwith "End passed as operation"
    }
  )

let zero() =
  End

let getFirstOperation delayedFunc componentState =
  if componentState.OperationIndex = -1 then
    let firstOperation = delayedFunc()
    {
      componentState with 
        OperationIndex = 0
        Operations = [|{State = None; Operation = firstOperation; Index = 0; Disposer = None}|]
    }
  else
    componentState

let getOperationState refState operationType opState props =
  match operationType with
  | PropsOperation -> 
    match opState with
    | None -> 
      let propsState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
      Some(propsState :> obj)
    | Some(existingPropsState) -> 
      let castPropsState: Operations.PropsOperationState<obj> = explicitConvert existingPropsState
      let propsState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = Some(castPropsState.Props)}
      Some(propsState :> obj)
  | StateGet -> 
    printf "Getting state during pre process: %A\n" refState.ComponentState
    Option.map (fun state -> state :> obj) refState.ComponentState
  | _ -> opState

// Preprocess operations e.g. props, context, refs
let preprocessOperations refState props =
  let mutable nextIndex = refState.OperationIndex
  let mutable nextState = refState
  for item in refState.Operations do
    match item with
      | {State = opState; Operation = op; Index = index} ->
        match op with 
          | Control({PreProcess = preProcess; OperationType = operationType}) -> 
            let processOpState: obj option = getOperationState refState operationType opState props

            let result = preProcess processOpState
            match result with
              | Some(newOpState) -> 
                if operationType = StateGet then
                  printf "Setting state during pre process: %A\n" newOpState
                  nextState <- {refState with ComponentState = Some(newOpState)}
                Array.set 
                  nextState.Operations 
                  index 
                  {State = Some(newOpState); Operation = op; Index = index; Disposer = None}
                if index < nextIndex then
                  nextState <- {refState with OperationIndex = index}
                  nextIndex <- index
              | None -> ()
          | End -> ()
          | other -> failwith (sprintf "Should not happen %A\n" other)

  nextState

let execute resultCapture wrapEffect componentState props =
  let mutable currentIndex = componentState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = None
  let mutable nextOperations = componentState.Operations
  let mutable nextEffects = []
  let mutable updatedComponentState = componentState.ComponentState

  while not stop do
    let currentOperation = nextOperations.[currentIndex]
    match currentOperation.Operation with
      | Control({GetResult = getResult}) ->
        let capture = resultCapture currentOperation.Index 
        let invokeResult = getResult capture currentOperation.State 
        match invokeResult with
        | ControlWait(elementOpt, effectOpt) ->
          match elementOpt with 
          | Some(element) ->
            renderedElement <- Some(element)
          | _ -> ()
          match effectOpt with 
          | Some(effect) ->
            nextEffects <- nextEffects @ [(wrapEffect currentOperation.Index effect)]
          | _ -> ()
          stop <- true
        | ControlNext(elementOpt, effectOpt, nextOperation) ->
          match elementOpt with 
          | Some(element) ->
            renderedElement <- Some(element)
          | _ -> ()
          match effectOpt with 
          | Some(effect) ->
            nextEffects <- nextEffects @ [(wrapEffect currentOperation.Index effect)]
          | _ -> ()
          match nextOperation with
          | End -> 
            stop <- true
          | Control(nextOpData) ->
            if (currentIndex + 1) < componentState.Operations.Length then
              let currentNextOp = nextOperations.[currentIndex+1]
              Array.set
                nextOperations
                (currentIndex + 1)
                {currentNextOp with Operation = Control(nextOpData)}
            else
              let preProcessState = 
                match nextOpData.OperationType with
                | PropsOperation -> 
                  let propsOperationState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
                  nextOpData.PreProcess(Some(propsOperationState :> obj)) |> ignore
                  Some(propsOperationState :> obj)
                | StateGet -> 
                  printf "Getting state during execute: %A\n" componentState.ComponentState
                  let updatedStateGet = nextOpData.PreProcess(componentState.ComponentState) |> ignore
                  let updatedStateGetOpt = Some(updatedStateGet :> obj)
                  updatedComponentState <- updatedStateGetOpt
                  printf "After getting state during execute: %A\n" updatedStateGet
                  updatedStateGetOpt
                | _ ->
                  nextOpData.PreProcess(None)
              nextOperations <- 
                Array.append 
                  nextOperations 
                  [|
                    {
                      State = preProcessState; 
                      Operation = Control(nextOpData); 
                      Index = currentIndex + 1;
                      Disposer = None;
                    }
                  |]
            currentIndex <- currentIndex + 1
          | other -> failwith (sprintf "Unhandled operation %A" other)
      | unknownOperation -> failwith (sprintf "Unknown operation %A" unknownOperation)
  
  (
    {
      OperationIndex = currentIndex; 
      Operations = nextOperations; 
      Element = renderedElement;
      ComponentState = updatedComponentState
    },
    nextEffects
  )

let render useRef useState useEffect delayedFunc props = 
  let componentStateRef: IRefValue<RefState<'props, 'state>> = useRef({
    Element = None;
    Operations = [||];
    OperationIndex = -1;
    ComponentState = None;
  })

  // trigger rerender by updating a state variable
  let state: IStateHook<string> = useState("asdf")
  let rerender _ =
    state.update("blah")

  // Capture a result to return to the flow.
  let updateStateAt index newOpState = 
    let op = componentStateRef.current.Operations.[index]
    let updatedOp = {op with State = newOpState}
    Array.set
      componentStateRef.current.Operations
      index
      updatedOp
    rerender ()

  let updateDisposer index disposer = 
    let op = componentStateRef.current.Operations.[index]
    let updatedOp = {op with Disposer = disposer}
    Array.set
      componentStateRef.current.Operations
      index
      updatedOp

  let wrapEffect index effect =
    let wrapUpdateState index stateUpdater = 
      let op = componentStateRef.current.Operations.[index]

      let state =
        match op.Operation with 
        | Control({OperationType = StateSet}) -> 
          componentStateRef.current.ComponentState
        | _ -> op.State
      let updatedState = stateUpdater state
      match op.Operation with 
      | Control({OperationType = StateSet}) -> 
        componentStateRef.current <- {componentStateRef.current with ComponentState = updatedState}
      | _ ->
        Array.set
          componentStateRef.current.Operations
          index
          {op with State = updatedState}
      rerender ()
    let updateState = wrapUpdateState index
    let wrappedEffect _ =
      let disposer = effect updateState
      updateDisposer index disposer
      ()
    wrappedEffect

  printf "---------------------\n"

  componentStateRef.current <- getFirstOperation delayedFunc componentStateRef.current

  componentStateRef.current <- preprocessOperations componentStateRef.current props

  let nextState, wrappedNextEffects = execute updateStateAt wrapEffect componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  useEffect (
    fun () ->
      List.map (fun eff -> eff ()) wrappedNextEffects |> ignore
      ()
    )

  // Render current element
  match componentStateRef.current.Element with
    | Some(element) -> element
    | None -> null

type HacnBuilder(render) = 
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = zero()
  member _.Delay(f) = f
  member _.Run(delayedFunc) =
    fun props children -> 
      ofFunction 
        (render delayedFunc)
        props
        children

let hacn = HacnBuilder((render Hooks.useRef Hooks.useState Hooks.useEffect))