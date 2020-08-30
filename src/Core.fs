module Hacn.Core
open Fable.React

type OpTreeNode<'props> =
  {
    // State storage for the operation.
    State: obj option;
    // Operation at this index.
    Operation: Operation<'props, unit>;
    // Index of the operation.
    Index: int;
  }

// type OpTree<'props> =
//   | OpState of OpTreeNode<'props>

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
    ComponentState: Operations.StateContainer<'state>;
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
      RunOperation = 
        fun captureFunc operationState ->
          match underlyingOperation with
          | Perform(operationData) -> 
            let operationResult = operationData.GetResult captureFunc operationState

            match operationResult with
            | InvokeReturn(result) -> 
              let nextOperation = f(result)
              ControlNextOperation(nextOperation)
            | InvokeBoth(element, effect) -> ControlBoth(element, effect)
            | InvokeEffect(effect) -> ControlEffect effect
            | InvokeRender(element) -> ControlRender element
            | InvokeWait -> ControlWait
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
        Operations = [|{State = None; Operation = firstOperation; Index = 0}|]
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
      let propsState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = Some(existingPropsState)}
      Some(propsState :> obj)
  | StateGet -> 
    Some(refState.ComponentState :> obj)
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
                Array.set 
                  nextState.Operations 
                  index 
                  {State = Some(newOpState); Operation = op; Index = index}
                if index < nextIndex then
                  nextState <- {refState with OperationIndex = index}
                  nextIndex <- index
              | None -> ()
          | Perform(_) -> failwith "Should not happen"
          | End -> ()

  nextState

let execute rerender (componentStateRef: IRefValue<RefState<'props, 'state>>) props =
  let componentState = componentStateRef.current
  let mutable currentIndex = componentState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = None
  let mutable nextOperations = componentState.Operations
  let mutable nextEffect = None

  let resultCapture index newOpState = 
    let op = componentStateRef.current.Operations.[index]
    let updatedOp = {op with State = newOpState}
    Array.set
      componentStateRef.current.Operations
      index
      updatedOp
    rerender ()

  let 

  while not stop do
    let currentOperation = nextOperations.[currentIndex]
    match currentOperation.Operation with
      | Control({RunOperation = runOperation}) ->
        let capture = resultCapture currentOperation.Index 
        let invokeResult = runOperation capture currentOperation.State 
        match invokeResult with
        | ControlWait -> 
          stop <- true
        | ControlRender(element) -> 
          renderedElement <- Some(element)
          stop <- true
        | ControlEffect(effect) ->
          nextEffect <- Some((effect, currentOperation))
          stop <- true
        | ControlBoth(element, effect) -> 
          renderedElement <- Some(element)
          nextEffect <- Some((effect, currentOperation))
          stop <- true
        | ControlNextOperation(nextOperation) ->
          match nextOperation with
          | End -> stop <- true
          | Perform(_) -> failwith "Perform should not be passed here"
          | Control(nextOpData) ->
            if (currentIndex + 1) < componentState.Operations.Length then
              Array.set
                nextOperations
                (currentIndex + 1)
                {currentOperation with Operation = Control(nextOpData)}
            else
              let preProcessState = 
                match nextOpData.OperationType with
                | PropsOperation -> 
                  let propsOperationState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
                  nextOpData.PreProcess(Some(propsOperationState :> obj)) |> ignore
                  Some(propsOperationState :> obj)
                | StateGet -> 
                  Some(componentState.ComponentState :> obj)
                | _ ->
                  nextOpData.PreProcess(None)
              nextOperations <- 
                Array.append 
                  nextOperations 
                  [|
                    {
                      State = preProcessState; 
                      Operation = Control(nextOpData); 
                      Index = currentIndex + 1
                    }
                  |]
            currentIndex <- currentIndex + 1
      | _ -> failwith "Unknown "
  
  (
    {
      OperationIndex = currentIndex; 
      Operations = nextOperations; 
      Element = renderedElement;
      ComponentState = componentState.ComponentState;
    },
    nextEffect
  )

let runEffects (componentStateRef: IRefValue<RefState<'props, 'state>>) rerender useEffect effect =
  let rerender opTreeIndex nextOpState =
    let currentOperation = componentStateRef.current.Operations.[opTreeIndex]
      Array.set
        componentStateRef.current.Operations
        opTreeIndex
        (OpState({node with State = nextOpState}))
      if opTreeIndex < componentStateRef.current.OperationIndex then  
        componentStateRef.current <- 
          {
            componentStateRef.current with 
              OperationIndex = opTreeIndex
              Element = None
          }
      state.update("blah")
    ()

  let getOperationState opTreeIndex _ =
    let currentOperation = componentStateRef.current.Operations.[opTreeIndex]
    match currentOperation with
    | OpState(node) -> 
      node.State

  let handleEffect = 
    fun () ->
      let (effect, opTree) = effect
      match opTree with
      | OpState(node) ->
        effect (getOperationState node.Index) (rerender node.Index)
      ()

  useEffect handleEffect

let render useRef useState useEffect delayedFunc props = 
  let componentStateRef: IRefValue<RefState<'props, 'state>> = useRef({
    Element = None;
    Operations = [||];
    OperationIndex = -1;
    ComponentState = {
      Updated = false;
      ComponentState = None;
    };
  })
  let state: IStateHook<string> = useState("asdf")
  let rerender _ =
    state.update("blah")

  // Run first operation if none already exist.
  componentStateRef.current <- getFirstOperation delayedFunc componentStateRef.current

  // run any preprocess operations.
  componentStateRef.current <- preprocessOperations componentStateRef.current props

  // execute until we hit suspend/end.
  let nextState, effect = execute rerender componentStateRef props

  componentStateRef.current <- nextState

  // run any effects.
  match effect with
  | Some(effect) -> runEffects componentStateRef useState useEffect effect
  | None -> ()

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

let hacn _ = HacnBuilder((render Hooks.useRef Hooks.useState Hooks.useEffect))