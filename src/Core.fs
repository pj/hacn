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

type OpTree<'props> =
  | OpState of OpTreeNode<'props>

type RefState<'props, 'state> =
  {
    // Last element rendered.
    Element: ReactElement option;
    // list of current operations.
    Operations: OpTree<'props> array;
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
      Invoke = 
        fun (operationState) ->
          match underlyingOperation with
          | Perform(operationData) -> 
            let operationResult = operationData.Run(operationState)

            match operationResult with
            | InvokeResult(result) -> 
              let nextOperation = f(result)
              ControlNextOperation(nextOperation)
            | InvokeRender(element) -> ControlRender(element)
            | InvokeEffect(effect) -> ControlEffect(effect)
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
        Operations = [|OpState({State = None; Operation = firstOperation; Index = 0})|]
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
      | OpState({State = opState; Operation = op; Index = index}) ->
        match op with 
          | Control({PreProcess = preProcess; OperationType = operationType}) -> 
            let processOpState: obj option = getOperationState refState operationType opState props

            let result = preProcess processOpState
            match result with
              | Some(newOpState) -> 
                Array.set 
                  nextState.Operations 
                  index 
                  (OpState({State = Some(newOpState); Operation = op; Index = index}))
                if index < nextIndex then
                  nextState <- {refState with OperationIndex = index}
                  nextIndex <- index
              | None -> ()
          | Perform(_) -> failwith "Should not happen"
          | End -> ()

  nextState

let execute refState props =
  let mutable currentIndex = refState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = None
  let mutable nextOperations = refState.Operations
  let mutable nextEffect = None
  // let mutable nextComponentState = None
  while not stop do
    let currentOperation = nextOperations.[currentIndex]
    match currentOperation with
      | OpState(opState) ->
        match opState.Operation with
        | Control(opData) ->
          let invokeResult = opData.Invoke opState.State 
          match invokeResult with
          | ControlRender(element) -> 
            renderedElement <- Some(element)
            stop <- true
          // | UpdateState(componentState) ->
          //   nextComponentState <- Some(componentState)
          //   stop <- true
          | ControlEffect(effect) ->
            nextEffect <- Some((effect, currentOperation))
            stop <- true
          | ControlNextOperation(nextOperation) ->
            // handle next operation
            match nextOperation with
            | End -> stop <- true
            | Perform(_) -> failwith "Perform should not be passed here"
            | Control(nextOpData) ->
              // If the op already exists update the stored op
              if (currentIndex + 1) < refState.Operations.Length then
                Array.set
                  nextOperations
                  (currentIndex + 1)
                  (OpState({opState with Operation = Control(nextOpData)})) 
              else
                let preProcessState = 
                  match nextOpData.OperationType with
                  | PropsOperation -> 
                    let propsOperationState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
                    nextOpData.PreProcess(Some(propsOperationState :> obj)) |> ignore
                    Some(propsOperationState :> obj)
                  | StateGet -> 
                    Some(refState.ComponentState :> obj)
                  | _ ->
                    nextOpData.PreProcess(None)
                nextOperations <- 
                  Array.append 
                    nextOperations 
                    [|OpState({
                      State = preProcessState; 
                      Operation = Control(nextOpData); 
                      Index = currentIndex + 1
                    })|]
              currentIndex <- currentIndex + 1
        | _ -> failwith "Unknown "
  
  (
    {
      OperationIndex = currentIndex; 
      Operations = nextOperations; 
      Element = renderedElement;
      ComponentState = refState.ComponentState;
    },
    nextEffect
  )

let runEffects (componentStateRef: IRefValue<RefState<'props, 'state>>) useState useEffect effect =
  let state: IStateHook<string> = useState("asdf")
  let rerender opTreeIndex nextOpState =
    let currentOperation = componentStateRef.current.Operations.[opTreeIndex]
    match currentOperation with
    | OpState(node) -> 
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

type UseEffect = ((unit -> unit)  -> (obj array) option) -> unit

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

  // Run first operation if none already exist.
  componentStateRef.current <- getFirstOperation delayedFunc componentStateRef.current

  // run any preprocess operations.
  componentStateRef.current <- preprocessOperations componentStateRef.current props

  // execute until we hit suspend/end.
  let nextState, effect = execute componentStateRef.current props

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