module Core
open Fable.React
open Hacn.Types

type OpTreeNode<'props> =
  {
    // State storage for the operation.
    State: OperationState option;
    // Operation at this index.
    Operation: Operation<'props, unit>;
    // Index of the operation.
    Index: int;
  }

type OpTree<'props> =
  | OpState of OpTreeNode<'props>

type RefState<'props> =
  {
    // Last element rendered.
    Element: ReactElement option;
    // list of current operations.
    Operations: OpTree<'props> array;
    // 'current' operation to perform, get's updated as operations change it
    // e.g. props changes
    OperationIndex: int;
  }

let bind underlyingOperation f = 
  Perform(
    {
      IsPropsOperation = 
        match underlyingOperation with
         | Perform(opData) -> opData.IsPropsOperation; 
         | _ -> false
      PreProcess = 
        fun operationState -> 
          match underlyingOperation with
           | Perform(opData) -> opData.PreProcess(operationState)
           | _ -> None;
      Invoke = 
        fun (operationState) ->
          match underlyingOperation with
          | Perform(operationData) -> 
            let (operationResult, returnType) = operationData.Invoke(operationState)
            let nextOperation = f(returnType)
            (
              {
                operationResult with
                  NextOperation = Some(nextOperation)
              }, 
              ()
            )
          | Suspend -> failwith "Suspend passed as operation"
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


// Preprocess operations e.g. props, context, refs
let preprocessOperations state =
  let mutable nextIndex = state.OperationIndex
  let mutable nextState = state
  for item in state.Operations do
    match item with
      | OpState({State = opState; Operation = op; Index = index}) ->
        match op with 
          | Perform({PreProcess = preProcess}) -> 
            let result = preProcess opState
            match result with
              | Some(newOpState) -> 
                Array.set 
                  nextState.Operations 
                  index 
                  (OpState({State = Some(newOpState); Operation = op; Index = index}))
                if index < nextIndex then
                  nextState <- {state with OperationIndex = index}
                  nextIndex <- index

  nextState

let execute state props =
  let mutable currentIndex = state.OperationIndex
  let mutable stop = false
  let mutable renderedElement = None
  let mutable nextOperations = state.Operations
  let mutable nextEffects = [||]
  while not stop do
    let currentOperation = nextOperations.[currentIndex]
    match currentOperation with
      | OpState(opState) ->
        match opState.Operation with
        | Perform(opData) ->
          let invokeResult, _ = opData.Invoke opState.State 
          // Update op state
          match invokeResult.UpdatedOperationState with 
          | Some(nextOpState) ->
            Array.set
              state.Operations
              currentIndex
              (OpState({opState with State = Some(nextOpState)}))
          
          // set rendered element
          match invokeResult.Element with
          | Some(element) -> 
            renderedElement <- Some(element)
          
          // handle effects
          match invokeResult.Effect with
          | Some(effect) ->
            nextEffects <- 
              Array.append
                nextEffects
                [|(effect, currentOperation)|]

          // handle next operation
          match invokeResult.NextOperation with 
          | Some(nextOp) -> 
            match nextOp with
            | Suspend -> stop <- true
            | End ->  stop <- true
            | Perform(nextOpData) ->
              // If the op already exists update the stored op
              if (currentIndex + 1) < state.Operations.Length then
                Array.set
                  nextOperations
                  (currentIndex + 1)
                  (OpState({opState with Operation = Perform(nextOpData)})) 
              else
                let preProcessState = 
                  if nextOpData.IsPropsOperation then
                    let propsOperationState = Hacn.Operations.PropsOperationState(props, None) :> OperationState
                    nextOpData.PreProcess(Some(propsOperationState)) |> ignore
                    Some(propsOperationState)
                  else
                    nextOpData.PreProcess(None)
                nextOperations <- 
                  Array.append 
                    nextOperations 
                    [|OpState({
                      State = preProcessState; 
                      Operation = Perform(nextOpData); 
                      Index = currentIndex + 1
                    })|]
              currentIndex <- currentIndex + 1
          | None -> failwith "Control operations should always return a next operation"
        | _ -> failwith "Should not happen"
  
  (
    {
      OperationIndex = currentIndex; 
      Operations = nextOperations; 
      Element = renderedElement;
    },
    nextEffects
  )

let runEffects (componentStateRef: IRefValue<RefState<'props>>) (useState: 'T -> IStateHook<'T>) useEffect effects =
  let state = useState("asdf")
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

  let handleEffects = 
    fun () ->
      for (effect, opTree) in effects do
        match opTree with
        | OpState(node) ->
          effect (getOperationState node.Index) (rerender node.Index)
      ()

  useEffect handleEffects

let render (useRef: RefState<'props> -> IRefValue<RefState<'props>>) useState useEffect delayedFunc props = 
  let componentStateRef = useRef({
    Element = None;
    Operations = [||];
    OperationIndex = -1;
  })

  // Run first operation if none already exist.
  componentStateRef.current <- getFirstOperation delayedFunc componentStateRef.current

  // run any preprocess operations.
  componentStateRef.current <- preprocessOperations componentStateRef.current

  // execute until we hit suspend/end.
  let nextState, effects = execute componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  runEffects componentStateRef useState useEffect effects

  // Render current element
  match componentStateRef.current.Element with
    | Some(element) -> element
    | None -> null

type HacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>, useState, useEffect) = 
  member this.Bind(operation, f) = bind operation f
  member this.Zero() = zero()
  member this.Delay(f) = f
  member this.Run(delayedFunc) =
    fun props children -> 
      ofFunction 
        (render useRef useState useEffect delayedFunc)
        props 
        children

let hacn<'a> = HacnBuilder<'a>(Hooks.useRef, Hooks.useState, Hooks.useEffect)