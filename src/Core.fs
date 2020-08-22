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
  Perform(
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
            let (operationResult, returnType) = operationData.Invoke(operationState)

            match operationResult.NextOperation with
            | Some(underly) ->
            | None ->  
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

let getOperationState operationType opState props =
  match operationType with
  | Some(coreOperationType) -> 
    match coreOperationType with
    | PropsOperation -> 
      match opState with
      | None -> 
        let propsState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
        Some(propsState :> obj)
      | Some(existingPropsState) -> 
        let propsState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = Some(existingPropsState)}
        Some(propsState :> obj)
    | _ -> None
  | None -> 
    opState

// Preprocess operations e.g. props, context, refs
let preprocessOperations refState props =
  let mutable nextIndex = refState.OperationIndex
  let mutable nextState = refState
  for item in refState.Operations do
    match item with
      | OpState({State = opState; Operation = op; Index = index}) ->
        match op with 
          | Perform({PreProcess = preProcess; OperationType = operationType}) -> 
            let processOpState: obj option = getOperationState operationType opState props

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

  nextState

let execute refState props =
  let mutable currentIndex = refState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = None
  let mutable nextOperations = refState.Operations
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
              refState.Operations
              currentIndex
              (OpState({opState with State = Some(nextOpState)}))
          | _ -> ()
          
          // set rendered element
          match invokeResult.Element with
          | Some(element) -> 
            renderedElement <- Some(element)
          | _ -> ()
          
          // handle effects
          match invokeResult.Effect with
          | Some(effect) ->
            nextEffects <- 
              Array.append
                nextEffects
                [|(effect, currentOperation)|]
          | _ -> ()

          // handle next operation
          match invokeResult.NextOperation with 
          | Some(nextOp) -> 
            match nextOp with
            | Suspend -> stop <- true
            | End -> stop <- true
            | Perform(nextOpData) ->
              // If the op already exists update the stored op
              if (currentIndex + 1) < refState.Operations.Length then
                Array.set
                  nextOperations
                  (currentIndex + 1)
                  (OpState({opState with Operation = Perform(nextOpData)})) 
              else
                let preProcessState = 
                  match nextOpData.OperationType with
                    | Some(coreOperationType) -> 
                      match coreOperationType with
                      | PropsOperation -> 
                        let propsOperationState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
                        nextOpData.PreProcess(Some(propsOperationState :> obj)) |> ignore
                        Some(propsOperationState :> obj)
                      | StateGet ->
                        
                      | _ ->
                        nextOpData.PreProcess(None)
                    | None -> 
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

let runEffects (componentStateRef: IRefValue<RefState<'props>>) useState useEffect effects =
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

  let handleEffects = 
    fun () ->
      for (effect, opTree) in effects do
        match opTree with
        | OpState(node) ->
          effect (getOperationState node.Index) (rerender node.Index)
      ()

  useEffect handleEffects

let render useRef useState useEffect delayedFunc props = 
  let componentStateRef: IRefValue<RefState<'props>> = useRef({
    Element = None;
    Operations = [||];
    OperationIndex = -1;
  })

  // Run first operation if none already exist.
  componentStateRef.current <- getFirstOperation delayedFunc componentStateRef.current

  // run any preprocess operations.
  componentStateRef.current <- preprocessOperations componentStateRef.current props

  // execute until we hit suspend/end.
  let nextState, effects = execute componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  runEffects componentStateRef useState useEffect effects

  // Render current element
  match componentStateRef.current.Element with
    | Some(element) -> element
    | None -> null

type HacnBuilder(render) = 
  member this.Bind(operation, f) = bind operation f
  member this.Zero() = zero()
  member this.Delay(f) = f
  member this.Run(delayedFunc) =
    fun props children -> 
      ofFunction 
        (render delayedFunc)
        props 
        children

let hacn _ = HacnBuilder((render Hooks.useRef Hooks.useState Hooks.useEffect))