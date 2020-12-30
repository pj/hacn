module Hacn.Core
open Fable.React
open Feliz
open Fable.Core.JS
open Hacn.Operations

type OperationElement<'props when 'props: equality> =
  {
    // State storage for the operation.
    State: obj option;

    // Operation at this index.
    Operation: Operation<'props, unit>;

    // Index of the operation.
    Index: int;

    Disposer: Dispose;
  }

type RefState<'props when 'props: equality> =
  {
    // Last element rendered.
    Element: ReactElement option

    // list of current operations.
    Operations: OperationElement<'props> array

    // 'current' operation to perform, get's updated as operations change it
    // e.g. props changes
    OperationIndex: int

    PrevProps: 'props option

    ComposeReturn: obj option
  }

type CombineState<'props when 'props: equality> = {
  FirstState: obj option
  SecondState: obj option
}

let updateOperationsWith index (operations: OperationElement<'props> array) stateUpdater = 
  let op = operations.[index]
  let updatedState = stateUpdater op.State
  match updatedState with
  | Keep -> ()
  | Erase ->
    FSharp.Collections.Array.set
      operations
      index
      ({op with State = None})
  | Replace(newState) ->
    FSharp.Collections.Array.set
      operations
      index
      ({op with State = Some(newState)})

let updateDisposer index (operations: OperationElement<'props> array) disposer = 
  let op = operations.[index]
  let updatedOp = {op with Disposer = disposer}
  FSharp.Collections.Array.set
    operations
    index
    updatedOp


let runDisposers operationIndex (operations: OperationElement<'props> array) =
  for i in (operationIndex+1) .. (operations.Length-1) do
    let op = operations.[i]
    match op.Disposer with
    | Some(dispose) ->
      updateOperationsWith i operations dispose
    | _ -> ()

let getFirstOperation delayOperation resultCapture componentState =
  if componentState.OperationIndex = -1 then
    let firstOperation = 
      match delayOperation with
      | Delay(f) -> f ()
      | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)
    {
      componentState with 
        OperationIndex = 0
        Operations = [|{State = None; Operation = firstOperation; Index = 0; Disposer = None}|]
    }
  else
    componentState

// Preprocess operations e.g. props, context, refs
let preprocessOperations refState props =
  let mutable nextIndex = refState.OperationIndex
  let mutable nextState = refState
  for item in refState.Operations do
    match item with
      | {State = opState; Operation = op; Index = index} ->
        let executePreprocess preProcess = 
          // let processOpState: obj option = getOperationState refState operationType opState props

          let result = preProcess opState
          match result with
            | Some(newOpState) -> 
              FSharp.Collections.Array.set 
                nextState.Operations 
                index 
                {State = Some(newOpState); Operation = op; Index = index; Disposer = None}
              if index < nextIndex then
                nextState <- {nextState with OperationIndex = index}
                nextIndex <- index
            | None -> ()
        match op with 
          | Control({PreProcess = preProcess}) -> 
            executePreprocess preProcess
          | Compose({PreProcess = preProcess}) -> 
            executePreprocess preProcess
          | ControlProps({Changed = changed}) ->
            if changed refState.PrevProps props && index < nextIndex then
              nextState <- {nextState with OperationIndex = index}
              nextIndex <- index
          | End -> ()
          | _ -> failwith (sprintf "Unknown op %A" op)

  nextState

let execute resultCapture componentState props =
  let mutable currentIndex = componentState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = componentState.Element
  let mutable nextOperations = componentState.Operations
  let mutable nextEffects = []
  let mutable nextLayoutEffects = []
  let mutable nextProps = componentState.PrevProps
  let mutable composeReturn = None

  while not stop do
    let currentOperation = nextOperations.[currentIndex]
    let setElement element =
      match element with 
      | Some(element) ->
        renderedElement <- Some(element)
      | _ -> ()

    let setEffect effect =
      match effect with 
      | Some(effect) ->
        nextEffects <- nextEffects @ [(currentOperation.Index, effect)]
      | _ -> ()

    let setLayoutEffect layoutEffect = 
      match layoutEffect with 
      | Some(effect) ->
        nextLayoutEffects <- nextLayoutEffects @ [(currentOperation.Index, effect)]
      | _ -> ()

    let setOperationState operationState = 
      match operationState with 
      | Some(state) ->
        FSharp.Collections.Array.set
          nextOperations
          currentIndex
          {nextOperations.[currentIndex] with State = state}
      | _ -> ()

    let updateElementAndEffects (operationData: OperationData) =
      setElement operationData.Element
      setEffect operationData.Effect
      setLayoutEffect operationData.LayoutEffect
      updateOperationsWith currentIndex nextOperations (fun _ -> operationData.OperationState)

    let updateComposeElementAndEffects (composeEffects: ComposeSideEffects<'props>) =
      setElement composeEffects.Element
      nextEffects <- nextEffects @ composeEffects.Effects
      nextLayoutEffects <- nextLayoutEffects @ composeEffects.LayoutEffects
      setOperationState composeEffects.OperationState
    
    let handleNextOperation nextOperation =
      match nextOperation with
      | End -> 
        if (currentIndex + 1) < componentState.Operations.Length then
          let currentNextOp = nextOperations.[currentIndex+1]
          FSharp.Collections.Array.set
            nextOperations
            (currentIndex + 1)
            {currentNextOp with Operation = End}
        else
          nextOperations <- 
            FSharp.Collections.Array.append 
              nextOperations 
              [|
                {
                  State = None; 
                  Operation = End; 
                  Index = currentIndex + 1;
                  Disposer = None;
                }
              |]
        currentIndex <- currentIndex + 1
        stop <- true
      | Control(nextOpData) ->
        if (currentIndex + 1) < componentState.Operations.Length then
          let currentNextOp = nextOperations.[currentIndex+1]
          FSharp.Collections.Array.set
            nextOperations
            (currentIndex + 1)
            {currentNextOp with Operation = Control(nextOpData)}
        else
          let preProcessState = nextOpData.PreProcess(None)
          nextOperations <- 
            FSharp.Collections.Array.append 
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
      | Compose(nextOpData) ->
        if (currentIndex + 1) < componentState.Operations.Length then
          let currentNextOp = nextOperations.[currentIndex+1]
          FSharp.Collections.Array.set
            nextOperations
            (currentIndex + 1)
            {currentNextOp with Operation = Compose(nextOpData)}
        else
          let preProcessState = nextOpData.PreProcess(None)
          nextOperations <- 
            FSharp.Collections.Array.append 
              nextOperations 
              [|
                {
                  State = preProcessState; 
                  Operation = Compose(nextOpData); 
                  Index = currentIndex + 1;
                  Disposer = None;
                }
              |]
        currentIndex <- currentIndex + 1
      | ComposeReturn(returnType) ->
        composeReturn <- Some(returnType :> obj)
        currentIndex <- currentIndex + 1
        stop <- true

    let handleInvokeResult invokeResult =
      match invokeResult with
      // | Continue(_, __) -> failwith "Continue should only be passed into bind, not into execution."
      | ControlWait(operationData) ->
        updateElementAndEffects operationData
        stop <- true
      | ControlNext(operationData, nextOperation) ->
        updateElementAndEffects operationData
        handleNextOperation nextOperation

    match currentOperation.Operation with
    | ControlProps({Execute = execute}) ->
      let nextOperation = execute props
      nextProps <- Some(props)
      handleNextOperation nextOperation
    | Control({GetResult = getResult}) ->
      let capture = resultCapture currentOperation.Index 
      let invokeResult = getResult capture currentOperation.State props 
      handleInvokeResult invokeResult
    | Compose({GetResult = getResult}) ->
      let capture = resultCapture currentOperation.Index 
      let invokeResult = getResult capture currentOperation.State props
      handleInvokeResult invokeResult
    | End -> 
      stop <- true
    | _ -> failwith (sprintf "Unknown op %A" currentOperation)
  
  (
    {
      OperationIndex = currentIndex
      Operations = nextOperations
      Element = renderedElement
      PrevProps = nextProps
      ComposeReturn = composeReturn
    },
    nextEffects,
    nextLayoutEffects
  )

let render firstOperation props = 
  let componentStateRef: IRefValue<RefState<'props>> = Hooks.useRef({
    Element = None
    Operations = [||]
    OperationIndex = -1
    PrevProps = None
    ComposeReturn = None
  })

  // trigger rerender by updating a state variable
  let state: IStateHook<string> = Hooks.useState("asdf")
  let rerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)

  // Capture a result to return to the flow.
  let captureResult index stateUpdater = 
    // Ignore captures that occur when the operation index is less than the 
    // capture, since we might be rerendering something different.
    // console.log (sprintf "capturing at %d %A current operation index %d" index newOpState componentStateRef.current.OperationIndex)
    if index <= componentStateRef.current.OperationIndex then
      updateOperationsWith index componentStateRef.current.Operations stateUpdater
      componentStateRef.current <- {componentStateRef.current with OperationIndex = index}

      runDisposers componentStateRef.current.OperationIndex componentStateRef.current.Operations

      rerender ()

  let callEffect index (effect: Effect) =
    let disposer = effect ()
    updateDisposer index componentStateRef.current.Operations disposer

  componentStateRef.current <- 
    getFirstOperation firstOperation captureResult componentStateRef.current

  componentStateRef.current <- preprocessOperations componentStateRef.current props
  runDisposers componentStateRef.current.OperationIndex componentStateRef.current.Operations

  let nextState, nextEffects, nextLayoutEffects = 
    execute captureResult componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  Hooks.useEffect (
    fun () ->
      List.iter (fun (index, eff) -> callEffect index eff) nextEffects
      ()
    )

  // run any layout effect.
  React.useLayoutEffect (
    fun () ->
      List.iter (fun (index, eff) -> callEffect index eff) nextLayoutEffects
      ()
    )

  // Render current element
  match componentStateRef.current.Element with
    | Some(element) -> 
      element
    | None -> null

let initialExecutionState composeCapture operationStateOpt firstOperation: RefState<'props> = 
  match operationStateOpt with
  | Some(operationState) -> unbox operationState
  | None -> 
    {
      OperationIndex = 0 
      Operations = [|{
        State = None
        Operation = (firstOperation ())
        Index = 0
        Disposer = None
      }|]
      Element = None
      PrevProps = None
      ComposeReturn = None
    }

let executionCapture captureReturn index subStateUpdater = 
  let stateUpdater existingStateOpt = 
    match existingStateOpt with
    | Some(existingState) ->
      let castExistingState = unbox existingState
      if index <= castExistingState.OperationIndex then
        updateOperationsWith index castExistingState.Operations subStateUpdater
        runDisposers castExistingState.OperationIndex castExistingState.Operations

        Replace (castExistingState :> obj)
      else 
        Keep
    | None -> failwith "should not happen"
  captureReturn stateUpdater

let wrapExecutionEffects composeState effects () =
  let callEffect (index, effect) = 
    let dispose = effect ()
    updateDisposer index composeState.Operations dispose
    (index, dispose)

  let disposers = List.map callEffect effects
  let composeDisposer existingStateOpt = 
    match existingStateOpt with
    | Some(existingState) ->
      let castExistingState = unbox existingState
      let callDisposer (index, disposeOpt) =
        match disposeOpt with
        | Some(dispose) -> 
          updateOperationsWith 
            index 
            castExistingState.Operations 
            dispose
        | None -> ()

      List.iter callDisposer disposers
      Replace(castExistingState :> obj)
    | None -> failwith "should not happen"
  Some(composeDisposer)

let combine op1 op2 =
  match (op1, op2) with 
  | End, End -> End
  | _ -> 
    Control({
      PreProcess = fun _ -> None
      GetResult = fun combineCapture combineStateOpt props ->  
        let combineState = 
          match combineStateOpt with
          | Some(combineState) -> unbox combineState
          | None -> {
            FirstState = None
            SecondState = None
          }

        let createCombineCapture stateGetter stateSetter underlyingStateUpdater =
          let captureUpdater combineStateOpt =
            let combineState = 
              match combineStateOpt with
              | Some(combineState) -> unbox combineState
              | None -> failwith "Should not happen"
          
            let updateState = underlyingStateUpdater (stateGetter combineState)

            match updateState with
            | Keep -> Keep
            | Erase -> Replace(stateSetter combineState None)
            | Replace(state) -> Replace(stateSetter combineState (Some state))

          combineCapture captureUpdater

        let createCombineEffect combineState firstEffect secondEffect = 
          let combineEffect () =
            let firstDisposer = 
              match firstEffect with
              | Some(effect) ->
                effect ()
              | None -> None
            let secondDisposer = 
              match secondEffect with
              | Some(effect) ->
                effect ()
              | None -> None

            let combineDisposer combineStateOpt = 
              let combineState = 
                match combineStateOpt with
                | Some(combineState) -> unbox combineState
                | None -> failwith "Should not happen"
              
              let updatedState = 
                match firstDisposer with
                | Some(disposer) -> 
                  let firstDisposeResponse = disposer combineState.FirstState 
                  match firstDisposeResponse with
                  | Erase -> {combineState with FirstState = None}
                  | Keep -> combineState
                  | Replace(s) -> {combineState with FirstState = Some(s)}
                | None -> combineState
              match secondDisposer with
              | Some(disposer) -> 
                let disposeResponse = disposer combineState.SecondState 
                match disposeResponse with
                | Erase -> Replace({updatedState with SecondState = None})
                | Keep -> Replace(updatedState)
                | Replace(s) -> Replace({updatedState with SecondState = Some(s)})
              | None -> Replace(updatedState)
              
            Some(combineDisposer)
          combineEffect
        
        let combineExecute op existingState stateGetter stateSetter =
          let underlyingExecute firstOperation =
            let capture = createCombineCapture stateGetter stateSetter 
            let captureReturn = executionCapture capture
            let executionState = initialExecutionState captureReturn existingState firstOperation

            let executionResult, effects, layoutEffects = 
              execute captureReturn executionState (unbox props)

            let ended = 
              match executionResult.Operations.[executionResult.OperationIndex].Operation with
              | End -> true
              | _ -> false

            (
              {
                Effect = Some(wrapExecutionEffects executionResult effects)
                LayoutEffect = Some(wrapExecutionEffects executionResult layoutEffects)
                Element = executionResult.Element
                OperationState = Replace(executionResult :> obj)
              }, 
              ended
            )
          match op with
          | End -> 
            (
              {
                Effect = None
                LayoutEffect = None
                Element = None
                OperationState = Keep
              }, 
              true
            )
          | Control(controlData) ->
            underlyingExecute (fun () -> Control(controlData))
          | Delay(firstOperation) ->
            underlyingExecute firstOperation
          | _ -> failwith (sprintf "OperationType can't be used here: %A" op1)

        let firstOperationData, firstEnded = 
          combineExecute 
            op1
            combineState.FirstState
            (fun existingState -> existingState.FirstState)
            (fun existingState newValue -> {existingState with FirstState = newValue})
        
        let updatedState = 
          match firstOperationData.OperationState with
          | Erase -> {combineState with FirstState = None}
          | Keep -> combineState
          | Replace(s) -> {combineState with FirstState = Some(s)}
        
        if firstEnded then
          let secondOperationData, secondEnded = 
            combineExecute 
              op2
              combineState.SecondState
              (fun existingState -> existingState.SecondState)
              (fun existingState newValue -> {existingState with SecondState = newValue})
            
          let updatedState = 
            match secondOperationData.OperationState with
            | Erase -> {updatedState with SecondState = None}
            | Keep -> updatedState
            | Replace(s) -> {updatedState with SecondState = Some(s)}
          let element = Option.orElse firstOperationData.Element secondOperationData.Element

          let controlData = {
            Effect = Some(createCombineEffect updatedState firstOperationData.Effect secondOperationData.Effect)
            LayoutEffect = Some(createCombineEffect updatedState firstOperationData.LayoutEffect secondOperationData.LayoutEffect)
            Element = element
            OperationState = Replace(updatedState)
          }
            
          if secondEnded then
            ControlNext(controlData, End)
          else
            ControlWait(controlData)
        else
          ControlWait(
            {
              Effect = Some(createCombineEffect updatedState firstOperationData.Effect None)
              LayoutEffect = Some(createCombineEffect updatedState firstOperationData.LayoutEffect None)
              Element = firstOperationData.Element
              OperationState = Replace(updatedState)
            }
          )
    })

let bind<'props, 'resultType, 'x, 'y when 'props: equality and 'x: equality> (underlyingOperation: Operation<'props, 'resultType>) (f: 'resultType -> Operation<'x, unit>) : Operation<'x, 'y> = 
// let bind underlyingOperation f = 
  match underlyingOperation with
  | PerformProps({Changed = changed}) ->
    ControlProps(
      {
        Changed =  fun a b -> changed (unbox a) (unbox b)
        Execute = fun props -> f(unbox props)
      }
    )
  | Perform(underlyingOperationData) ->
    Control(
      {
        PreProcess = fun operationState -> underlyingOperationData.PreProcess(operationState)
        GetResult = 
          fun captureFunc operationState props ->
            let operationResult = underlyingOperationData.GetResult captureFunc operationState

            match operationResult with
            | PerformContinue(operationData, result) ->
              let nextOperation = f(result)
              ControlNext(operationData, nextOperation)
            | PerformWait(asdf) -> ControlWait(asdf)
      }
    )
  | ComposeStart(composeFirst) ->
    Compose(
      {
        PreProcess = fun operationState -> None
        GetResult = 
          fun captureReturn operationStateOpt props ->
            let composeCaptureReturn = executionCapture captureReturn
            let composeState = initialExecutionState composeCaptureReturn operationStateOpt composeFirst
            let executionResult, nextEffects, nextLayoutEffects = 
              execute composeCaptureReturn composeState (unbox props)
            
            let composeEffects = {
              Effect = Some(wrapExecutionEffects composeState nextEffects)
              LayoutEffect = Some(wrapExecutionEffects composeState nextLayoutEffects)
              Element = executionResult.Element
              OperationState = Replace(executionResult :> obj)
            }

            match executionResult.ComposeReturn with
            | Some(returnType) -> 
              let nextOperation = f (unbox returnType)
              ControlNext(composeEffects, nextOperation)
            | None ->
              ControlWait(composeEffects)
      }
    )
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

type ReactBuilder() = 
  member _.Bind(operation, f) = 
    bind operation f
  member _.Combine(left, right) =
    combine left right
  member _.Zero() = 
    End
  member _.Delay(f) = Delay f 
  member _.Run(firstOperation) =
    React.functionComponent<'props>(
      fun (props: 'props) -> 
        render firstOperation props
    ) 

let react = ReactBuilder ()

// Used for creating fragments that can be composed into a sequence
type HacnBuilder() = 
  member _.Bind(operation, f) = 
    bind operation f
  member _.Combine(left, right) =
    combine left right
  member _.Zero() = 
    ComposeReturn ()
  member _.Delay(f) = f
  member _.Run(firstOperation) =
    ComposeStart (firstOperation)

let hacn = HacnBuilder ()