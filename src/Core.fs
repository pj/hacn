module Hacn.Core

open Fable.React
open Feliz
open Fable.Core.JS
open Hacn.Operations

type OperationElement<'props when 'props: equality> =
  {
    // State storage for the operation.
    State: obj option

    // Operation at this index.
    Operation: Operation<'props, unit>

    // Index of the operation.
    Index: int

    Disposer: Dispose

    Exception: ExceptionDetails option }

type RefState<'props when 'props: equality> =
  {
    // Last element rendered.
    Element: (ReactElement * CaptureReturn) option

    // list of current operations.
    Operations: OperationElement<'props> list

    // 'current' operation to perform, get's updated as operations change it
    // e.g. props changes
    OperationIndex: int

    PrevProps: 'props option

    ComposeReturn: obj option }
 
type ExecutionResult<'props when 'props: equality> =
  {
    RefState: RefState<'props>

    NextEffects: (int * Effect) list

    NextLayoutEffects: (int * Effect) list

    InTryWith: bool
  }

type CombineState<'props when 'props: equality> =
  { FirstState: obj option
    SecondState: obj option }

type ErrorBoundaryProps =
  { Inner: ReactElement
    OnError: CaptureReturn }

type ErrorBoundary(props) =
  inherit Component<ErrorBoundaryProps, unit>(props)

  override x.componentDidCatch(error, info) =
    let info = info :?> InfoComponentObject

    props.OnError
      (fun _ ->
        SetException(
          { Exception = error
            InfoComponent = Some(info) }
        ))

  override x.render() = x.props.Inner

let hasException operations =
  FSharp.Collections.Array.tryFind (fun x -> x.Exception.IsSome) operations

let checkStateEnded refState =
  match refState.Operations.[refState.OperationIndex]
          .Operation with
  | End -> true
  | _ -> false

let updateOperationsWith index (operations: OperationElement<'props> array) stateUpdater =
  let op = operations.[index]
  let updatedState = stateUpdater op.State

  match updatedState with
  | Keep -> ()
  | Erase -> FSharp.Collections.Array.set operations index ({ op with State = None })
  | Replace (newState) -> FSharp.Collections.Array.set operations index ({ op with State = Some(newState) })
  | SetException (exceptionDetails) ->
      FSharp.Collections.Array.set
        operations
        index
        ({ op with
             Exception = Some(exceptionDetails) })

let updateDisposer index (operations: OperationElement<'props> array) disposer =
  let op = operations.[index]
  let updatedOp = { op with Disposer = disposer }
  FSharp.Collections.Array.set operations index updatedOp

let runDisposers operationIndex (operations: OperationElement<'props> array) =
  for i in (operationIndex + 1) .. (operations.Length - 1) do
    let op = operations.[i]

    match op.Disposer with
    | Some (dispose) -> updateOperationsWith i operations dispose
    | _ -> ()

let getFirstOperation delayOperation componentState =
  if componentState.OperationIndex = -1 then
    let firstOperation =
      match delayOperation with
      | Delay (f) -> f ()
      | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)

    { componentState with
        OperationIndex = 0
        Operations =
          [ { 
              State = None
              Operation = firstOperation
              Index = 0
              Disposer = None
              Exception = None 
            } ] }
  else
    componentState

let initialExecutionState operationStateOpt firstOperation : RefState<'props> =
  match operationStateOpt with
  | Some (operationState) -> unbox operationState
  | None ->
      { OperationIndex = 0
        Operations =
          [ { 
              State = None
              Operation = (firstOperation ())
              Index = 0
              Disposer = None
              Exception = None 
            } ]
        Element = None
        PrevProps = None
        ComposeReturn = None }

let throwIfException operations =
  for operation in operations do
    match operation.Exception with
    | Some (details) -> raise details.Exception
    | None -> ()

// Preprocess operations e.g. props, context, refs
let preprocessOperations refState props =
  let mutable nextIndex = refState.OperationIndex
  let mutable nextState = refState

  for item in refState.Operations do
    match item with
    | { State = opState
        Operation = op
        Index = index } ->
        let executePreprocess preProcess =
          // let processOpState: obj option = getOperationState refState operationType opState props

          let result = preProcess opState

          match result with
          | Some (newOpState) ->
              FSharp.Collections.Array.set
                nextState.Operations
                index
                { State = Some(newOpState)
                  Operation = op
                  Index = index
                  Disposer = None
                  Exception = None }

              if index < nextIndex then
                nextState <-
                  { nextState with
                      OperationIndex = index }

                nextIndex <- index
          | None -> ()

        match op with
        | Control ({ PreProcess = preProcess }) -> executePreprocess preProcess
        | Compose ({ PreProcess = preProcess }) -> executePreprocess (fun state -> preProcess state props)
        | ControlProps ({ Changed = changed }) ->
            if changed refState.PrevProps props
               && index < nextIndex then
              nextState <-
                { nextState with
                    OperationIndex = index }

              nextIndex <- index
        | TryWith (_) ->
            // TODO: preprocess for TryWith
            ()
        | End -> ()
        | _ -> failwith (sprintf "Unknown op %A" op)

  nextState

let consEffect index headOpt rest = 
  match headOpt with 
  | Some(effect) -> (index, effect) :: rest
  | None -> rest

let updateOperationWithUpdater operation operationUpdater = 
  match operationUpdater with
  | Replace(state) -> 
      {
        operation with
          State = Some(state)
      }
  | Erase ->
      {
        operation with
          State = None
      }
  | Keep -> operation
  | SetException(_exception) ->
      {
        operation with
          Exception = Some(_exception)
      }

let rec executionRecur rerender resultCapture exState props operations = 
  let updateElementAndEffects state operation rest operationData =
    let effects = 
      match operationData.Effect with 
      | Some(effect) -> (operation.Index, effect) :: state.NextEffects
      | None -> state.NextEffects
    let layoutEffects = 
      match operationData.LayoutEffect with 
      | Some(effect) -> (operation.Index, effect) :: state.NextLayoutEffects
      | None -> exState.NextLayoutEffects
    let element = 
      match operationData.Element with 
      | Some(element) -> Some(element)
      | None -> state.RefState.Element
    let updatedOperation = 
      match operationData.OperationState with
      | Replace(state) -> 
          {
            operation with
              State = Some(state)
          }
      | Erase ->
          {
            operation with
              State = None
          }
      | Keep -> operation
      | SetException(_exception) ->
          {
            operation with
              Exception = Some(_exception)
          }

    {
      state with
        RefState = {
          state.RefState with 
            Operations = updatedOperation :: rest
            Element = element
        } 
        NextEffects = effects
        NextLayoutEffects = layoutEffects
        
    }
  match operations with 
  | operation :: rest -> 
    if operation.Index < exState.RefState.OperationIndex then
      let next = executionRecur rerender resultCapture exState props rest
      { 
        next with 
          RefState = {
            next.RefState with 
              Operations = operation :: next.RefState.Operations
            }
        }
    else
      match operation.Operation with 
      | End ->
        {
          exState with
            InTryWith = true
            RefState = {
              exState.RefState with 
                Operations = operation :: []
                OperationIndex = operation.Index
          } 
        }
      | ControlProps ({ Execute = execute }) ->
        let nextOperation = execute props
        let nextElement = {
          State = None
          Operation = nextOperation
          Index = operation.Index + 1
          Disposer = None
          Exception = None
        }
        let next = executionRecur rerender resultCapture exState props [nextElement]
        {
          next with
            InTryWith = false
            RefState = {
              next.RefState with 
                Operations = operation :: next.RefState.Operations 
                PrevProps = Some(props)
                OperationIndex = operation.Index
          } 
        }
      | Control ({ GetResult = getResult }) ->
        let capture = resultCapture operation.Index

        let invokeResult =
          getResult rerender capture operation.State props
        
        match invokeResult with
        | ControlWait (operationData) ->
          {
            exState with
              InTryWith = false
              RefState = {
                exState.RefState with 
                  Operations = (updateOperationWithUpdater operation operationData.OperationState) :: rest
                  Element = Option.orElse exState.RefState.Element operationData.Element
                  OperationIndex = operation.Index
              } 
              NextEffects = consEffect operation.Index operationData.Effect exState.NextEffects
              NextLayoutEffects = consEffect operation.Index operationData.LayoutEffect exState.NextLayoutEffects
          }
        | ControlNext (operationData, nextOperation) ->
            let nextElement = {
              State = None
              Operation = nextOperation
              Index = operation.Index + 1
              Disposer = None
              Exception = None
            }
            let next = executionRecur rerender resultCapture exState props [nextElement]
            {
              next with
                InTryWith = false
                RefState = {
                  next.RefState with 
                    Operations = (updateOperationWithUpdater operation operationData.OperationState) :: next.RefState.Operations
                    Element = Option.orElse exState.RefState.Element operationData.Element
                    OperationIndex = operation.Index
                } 
                NextEffects = consEffect operation.Index operationData.Effect next.NextEffects
                NextLayoutEffects = consEffect operation.Index operationData.LayoutEffect next.NextLayoutEffects
            }
        | Compose ({ GetResult = getResult }) ->
          inTryWith <- false
          let capture = resultCapture currentOperation.Index

          let invokeResult =
            getResult rerender capture currentOperation.State props

          handleInvokeResult invokeResult
        | TryWith ({ GetResult = getResult }) ->
            inTryWith <- true
            let capture = resultCapture currentOperation.Index

            let invokeResult =
              getResult rerender capture currentOperation.State props

            handleInvokeResult invokeResult

  | _ -> failwith "Implementation incorrect"

let recursiveExecution rerender resultCapture componentState props = 
  let initial = {
      State = { componentState with Operations = [] }
      NextEffects = []
      NextLayoutEffects = []
      InTryWith = false
    }
  
  executionRecur rerender resultCapture initial props componentState.Operations

let foldExecutionLoop rerender resultCapture componentState props = 
  let initial = {
      State = { componentState with Operations = [] }
      NextEffects = []
      NextLayoutEffects = []
      InTryWith = false
    }
  
  let executionFolder exState operation =
    if operation.Index < exState.State.OperationIndex then
      { 
        exState with 
          State = {
            exState.State with 
              Operations = operation :: exState.State.Operations
          }
      }
    else
      exState
  
  List.fold executionFolder initial componentState.Operations

let executionLoop rerender resultCapture componentState props =
  let mutable currentIndex = componentState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = componentState.Element
  let mutable nextOperations = componentState.Operations
  let mutable nextEffects = []
  let mutable nextLayoutEffects = []
  let mutable nextProps = componentState.PrevProps
  let mutable composeReturn = None
  let mutable inTryWith = false

  while not stop do
    let currentOperation = nextOperations.[currentIndex]

    let setElement element =
      match element with
      | Some (element) -> renderedElement <- Some(element)
      | _ -> ()

    let setEffect effect =
      match effect with
      | Some (effect) -> nextEffects <- nextEffects @ [ (currentOperation.Index, effect) ]
      | _ -> ()

    let setLayoutEffect layoutEffect =
      match layoutEffect with
      | Some (effect) ->
          nextLayoutEffects <-
            nextLayoutEffects
            @ [ (currentOperation.Index, effect) ]
      | _ -> ()

    let setOperationState operationState =
      match operationState with
      | Some (state) ->
          FSharp.Collections.Array.set
            nextOperations
            currentIndex
            { nextOperations.[currentIndex] with
                State = state }
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
            let currentNextOp = nextOperations.[currentIndex + 1]

            FSharp.Collections.Array.set nextOperations (currentIndex + 1) { currentNextOp with Operation = End }
          else
            nextOperations <-
              FSharp.Collections.Array.append
                nextOperations
                [| { State = None
                     Operation = End
                     Index = currentIndex + 1
                     Disposer = None
                     Exception = None } |]

          currentIndex <- currentIndex + 1
          stop <- true
      | Control (nextOpData) ->
          if (currentIndex + 1) < componentState.Operations.Length then
            let currentNextOp = nextOperations.[currentIndex + 1]

            FSharp.Collections.Array.set
              nextOperations
              (currentIndex + 1)
              { currentNextOp with
                  Operation = Control(nextOpData) }
          else
            let preProcessState = nextOpData.PreProcess(None)

            nextOperations <-
              FSharp.Collections.Array.append
                nextOperations
                [| { State = preProcessState
                     Operation = Control(nextOpData)
                     Index = currentIndex + 1
                     Disposer = None
                     Exception = None } |]

          currentIndex <- currentIndex + 1
      | Compose (nextOpData) ->
          if (currentIndex + 1) < componentState.Operations.Length then
            let currentNextOp = nextOperations.[currentIndex + 1]

            FSharp.Collections.Array.set
              nextOperations
              (currentIndex + 1)
              { currentNextOp with
                  Operation = Compose(nextOpData) }
          else
            let preProcessState = nextOpData.PreProcess None props

            nextOperations <-
              FSharp.Collections.Array.append
                nextOperations
                [| { State = preProcessState
                     Operation = Compose(nextOpData)
                     Index = currentIndex + 1
                     Disposer = None
                     Exception = None } |]

          currentIndex <- currentIndex + 1
      | ComposeReturn (returnType) ->
          composeReturn <- Some(returnType :> obj)

          nextOperations <-
            FSharp.Collections.Array.append
              nextOperations
              [| { State = None
                   Operation = End
                   Index = currentIndex + 1
                   Disposer = None
                   Exception = None } |]

          currentIndex <- currentIndex + 1

          stop <- true

    let handleInvokeResult invokeResult =
      match invokeResult with
      // | Continue(_, __) -> failwith "Continue should only be passed into bind, not into execution."
      | ControlWait (operationData) ->
          updateElementAndEffects operationData
          stop <- true
      | ControlNext (operationData, nextOperation) ->
          updateElementAndEffects operationData
          handleNextOperation nextOperation

    match currentOperation.Operation with
    | ControlProps ({ Execute = execute }) ->
        inTryWith <- false
        let nextOperation = execute props
        nextProps <- Some(props)
        handleNextOperation nextOperation
    | Control ({ GetResult = getResult }) ->
        inTryWith <- false
        let capture = resultCapture currentOperation.Index

        let invokeResult =
          getResult rerender capture currentOperation.State props

        handleInvokeResult invokeResult
    | Compose ({ GetResult = getResult }) ->
        inTryWith <- false
        let capture = resultCapture currentOperation.Index

        let invokeResult =
          getResult rerender capture currentOperation.State props

        handleInvokeResult invokeResult
    | TryWith ({ GetResult = getResult }) ->
        inTryWith <- true
        let capture = resultCapture currentOperation.Index

        let invokeResult =
          getResult rerender capture currentOperation.State props

        handleInvokeResult invokeResult
    | End ->
        inTryWith <- true
        stop <- true
    | _ -> failwith (sprintf "Unknown op %A" currentOperation)

  ({ OperationIndex = currentIndex
     Operations = nextOperations
     Element = renderedElement
     PrevProps = nextProps
     ComposeReturn = composeReturn },
   nextEffects,
   nextLayoutEffects,
   inTryWith)

let render firstOperation props =
  let componentStateRef : IRefValue<RefState<'props>> =
    Hooks.useRef (
      { Element = None
        Operations = [||]
        OperationIndex = -1
        PrevProps = None
        ComposeReturn = None }
    )

  // trigger rerender by updating a state variable
  let state : IStateHook<string> = Hooks.useState ("asdf")

  let rerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)

  // Capture a result to return to the flow.
  let captureResult index stateUpdater =
    // Ignore captures that occur when the operation index is less than the
    // capture, since we might be rerendering something different.
    if index <= componentStateRef.current.OperationIndex then
      updateOperationsWith index componentStateRef.current.Operations stateUpdater

      componentStateRef.current <-
        { componentStateRef.current with
            OperationIndex = index }

      runDisposers componentStateRef.current.OperationIndex componentStateRef.current.Operations

      rerender ()

  let callEffect index (effect: Effect) =
    let disposer = effect ()
    updateDisposer index componentStateRef.current.Operations disposer

  componentStateRef.current <- getFirstOperation firstOperation componentStateRef.current

  throwIfException componentStateRef.current.Operations

  componentStateRef.current <- preprocessOperations componentStateRef.current props
  runDisposers componentStateRef.current.OperationIndex componentStateRef.current.Operations

  let nextState, nextEffects, nextLayoutEffects, inTryWith =
    executionLoop rerender captureResult componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  Hooks.useEffect
    (fun () ->
      List.iter (fun (index, eff) -> callEffect index eff) nextEffects
      ())

  // run any layout effect.
  React.useLayoutEffect
    (fun () ->
      List.iter (fun (index, eff) -> callEffect index eff) nextLayoutEffects
      ())

  // Render current element
  match componentStateRef.current.Element with
  | Some ((element, capture)) ->
      if inTryWith then
        ofType<ErrorBoundary, _, _> { Inner = element; OnError = capture } []
      else
        element
  | None -> null

let executionCapture captureReturn index subStateUpdater =
  let stateUpdater existingStateOpt =
    match existingStateOpt with
    | Some (existingState) ->
        let castExistingState = unbox existingState

        if index <= castExistingState.OperationIndex then
          updateOperationsWith index castExistingState.Operations subStateUpdater

          let nextState =
            { castExistingState with
                OperationIndex = index }

          runDisposers castExistingState.OperationIndex castExistingState.Operations

          Replace(nextState :> obj)
        else
          Keep
    | None -> failwith "should not happen"

  captureReturn stateUpdater

let wrapExecutionEffects composeState effects () =
  let callEffect (index, effect) =
    let dispose = effect ()
    updateDisposer index composeState.Operations dispose

  List.iter callEffect effects

  let composeDisposer existingStateOpt =
    printf "Compose Disposer called %A" existingStateOpt
    match existingStateOpt with
    | Some (existingState) ->
        let castExistingState = unbox existingState


        let callDisposer op =
          match op.Disposer with
          | Some (dispose) -> updateOperationsWith op.Index castExistingState.Operations dispose
          | None -> ()

        FSharp.Collections.Array.iter callDisposer castExistingState.Operations
        Replace(castExistingState :> obj)
    | None -> failwith "should not happen"

  Some(composeDisposer)

let captureForState rerender underlyingCapture stateGetter stateSetter underlyingStateUpdater =
  let captureUpdater combineStateOpt =
    let combineState =
      match combineStateOpt with
      | Some (combineState) -> unbox combineState
      | None -> failwith "Should not happen"

    let updateState =
      underlyingStateUpdater (stateGetter combineState)

    match updateState with
    | Keep -> Keep
    | Erase -> Replace(stateSetter combineState None)
    | Replace (state) ->
        rerender ()
        Replace(stateSetter combineState (Some state))
    | SetException (_) -> failwith "Can't set exception in combine"

  underlyingCapture captureUpdater

// Execute state for the compose, combine, try/with constructs.
let executeState rerender underlyingCapture props op existingState stateGetter stateSetter =
  let underlyingExecute firstOperation =
    let capture =
      captureForState rerender underlyingCapture stateGetter stateSetter

    let captureReturn = executionCapture capture

    let executionState =
      initialExecutionState existingState firstOperation

    let executionResult, effects, layoutEffects, _ =
      executionLoop rerender captureReturn executionState (unbox props)

    let ended = checkStateEnded executionResult

    ({ Effect = Some(wrapExecutionEffects executionResult effects)
       LayoutEffect = Some(wrapExecutionEffects executionResult layoutEffects)
       Element = executionResult.Element
       OperationState = Replace(executionResult :> obj) },
     ended,
     executionResult.ComposeReturn)

  match op with
  | End ->
      ({ Effect = None
         LayoutEffect = None
         Element = None
         OperationState = Keep },
       true,
       None)
  | Control (controlData) -> underlyingExecute (fun () -> Control(controlData))
  | Delay (firstOperation) -> underlyingExecute firstOperation
  | ComposeReturn (value) ->
      ({ Effect = None
         LayoutEffect = None
         Element = None
         OperationState = Keep },
       true,
       Some(unbox value))
  | ControlProps (data) -> underlyingExecute (fun () -> ControlProps(data))
  | _ -> failwith (sprintf "OperationType can't be used here: %A" op)

let combine op1 op2 =
  match (op1, op2) with
  | End, End -> End
  | _ ->
      Control(
        { PreProcess = fun _ -> None
          GetResult =
            fun rerender combineCapture combineStateOpt props ->
              let combineState =
                match combineStateOpt with
                | Some (combineState) -> unbox combineState
                | None ->
                    { FirstState = None
                      SecondState = None }

              let createCombineEffect combineState firstEffect secondEffect =
                let combineEffect () =
                  let firstDisposer =
                    match firstEffect with
                    | Some (effect) -> effect ()
                    | None -> None

                  let secondDisposer =
                    match secondEffect with
                    | Some (effect) -> effect ()
                    | None -> None

                  let combineDisposer combineStateOpt =
                    printf "Combined Disposer Called"
                    let combineState =
                      match combineStateOpt with
                      | Some (combineState) -> unbox combineState
                      | None -> failwith "Should not happen"

                    // let updatedState =
                    match firstDisposer with
                    | Some (disposer) ->
                        printf "First Disposer Called"
                        disposer combineState.FirstState |> ignore

                        // match firstDisposeResponse with
                        // | Erase -> { combineState with FirstState = None }
                        // | Keep -> combineState
                        // | Replace (s) ->
                        //     { combineState with
                        //         FirstState = Some(unbox s) }
                        // | SetException (_) -> failwith "Can't set exception in dispose"
                    // | None -> combineState
                    | None -> ()

                    match secondDisposer with
                    | Some (disposer) ->
                        printf "Second Disposer Called: %A" disposer
                        disposer combineState.SecondState |> ignore

                        // match disposeResponse with
                        // | Erase -> Replace({ updatedState with SecondState = None })
                        // | Keep -> Replace(updatedState)
                        // | Replace (s) ->
                        //     Replace(
                        //       { updatedState with
                        //           SecondState = Some(unbox s) }
                        //     )
                        // | SetException (_) -> failwith "Can't set exception in dispose"
                    | None -> ()
                    Replace({FirstState = None; SecondState = None})

                  Some(combineDisposer)

                combineEffect

              let firstOperationData, firstEnded, _ =
                executeState
                  rerender
                  combineCapture
                  props
                  op1
                  combineState.FirstState
                  (fun existingState -> existingState.FirstState)
                  (fun existingState newValue ->
                    { existingState with
                        FirstState = newValue })

              let updatedState =
                match firstOperationData.OperationState with
                | Erase -> { combineState with FirstState = None }
                | Keep -> combineState
                | Replace (s) ->
                    { combineState with
                        FirstState = Some(s) }
                | SetException (_) -> failwith "Can't set exception in combine execute"

              if firstEnded then
                let secondOperationData, secondEnded, _ =
                  executeState
                    rerender
                    combineCapture
                    props
                    op2
                    combineState.SecondState
                    (fun existingState -> existingState.SecondState)
                    (fun existingState newValue ->
                      { existingState with
                          SecondState = newValue })

                let updatedState =
                  match secondOperationData.OperationState with
                  | Erase -> { updatedState with SecondState = None }
                  | Keep -> updatedState
                  | Replace (s) ->
                      { updatedState with
                          SecondState = Some(s) }
                  | SetException (_) -> failwith "Can't set exception in combine execute"
                // printf "Updated State second operation %A" updatedState

                let element =
                  Option.orElse firstOperationData.Element secondOperationData.Element

                // printf "First Effect: %A " firstOperationData.Effect
                // printf "Second Effect: %A " secondOperationData.Effect
                let controlData =
                  { Effect = Some(createCombineEffect updatedState firstOperationData.Effect secondOperationData.Effect)
                    LayoutEffect =
                      Some(
                        createCombineEffect
                          updatedState
                          firstOperationData.LayoutEffect
                          secondOperationData.LayoutEffect
                      )
                    Element = element
                    OperationState = Replace(updatedState) }

                if secondEnded then
                  ControlNext(controlData, End)
                else
                  ControlWait(controlData)
              else
                // printf "First Effect: %A " firstOperationData.Effect
                ControlWait(
                  { Effect = Some(createCombineEffect updatedState firstOperationData.Effect None)
                    LayoutEffect = Some(createCombineEffect updatedState firstOperationData.LayoutEffect None)
                    Element = firstOperationData.Element
                    OperationState = Replace(updatedState) }
                ) }
      )

let bind<'props, 'resultType, 'x, 'y when 'props: equality and 'x: equality>
  (underlyingOperation: Operation<'props, 'resultType>)
  (f: 'resultType -> Operation<'x, unit>)
  : Operation<'x, 'y> =
  // let bind underlyingOperation f =
  match underlyingOperation with
  | PerformProps ({ Changed = changed }) ->
      ControlProps(
        { Changed = fun a b -> changed (unbox a) (unbox b)
          Execute = fun props -> f (unbox props) }
      )
  | Perform (underlyingOperationData) ->
      Control(
        { PreProcess = fun operationState -> underlyingOperationData.PreProcess(operationState)
          GetResult =
            fun rerender captureFunc operationState props ->
              let operationResult =
                underlyingOperationData.GetResult captureFunc operationState

              match operationResult with
              | PerformContinue (operationData, result) ->
                  let nextOperation = f (result)
                  ControlNext(operationData, nextOperation)
              | PerformWait (asdf) -> ControlWait(asdf) }
      )
  | ComposeStart (composeFirst) ->
      Compose(
        { PreProcess =
            fun operationState props ->
              match operationState with
              | Some (refState) -> Some((preprocessOperations (unbox refState) props) :> obj)
              | None -> None

          GetResult =
            fun rerender captureReturn operationStateOpt props ->
              let composeEffects, ended, composeReturn =
                executeState
                  rerender
                  captureReturn
                  props
                  (composeFirst ())
                  operationStateOpt
                  (fun existingState -> existingState)
                  (fun _ newState -> newState)

              if ended then
                let nextOperation =
                  match composeReturn with
                  | Some (x) -> f (unbox x)
                  | None -> f (unbox ())

                ControlNext(composeEffects, nextOperation)
              else
                ControlWait(composeEffects) }
      )
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

type ExceptionState<'props when 'props: equality> =
  { ExecutingState: RefState<'props> option
    ExceptionState: RefState<'props> option
    InException: bool }

let tryWith bodyOperation handler =
  TryWith
    { GetResult =
        fun rerender captureResult tryWithStateOpt props ->
          let tryWithState =
            match tryWithStateOpt with
            | Some (s) -> unbox s
            | None ->
                { ExecutingState = None
                  ExceptionState = None
                  InException = false }

          let existingError =
            match tryWithState.ExecutingState with
            | Some (s) -> hasException s.Operations
            | None -> None

          let handleOperation =
            if (not tryWithState.InException)
               && existingError.IsSome then
              handler existingError.Value.Exception.Value
            else
              End

          let inException =
            tryWithState.InException || existingError.IsSome

          if inException then
            match tryWithState.ExceptionState with
            | Some (x) -> throwIfException x.Operations
            | None -> ()

            let exceptionEffects, ended, _ =
              executeState
                rerender
                captureResult
                props
                handleOperation
                tryWithState.ExceptionState
                (fun existingState -> existingState.ExceptionState)
                (fun existingState newValue ->
                  { existingState with
                      ExceptionState = (unbox newValue) })

            let updatedState =
              match exceptionEffects.OperationState with
              | Erase ->
                  Replace(
                    { tryWithState with
                        ExecutingState = None }
                  )
              | Keep -> Keep
              | Replace (s) ->
                  Replace(
                    { tryWithState with
                        ExecutingState = Some(unbox s) }
                  )
              | SetException (_) -> failwith "Can't set exception in combine execute"

            let x =
              { exceptionEffects with
                  OperationState = updatedState }

            if ended then
              ControlNext(x, End)
            else
              ControlWait(x)
          else
            let exceptionEffects, ended, _ =
              executeState
                rerender
                captureResult
                props
                bodyOperation
                tryWithState.ExecutingState
                (fun existingState -> existingState.ExecutingState)
                (fun existingState newValue ->
                  { existingState with
                      ExecutingState = unbox newValue })

            let updatedState =
              match exceptionEffects.OperationState with
              | Erase ->
                  Replace(
                    { tryWithState with
                        ExecutingState = None }
                  )
              | Keep -> Keep
              | Replace (s) ->
                  Replace(
                    { tryWithState with
                        ExecutingState = Some(unbox s) }
                  )
              | SetException (_) -> failwith "Can't set exception in combine execute"

            let x =
              { exceptionEffects with
                  OperationState = updatedState }

            if ended then
              ControlNext(x, End)
            else
              ControlWait(x) }

type ReactBuilder() =
  member _.Bind(operation, f) = bind operation f
  member _.Combine(left, right) = combine left right
  member _.TryWith(expr, handler) = tryWith expr handler
  member _.Zero() = End
  member _.Delay(f) = Delay f

  member _.Run(firstOperation) =
    React.functionComponent<'props> (fun (props: 'props) -> render firstOperation props)

let react = ReactBuilder()

// Used for creating fragments that can be composed into a sequence
type HacnBuilder() =
  member _.Bind(operation, f) = bind operation f
  member _.Combine(left, right) = combine left right
  member _.TryWith(expr, handler) = tryWith expr handler
  member _.Zero() = End
  member _.Delay(f) = f
  member _.Return(v) = ComposeReturn(unbox v)
  member _.Run(firstOperation) = ComposeStart(firstOperation)

let hacn = HacnBuilder()
