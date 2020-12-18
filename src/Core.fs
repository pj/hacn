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

// reference to capture function so that rendered elements can implicitly return
// captured events.
let mutable implicitCapture = None

// let Combine op1 op2 = 
//   // let checkNotCore underlyingOperation = 
//   //   match underlyingOperation with
//   //   | End -> ()
//   //   | Control({OperationType = NotCore}) -> ()
//   //   | _ -> failwith "Underlying operations must be a Control NotCore operation"
//   // checkNotCore op1
//   // checkNotCore op2
//   Perform({ 
//     PreProcess = fun _ -> None
//     GetResult = fun capture operationState -> 
//       let underlyingStateCast: (obj option) array = 
//         match operationState with
//         | Some(x) -> unbox x
//         | None -> [|None; None|]
//       let opState1 = underlyingStateCast.[0]
//       let opResult1 = 
//         match op1 with
//         | End -> Next(operationData, End)
//         | Control(pd1) -> 
//           pd1.GetResult capture opState1
//         | _ -> failwith "Can only work with Perform operations"
//       let opState2 = underlyingStateCast.[1]
//       let opResult2 = 
//         match op2 with
//         | End -> ControlNext(None, None, None, End)
//         | Control(pd2) -> 
//           pd2.GetResult capture opState2
//         | _ -> failwith "Can only work with Perform operations"
      
//       match opResult1, opResult2 with
//       | ControlWait(element1, effect1, layoutEffect1), ControlWait(element2, effect2, layoutEffect2) ->
//         ControlWait(
//           (getElement element1 element2), 
//           (createCombinedEffect effect1 effect2), 
//           (createCombinedEffect layoutEffect1 layoutEffect2)
//         )

//       | ControlWait(element1, effect1, layoutEffect1), ControlNext(element2, effect2, layoutEffect2, _) ->
//         ControlWait(
//           (getElement element1 element2), 
//           (createCombinedEffect effect1 effect2), 
//           (createCombinedEffect layoutEffect1 layoutEffect2)
//         )

//       | ControlNext(element1, effect1, layoutEffect1, _), ControlWait(element2, effect2, layoutEffect2) ->
//         ControlWait(
//           (getElement element1 element2), 
//           (createCombinedEffect effect1 effect2),
//           (createCombinedEffect layoutEffect1 layoutEffect2)
//         )

//       | ControlNext(element1, effect1, layoutEffect1, _), ControlNext(element2, effect2, layoutEffect2, ret2) ->
//         ControlNext(
//           (getElement element1 element2), 
//           (createCombinedEffect effect1 effect2),
//           (createCombinedEffect layoutEffect1 layoutEffect2),
//           ret2
//         )
//   })

// let combine left right = 
//     match left, right with
//     | (End, End) -> End 
//     | _ -> Combine left right

let zero() =
  End

let updateOperationsWith index (operations: OperationElement<'props> array) updatedState = 
  let op = operations.[index]
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

let runDisposers (componentStateRef: IRefValue<RefState<'props>>) =
  for i in (componentStateRef.current.OperationIndex+1) .. (componentStateRef.current.Operations.Length-1) do
    let op = componentStateRef.current.Operations.[i]
    match op.Disposer with
    | Some(dispose) ->
      let updatedState = dispose op.State
      updateOperationsWith i componentStateRef.current.Operations updatedState
    | _ -> ()

let getFirstOperation firstOperation resultCapture componentState =
  if componentState.OperationIndex = -1 then
    implicitCapture <- Some(resultCapture 0)
    {
      componentState with 
        OperationIndex = 0
        Operations = [|{State = None; Operation = (firstOperation ()); Index = 0; Disposer = None}|]
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
        match op with 
          | Control({PreProcess = preProcess}) -> 
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
          | ControlProps({Changed = changed}) ->
            if changed refState.PrevProps props && index < nextIndex then
              nextState <- {nextState with OperationIndex = index}
              nextIndex <- index
          | End -> ()
          | _ -> failwith (sprintf "Unknown op %A" op)

  nextState

let execute resultCapture wrapEffect componentState props =
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
    console.log (sprintf "current index: %d" currentIndex)
    // console.log (sprintf "Current Operation %A" currentOperation)
    let setElement element =
      match element with 
      | Some(element) ->
        console.log (sprintf "Rendering %A" element)
        renderedElement <- Some(element)
      | _ -> ()

    let setEffect effect =
      match effect with 
      | Some(effect) ->
        nextEffects <- nextEffects @ [(wrapEffect currentOperation.Index effect)]
      | _ -> ()

    let setLayoutEffect layoutEffect = 
      match layoutEffect with 
      | Some(effect) ->
        nextLayoutEffects <- nextLayoutEffects @ [(wrapEffect currentOperation.Index effect)]
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
      setOperationState operationData.OperationState
      updateOperationsWith currentIndex nextOperations operationData.OperationState

    let updateComposeElementAndEffects (composeEffects: ComposeSideEffects<'props>) =
      setElement composeEffects.Element
      List.iter (fun effect -> setEffect (Some(effect))) composeEffects.Effects
      List.iter (fun effect -> setLayoutEffect (Some(effect))) composeEffects.LayoutEffects
      setOperationState composeEffects.OperationState
    
    let handleNextOperation nextOperation =
      match nextOperation with
      | End -> 
        console.log "Next operation is End"
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
      | ComposeReturn(returnType) ->
        composeReturn <- Some(returnType :> obj)
        currentIndex <- currentIndex + 1
        stop <- true

    let handleInvokeResult invokeResult =
      match invokeResult with
      // | Continue(_, __) -> failwith "Continue should only be passed into bind, not into execution."
      | ControlWait(operationData) ->
        updateElementAndEffects operationData
        console.log "Waiting"
        stop <- true
      | ControlNext(operationData, nextOperation) ->
        updateElementAndEffects operationData
        handleNextOperation nextOperation

    implicitCapture <- Some(resultCapture (currentOperation.Index + 1))
    match currentOperation.Operation with
    | ControlProps({Execute = execute}) ->
      let nextOperation = execute props
      nextProps <- Some(props)
      handleNextOperation nextOperation
    | Control({GetResult = getResult}) ->
      let capture = resultCapture currentOperation.Index 
      console.log (sprintf "Setting capture for index %d" currentOperation.Index)
      let invokeResult = getResult capture currentOperation.State 
      handleInvokeResult invokeResult
    | Compose({ExecuteCompose = executeCompose}) ->
      let capture = resultCapture currentOperation.Index 
      console.log (sprintf "Setting capture for index %d" currentOperation.Index)
      let invokeResult = executeCompose capture currentOperation.State props (wrapEffect currentOperation.Index)
      match invokeResult with
      | ComposeWait(composeEffects) ->
        updateComposeElementAndEffects composeEffects
        stop <- true
      | ComposeFinished(composeEffects, nextOperation) ->
        updateComposeElementAndEffects composeEffects
        handleNextOperation nextOperation

    | End -> 
      console.log "Current operation is End"
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
      let op = componentStateRef.current.Operations.[index]
      let updatedOp = {op with State = (stateUpdater op.State)}
      FSharp.Collections.Array.set
        componentStateRef.current.Operations
        index
        updatedOp
      componentStateRef.current <- {componentStateRef.current with OperationIndex = index}

      runDisposers componentStateRef

      rerender ()

  let updateDisposer index disposer = 
    let op = componentStateRef.current.Operations.[index]
    let updatedOp = {op with Disposer = disposer}
    FSharp.Collections.Array.set
      componentStateRef.current.Operations
      index
      updatedOp

  let wrapEffect index (effect: Effect) =
    let wrapUpdateState index stateUpdater = 
      if index <= componentStateRef.current.OperationIndex then
        let op = componentStateRef.current.Operations.[index]

        let updatedState = stateUpdater op.State
        FSharp.Collections.Array.set
          componentStateRef.current.Operations
          index
          {op with State = updatedState}
        componentStateRef.current <- {componentStateRef.current with OperationIndex = index}
        runDisposers componentStateRef
        rerender ()
    let updateState = wrapUpdateState index
    let wrappedEffect () =
      let disposer = effect updateState
      updateDisposer index disposer
      ()
    wrappedEffect

  componentStateRef.current <- getFirstOperation firstOperation captureResult componentStateRef.current

  componentStateRef.current <- preprocessOperations componentStateRef.current props
  runDisposers componentStateRef

  let nextState, wrappedNextEffects, wrappedNextLayoutEffects = execute captureResult wrapEffect componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  Hooks.useEffect (
    fun () ->
      List.map (fun eff -> eff ()) wrappedNextEffects |> ignore
      ()
    )

  // run any layout effect.
  React.useLayoutEffect (
    fun () ->
      List.map (fun eff -> eff ()) wrappedNextLayoutEffects |> ignore
      ()
    )

  // Render current element
  match componentStateRef.current.Element with
    | Some(element) -> 
      element
    | None -> null

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
          fun captureFunc operationState ->
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
        ExecuteCompose = 
          fun captureReturn operationStateOpt props wrapEffect ->
            implicitCapture <- Some(resultCapture 0)
            let composeState: RefState<'props> = 
              match operationStateOpt with
              | Some(operationState) -> unbox operationState
              | None -> 
                {
                  OperationIndex = 0 
                  Operations = [|{
                    State = None
                    Operation = (composeFirst ())
                    Index = 0
                    Disposer = None
                  }|]
                  Element = None
                  PrevProps = None
                  ComposeReturn = None
                }
            let composeCapture index subStateUpdater = 
              let stateUpdater existingStateOpt = 
                match existingStateOpt with
                | Some(existingState) ->
                  let castExistingState = unbox existingState
                  if index <= castExistingState.OperationIndex then
                    let op = castExistingState.Operations.[index]
                    let updatedOp = {op with State = (subStateUpdater op.State)}
                    FSharp.Collections.Array.set
                      castExistingState.Operations
                      index
                      updatedOp
                    runDisposers castExistingState

                    Some (castExistingState :> obj)
                  else 
                    None
                  | None -> failwith "should not happen"
              captureReturn stateUpdater
            
            let composeWrapEffect index effect = 
              let composeEffect rerender =
                let composeStateUpdater existingStateOpt = 
                  match existingStateOpt with
                  | Some(existingState) ->
                    let castExistingState = unbox existingState
                    if index <= castExistingState.OperationIndex then
                      let mutable updatedState = None
                      let op = castExistingState.Operations.[index]
                      let underlyingRender underlyingUpdater =
                        updatedState <- underlyingUpdater op.State
                        ()

                      let dispose = effect underlyingRender
                      FSharp.Collections.Array.set
                        castExistingState.Operations
                        index
                        {op with State = updatedState; Disposer = dispose}
                      runDisposers componentStateRef

                      Some(castExistingState :> obj)
                    else 
                      None
                  | None -> failwith "should not happen"

                rerender composeStateUpdater

                None
              wrapEffect composeEffect

            let executionResult, nextEffects, layoutEffects = 
              execute composeCapture composeWrapEffect composeState (unbox props)
            
            let composeEffects = {
              Effects = nextEffects
              LayoutEffects = layoutEffects
              Element = executionResult.Element
              OperationState = Some (Some(executionResult :> obj))
            }
            match executionResult.ComposeReturn with
            | Some(returnType) -> 
              let nextOperation = f (unbox returnType)
              ComposeFinished(composeEffects, nextOperation)
            | None ->
              ComposeWait(composeEffects)
      }
    )
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)


type ReactBuilder() = 
  member _.Bind(operation, f) = 
    bind operation f
  // member _.Combine(left, right) =
  //   combine left right
  member _.Zero() = 
    zero()
  member _.Delay(f) = f
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
  // member _.Combine(left, right) =
  //   combine left right
  member _.Zero() = 
    ComposeReturn ()
  member _.Delay(f) = f
  member _.Run(firstOperation) =
    ComposeStart (firstOperation)

let hacn = HacnBuilder ()