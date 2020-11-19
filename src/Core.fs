module Hacn.Core
open Fable.React
open Feliz
open Fable.Core.JS

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

            match operationResult with
            | InvokeContinue(element, effect, layoutEffect, result) ->
              let nextOperation = f(result)
              ControlNext(element, effect, layoutEffect, nextOperation)
            | InvokeWait(element, effect, layoutEffect) -> ControlWait(element, effect, layoutEffect) 
          | Control(_) -> failwith "Control passed as operation"
          | End -> failwith "End passed as operation"
    }
  )

let zero() =
  End

let runDisposers (componentStateRef: IRefValue<RefState<'props, 'state>>) =
  for i in (componentStateRef.current.OperationIndex+1) .. (componentStateRef.current.Operations.Length-1) do
    let op = componentStateRef.current.Operations.[i]
    match op.Disposer with
    | Some(dispose) ->
      let updatedState = dispose op.State
      FSharp.Collections.Array.set
        componentStateRef.current.Operations
        i
        ({op with State = updatedState})
    | _ -> ()

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
      let castPropsState: Operations.PropsOperationState<obj> = unbox existingPropsState
      let propsState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = Some(castPropsState.Props)}
      Some(propsState :> obj)
  // | StateGet -> 
  //   Option.map (fun state -> state :> obj) refState.ComponentState
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
                // if operationType = StateGet then
                //   nextState <- {nextState with ComponentState = Some(newOpState)}
                FSharp.Collections.Array.set 
                  nextState.Operations 
                  index 
                  {State = Some(newOpState); Operation = op; Index = index; Disposer = None}
                if index < nextIndex then
                  nextState <- {nextState with OperationIndex = index}
                  nextIndex <- index
              | None -> ()
          | End -> ()
          | other -> failwith (sprintf "Should not happen %A\n" other)

  nextState

let execute resultCapture wrapEffect componentState props =
  let mutable currentIndex = componentState.OperationIndex
  let mutable stop = false
  let mutable renderedElement = componentState.Element
  let mutable nextOperations = componentState.Operations
  let mutable nextEffects = []
  let mutable nextLayoutEffects = []
  let mutable updatedComponentState = componentState.ComponentState

  while not stop do
    let currentOperation = nextOperations.[currentIndex]
    match currentOperation.Operation with
      | Control({GetResult = getResult}) ->
        let capture = resultCapture currentOperation.Index 
        let invokeResult = getResult capture currentOperation.State 
        match invokeResult with
        | ControlWait(elementOpt, effectOpt, layoutEffectOpt) ->
          match elementOpt with 
          | Some(element) ->
            renderedElement <- Some(element)
          | _ -> ()
          match effectOpt with 
          | Some(effect) ->
            nextEffects <- nextEffects @ [(wrapEffect currentOperation.Index effect)]
          | _ -> ()
          match layoutEffectOpt with 
          | Some(effect) ->
            nextLayoutEffects <- nextLayoutEffects @ [(wrapEffect currentOperation.Index effect)]
          | _ -> ()
          stop <- true
        | ControlNext(elementOpt, effectOpt, layoutEffectOpt, nextOperation) ->
          match elementOpt with 
          | Some(element) ->
            renderedElement <- Some(element)
          | _ -> ()
          match effectOpt with 
          | Some(effect) ->
            nextEffects <- nextEffects @ [(wrapEffect currentOperation.Index effect)]
          | _ -> ()
          match layoutEffectOpt with 
          | Some(effect) ->
            nextLayoutEffects <- nextLayoutEffects @ [(wrapEffect currentOperation.Index effect)]
          | _ -> ()
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
              let preProcessState = 
                match nextOpData.OperationType with
                | PropsOperation -> 
                  let propsOperationState: Operations.PropsOperationState<obj> = {Props = props; PrevProps = None}
                  nextOpData.PreProcess(Some(propsOperationState :> obj)) |> ignore
                  Some(propsOperationState :> obj)
                // | StateGet -> 
                //   let updatedStateGet = nextOpData.PreProcess(componentState.ComponentState) |> ignore
                //   let updatedStateGetOpt = Some(updatedStateGet :> obj)
                //   updatedComponentState <- updatedStateGetOpt
                //   updatedStateGetOpt
                | _ ->
                  nextOpData.PreProcess(None)
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
          | other -> failwith (sprintf "Unhandled operation %A" other)
      | End -> 
        stop <- true
  
  (
    {
      OperationIndex = currentIndex; 
      Operations = nextOperations; 
      Element = renderedElement;
      ComponentState = updatedComponentState
    },
    nextEffects,
    nextLayoutEffects
  )

let render useRef useState useEffect useLayoutEffect delayedFunc props = 
  let componentStateRef: IRefValue<RefState<'props, 'state>> = useRef({
    Element = None;
    Operations = [||];
    OperationIndex = -1;
    ComponentState = None;
  })

  // trigger rerender by updating a state variable
  let state: IStateHook<string> = useState("asdf")
  let rerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)

  // Capture a result to return to the flow.
  let updateStateAt index newOpState = 
    // Ignore captures that occur when the operation index is less than the 
    // capture, since we might be rerendering something different.
    if index <= componentStateRef.current.OperationIndex then
      let op = componentStateRef.current.Operations.[index]
      let updatedOp = {op with State = newOpState}
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

  let wrapEffect index effect =
    let wrapUpdateState index stateUpdater = 
      if index <= componentStateRef.current.OperationIndex then
        let op = componentStateRef.current.Operations.[index]

        // let state =
        //   match op.Operation with 
        //   | Control({OperationType = StateSet}) -> 
        //     componentStateRef.current.ComponentState
        //   | _ -> op.State
        let updatedState = stateUpdater op.State
        // match op.Operation with 
        // | Control({OperationType = StateSet}) -> 
        //   componentStateRef.current <- {componentStateRef.current with ComponentState = updatedState}
        // | _ ->
        FSharp.Collections.Array.set
          componentStateRef.current.Operations
          index
          {op with State = updatedState}
        componentStateRef.current <- {componentStateRef.current with OperationIndex = index}
        runDisposers componentStateRef
        rerender ()
    let updateState = wrapUpdateState index
    let wrappedEffect _ =
      let disposer = effect updateState
      updateDisposer index disposer
      ()
    wrappedEffect

  componentStateRef.current <- getFirstOperation delayedFunc componentStateRef.current

  componentStateRef.current <- preprocessOperations componentStateRef.current props
  runDisposers componentStateRef

  let nextState, wrappedNextEffects, wrappedNextLayoutEffects = execute updateStateAt wrapEffect componentStateRef.current props

  componentStateRef.current <- nextState

  // run any effects.
  useEffect (
    fun () ->
      List.map (fun eff -> eff ()) wrappedNextEffects |> ignore
      ()
    )

  // run any layout effect.
  useLayoutEffect (
    fun () ->
      List.map (fun eff -> eff ()) wrappedNextEffects |> ignore
      ()
    )

  // Render current element
  match componentStateRef.current.Element with
    | Some(element) -> 
      element
    | None -> null

type HacnBuilder(render) = 
  member _.Bind(operation, f) = 
    bind operation f
  // member _.Bind(element: ReactElement, f) = bind Hacn.Operations.Render(element) f
  member _.Zero() = 
    zero()
  member _.Delay(f) = f
  member _.Run(delayedFunc) =
    React.functionComponent<'props>(
      fun (props: 'props) -> 
        render delayedFunc props
    ) 

let react = HacnBuilder((render Hooks.useRef Hooks.useState Hooks.useEffect Feliz.React.useLayoutEffect))