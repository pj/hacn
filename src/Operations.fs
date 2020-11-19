module Hacn.Operations
open Fable.React
// open Utils
open Feliz
open Fable.Core.JsInterop
open FSharp.Core
open Fable.Core.JS
open Fable.Core.Util
open Fable.Core
open Fable.Core.JsInterop

[<ImportMember("./propsCompare.js")>]
let shallowEqualObjects (x: obj) (y: obj): bool = jsNative

type PropsOperationState<'props> =
  {
    Props: 'props;
    PrevProps: 'props option;
  }

let Props<'props when 'props: equality> =
  Perform({ 
    OperationType = PropsOperation;
    PreProcess = fun (operationState) -> 
      match operationState with 
      | None -> failwith "Should never happen"
      | Some(propsOpState) ->
        let castPropsOpState: PropsOperationState<obj> = unbox propsOpState
        match castPropsOpState.PrevProps with
        | None -> operationState
        | Some(prevProps) -> 
          let castPrevProps: 'props = unbox prevProps
          let castProps: 'props = unbox castPropsOpState.Props
          let asdf = JSON.stringify castProps
          let qwer = JSON.stringify castPrevProps
          // console.log (
          //   sprintf 
          //     "castProps: %A, castPrevProps %A, different %b, shallow equal %b" 
          //     asdf 
          //     qwer 
          //     (castProps <> castPrevProps)
          //     (not (shallowEqualObjects castProps castPrevProps))
          //   )
          if not (shallowEqualObjects castProps castPrevProps) then
          // if (castProps <> castPrevProps) then
            operationState
          else
            None
    GetResult = fun _ operationState -> 
      match operationState with
      | None -> failwith "Should not happen"
      | Some(propsOpState) ->
        let props: 'props = propsOpState?Props
        InvokeContinue(None, None, None, props)
  })

// type prop with
//   static member inline captureClick = Interop.mkAttr "captureOnClick" None
//   static member inline captureChange = Interop.mkAttr "captureOnChange" None

let Render (element: IReactProperty list -> ReactElement) (props: IReactProperty list) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun _ __ -> 
      InvokeWait(Some(element props), None, None)
  })

let RenderCapture<'returnType> captureElement =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      let captureResultInternal v =
        captureResult (Some(v))
      let eraseCapturedResult _ =
        Some(fun _ -> None)
      match operationState with
      | Some(result) -> 
        let castReturn: 'returnType = unbox result
        InvokeContinue(Some(captureElement captureResultInternal), Some(eraseCapturedResult), None, castReturn)
      | _ ->
        InvokeWait(Some(captureElement captureResultInternal), None, None)
  })

let RenderContinue element (props: IReactProperty list) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      let renderedElement = element props
      InvokeContinue(Some(renderedElement), None, None, ())
  })

type StateContainer<'state> = 
  {
    Updated: bool;
    ComponentState: 'state;
  }

// let Get<'state> (initialState: 'state) =
//   Perform({ 
//     OperationType = StateGet;
//     PreProcess = fun operationState -> 
//       match operationState with 
//       | None -> 
//         Some(
//           {
//             Updated = false; 
//             ComponentState = initialState;
//           } :> obj
//         )
//       | Some(currentState) -> 
//         let castCurrentState: StateContainer<'state> = unbox currentState
//         if castCurrentState.Updated then
//           Some({castCurrentState with Updated = false} :> obj)
//         else 
//           None
//     GetResult = fun _ operationState -> 
//       match operationState with
//       | Some(state) -> 
//         let castCurrentState: StateContainer<'state> = unbox state
//         InvokeContinue(None, None, castCurrentState.ComponentState)
//       | None -> failwith "Please set state before calling Get()"
//   })

let Get<'state> (initialState: 'state) = 
  Perform({
    OperationType = NotCore;
    PreProcess = fun operationState -> 
      match operationState with
      | None -> Some({
          ComponentState = initialState
          Updated = false
        } :> obj)
      | Some(currentState) -> 
        let castCurrentState: StateContainer<'state> = unbox currentState
        if castCurrentState.Updated then
          Some({castCurrentState with Updated = false} :> obj)
        else 
          None
    GetResult = fun captureResult operationState -> 
      let StateSetOperation (newState: 'state) : Operation<obj, unit> = 
        Perform({
          OperationType = NotCore;
          PreProcess = fun _ -> None;
          GetResult = fun _ _ -> 
            let stateSetEffect rerender =
              let updateState _ =
                captureResult (Some({Updated = true; ComponentState = newState} :> obj))
                None
              rerender updateState
              None
            InvokeWait(None, Some(stateSetEffect), None)
        })
      match operationState with
      | Some(currentState) -> 
        let castCurrentState: StateContainer<'state> = unbox currentState
        InvokeContinue(None, None, None, (castCurrentState.ComponentState, StateSetOperation))
      | None -> failwith "Should not happen"
  })

// let Set<'state> (newState: 'state) : Operation<obj, unit> =
//   Perform({
//     OperationType = StateSet;
//     PreProcess = fun _ -> None;
//     GetResult = fun _ _ -> 
//       let stateSetter rerender =
//         let updateState _ = 
//           Some(
//             {
//               Updated = true;
//               ComponentState = newState
//             } :> obj
//           )

//         rerender updateState
//         None
//       InvokeWait(None, Some(stateSetter))
//   })

let createCombinedDispose disposeOpt1 disposeOpt2 =
  let combinedDisposer underlyingState =
    let currentState: (obj option) array = 
      match underlyingState with 
      | None -> FSharp.Collections.Array.create 2 None
      | Some(x) -> unbox x
    match disposeOpt1 with 
    | Some(dispose) -> 
      let disposeResult = dispose currentState.[0]
      FSharp.Collections.Array.set
        currentState
        0
        disposeResult
    | _ -> ()

    match disposeOpt2 with
    | Some(dispose) -> 
      let disposeResult = dispose currentState.[1]
      FSharp.Collections.Array.set
        currentState
        1
        disposeResult
    | _ -> ()

    Some(currentState :> obj)
      
  match disposeOpt1, disposeOpt2 with
  | None, None -> None
  | _ -> Some(combinedDisposer)

let createCombinedEffect eff1Opt eff2Opt = 
  match eff1Opt, eff2Opt with
  | None, None -> None
  | eff1Opt, eff2Opt ->
    let combinedEffect rerender =
      let indexedRerender stateLength index stateUpdater = 
        let indexedStateUpdater underlyingState =
          let currentState: (obj option) array = 
            match underlyingState with 
            | None -> FSharp.Collections.Array.create stateLength None
            | Some(x) -> unbox x
          let updatedState = stateUpdater (currentState.[index])
          FSharp.Collections.Array.set
            currentState
            index
            updatedState
          Some(currentState :> obj)
        rerender indexedStateUpdater

      let disposeOpt1 = 
        match eff1Opt with
          | Some(eff1) ->
            eff1 (indexedRerender 2 0)
          | _ -> None
      
      let disposeOpt2 = 
        match eff2Opt with
          | Some(eff2) ->
            eff2 (indexedRerender 2 1)
          | _ -> None

      createCombinedDispose disposeOpt1 disposeOpt2
    Some(combinedEffect)

let getElement elementOpt1 elementOpt2 =
  match elementOpt1, elementOpt2 with
  | Some(element1), Some(_) -> Some(element1)
  | Some(element1), None -> Some(element1)
  | None, Some(element2) -> Some(element2)
  | None, None -> None

let Wait2 op1 op2 = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    GetResult = fun capture operationState -> 
      let underlyingStateCast: (obj option) array = 
        match operationState with
        | Some(x) -> unbox x
        | None -> [|None; None|]
      let opState1 = underlyingStateCast.[0]
      let opResult1 = 
        match op1 with
        | Perform(pd1) -> 
          pd1.GetResult capture opState1
        | _ -> failwith "Can only work with Perform operations"
      let opState2 = underlyingStateCast.[1]
      let opResult2 = 
        match op2 with
        | Perform(pd2) -> 
          pd2.GetResult capture opState2
        | _ -> failwith "Can only work with Perform operations"
      
      match opResult1, opResult2 with
      | InvokeWait(element1, effect1, layoutEffect1), InvokeWait(element2, effect2, layoutEffect2) ->
        InvokeWait(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2), 
          (createCombinedEffect layoutEffect1 layoutEffect2)
        )

      | InvokeWait(element1, effect1, layoutEffect1), InvokeContinue(element2, effect2, layoutEffect2, _) ->
        InvokeWait(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2), 
          (createCombinedEffect layoutEffect1 layoutEffect2)
        )

      | InvokeContinue(element1, effect1, layoutEffect1, _), InvokeWait(element2, effect2, layoutEffect2) ->
        InvokeWait(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2),
          (createCombinedEffect layoutEffect1 layoutEffect2)
        )

      | InvokeContinue(element1, effect1, layoutEffect1, ret1), InvokeContinue(element2, effect2, layoutEffect2, ret2) ->
        InvokeContinue(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2),
          (createCombinedEffect layoutEffect1 layoutEffect2),
          (ret1, ret2)
        )
  })

let WaitAny2 op1 op2 = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    GetResult = fun capture operationState -> 
      let underlyingStateCast: (obj option) array = 
        match operationState with
        | Some(x) -> unbox x
        | None -> [|None; None|]
      let opState1 = underlyingStateCast.[0]
      let opResult1 = 
        match op1 with
        | Perform(pd1) -> 
          pd1.GetResult capture opState1
        | _ -> failwith "Can only work with Perform operations"
      let opState2 = underlyingStateCast.[1]
      let opResult2 = 
        match op2 with
        | Perform(pd2) -> 
          pd2.GetResult capture opState2
        | _ -> failwith "Can only work with Perform operations"
      
      match opResult1, opResult2 with
      | InvokeWait(element1, effect1, layoutEffect1), InvokeWait(element2, effect2, layoutEffect2) ->
        InvokeWait(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2),
          (createCombinedEffect layoutEffect1 layoutEffect2)
        )

      | InvokeWait(element1, effect1, layoutEffect1), InvokeContinue(element2, effect2, layoutEffect2, ret2) ->
        InvokeContinue(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2), 
          (createCombinedEffect layoutEffect1 layoutEffect2),
          (None, Some(ret2))
        )

      | InvokeContinue(element1, effect1, layoutEffect1, ret1), InvokeWait(element2, effect2, layoutEffect2) ->
        InvokeContinue(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2), 
          (createCombinedEffect layoutEffect1 layoutEffect2),
          (Some(ret1), None)
        )

      | InvokeContinue(element1, effect1, layoutEffect1, ret1), InvokeContinue(element2, effect2, layoutEffect2, ret2) ->
        InvokeContinue(
          (getElement element1 element2), 
          (createCombinedEffect effect1 effect2), 
          (createCombinedEffect layoutEffect1 layoutEffect2),
          (Some(ret1), Some(ret2))
        )
  })

let WaitAny3 = End

// Return a series of suspended effect results as a stream
let Next = End

// Return a series of suspended effect results as a stream (in any order)
let NextAny = End

// time operations
let Timeout time = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun _ operationState -> 
      match operationState with
      | Some(_) -> InvokeContinue(None, None, None, ())
      | None -> 
        let timeoutEffect rerender =
          let timeoutCallback () =
            let updateState _ = 
              Some(() :> obj)
            rerender updateState
          let timeoutID = Fable.Core.JS.setTimeout timeoutCallback time

          Some(fun _ -> 
            Fable.Core.JS.clearTimeout timeoutID
            None
          )
          
        InvokeWait(None, Some(timeoutEffect), None)

  })

let Interval interval = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun _ operationState -> 
      match operationState with
      | Some(_) -> InvokeContinue(None, None, None, ())
      | None -> 
        let timeoutEffect rerender =
          let timeoutCallback () =
            let updateState _ = 
              Some(() :> obj)
            rerender updateState
          let timeoutID = Fable.Core.JS.setInterval timeoutCallback interval

          Some(fun _ -> 
            Fable.Core.JS.clearInterval timeoutID
            None
          )
          
        InvokeWait(None, Some(timeoutEffect), None)

  })

// fetch data.
let Fetch = End

// Core operations
let ContextCore<'returnType when 'returnType : equality> (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun operationState -> 
      let currentContext = useContext(context)
      let castOperationState: 'returnType option = unbox operationState
      match castOperationState with
      | Some(existingContext) -> 
        if existingContext <> currentContext then
          Some(currentContext :> obj)
        else 
          None
      | None -> Some(currentContext :> obj)
    GetResult = fun _ operationState -> 
      let castOperationState: 'returnType option = unbox operationState
      match castOperationState with
      | Some(existingContext) -> InvokeContinue(None, None, None, existingContext)
      | None -> failwith "should not happen"
  })

let Context context = ContextCore Hooks.useContext context

let Ref (initialValue: 'returnType option) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun operationState -> 
      let currentRef = Hooks.useRef(initialValue)
      let castOperationState: 'returnType option = unbox operationState
      match castOperationState with
      | Some(_) -> None
      | None -> Some(currentRef :> obj)
    GetResult = fun _ operationState -> 
      let castOperationState: (('returnType option) IRefValue) option = unbox operationState
      match castOperationState with
      | Some(existingRef) -> InvokeContinue(None, None, None, existingRef)
      | None -> failwith "should not happen"
  })

// Call a function passed in through props in an effect.
let Call callable = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    GetResult = fun _ __ -> 
      let callCallable _ =
        callable ()
        None
      InvokeContinue(None, Some(callCallable), None, ())
  })

// Don't auto-dispose an element?
let Background = End

// Error handling

// Handling events