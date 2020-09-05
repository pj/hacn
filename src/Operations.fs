module Hacn.Operations
open Fable.React
open Fable.React.Props
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Dyn
open Browser.Types

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
        let castPropsOpState: PropsOperationState<obj> = explicitConvert propsOpState
        match castPropsOpState.PrevProps with
        | None -> operationState
        | Some(prevProps) -> 
          let castPrevProps: 'props = explicitConvert prevProps
          let castProps: 'props = explicitConvert castPropsOpState.Props
          if castProps <> castPrevProps then
            operationState
          else
            None
    GetResult = fun _ operationState -> 
      match operationState with
      | None -> failwith "Should not happen"
      | Some(propsOpState) ->
        let props: 'props = propsOpState?Props
        InvokeReturn(props)
  })

type Captures<'returnType> =
  // | CaptureCut of (ClipboardEvent -> 'returnType)
  // | CapturePaste of (ClipboardEvent -> 'returnType)
  // | CaptureCompositionEnd of (CompositionEvent -> 'returnType)
  // | CaptureCompositionStart of (CompositionEvent -> 'returnType)
  // | CaptureCopy of (ClipboardEvent -> 'returnType)
  // | CaptureCompositionUpdate of (CompositionEvent -> 'returnType)
  // | CaptureFocus of (FocusEvent -> 'returnType)
  // | CaptureBlur of (FocusEvent -> 'returnType)
  | CaptureChange of (Event -> 'returnType)
  // | CaptureInput of (Event -> 'returnType)
  // | CaptureSubmit of (Event -> 'returnType)
  // | CaptureReset of (Event -> 'returnType)
  // | CaptureLoad of (Event -> 'returnType)
  // | CaptureError of (Event -> 'returnType)
  // | CaptureKeyDown of (KeyboardEvent -> 'returnType)
  // | CaptureKeyPress of (KeyboardEvent -> 'returnType)
  // | CaptureKeyUp of (KeyboardEvent -> 'returnType)
  // | CaptureAbort of (Event -> 'returnType)
  // | CaptureCanPlay of (Event -> 'returnType)
  // | CaptureCanPlayThrough of (Event -> 'returnType)
  // | CaptureDurationChange of (Event -> 'returnType)
  // | CaptureEmptied of (Event -> 'returnType)
  // | CaptureEncrypted of (Event -> 'returnType)
  // | CaptureEnded of (Event -> 'returnType)
  // | CaptureLoadedData of (Event -> 'returnType)
  // | CaptureLoadedMetadata of (Event -> 'returnType)
  // | CaptureLoadStart of (Event -> 'returnType)
  // | CapturePause of (Event -> 'returnType)
  // | CapturePlay of (Event -> 'returnType)
  // | CapturePlaying of (Event -> 'returnType)
  // | CaptureProgress of (Event -> 'returnType)
  // | CaptureRateChange of (Event -> 'returnType)
  // | CaptureSeeked of (Event -> 'returnType)
  // | CaptureSeeking of (Event -> 'returnType)
  // | CaptureStalled of (Event -> 'returnType)
  // | CaptureSuspend of (Event -> 'returnType)
  // | CaptureTimeUpdate of (Event -> 'returnType)
  // | CaptureVolumeChange of (Event -> 'returnType)
  // | CaptureWaiting of (Event -> 'returnType)
  | CaptureClick of (MouseEvent -> 'returnType)
  // | CaptureContextMenu of (MouseEvent -> 'returnType)
  // | CaptureDoubleClick of (MouseEvent -> 'returnType)
  // | CaptureDrag of (DragEvent -> 'returnType)
  // | CaptureDragEnd of (DragEvent -> 'returnType)
  // | CaptureDragEnter of (DragEvent -> 'returnType)
  // | CaptureDragExit of (DragEvent -> 'returnType)
  // | CaptureDragLeave of (DragEvent -> 'returnType)
  // | CaptureDragOver of (DragEvent -> 'returnType)
  // | CaptureDragStart of (DragEvent -> 'returnType)
  // | CaptureDrop of (DragEvent -> 'returnType)
  // | CaptureMouseDown of (MouseEvent -> 'returnType)
  // | CaptureMouseEnter of (MouseEvent -> 'returnType)
  // | CaptureMouseLeave of (MouseEvent -> 'returnType)
  // | CaptureMouseMove of (MouseEvent -> 'returnType)
  // | CaptureMouseOut of (MouseEvent -> 'returnType)
  // | CaptureMouseOver of (MouseEvent -> 'returnType)
  // | CaptureMouseUp of (MouseEvent -> 'returnType)
  // | CaptureSelect of (Event -> 'returnType)
  // | CaptureTouchCancel of (TouchEvent -> 'returnType)
  // | CaptureTouchEnd of (TouchEvent -> 'returnType)
  // | CaptureTouchMove of (TouchEvent -> 'returnType)
  // | CaptureTouchStart of (TouchEvent -> 'returnType)
  // | CaptureScroll of (UIEvent -> 'returnType)
  // | CaptureWheel of (WheelEvent -> 'returnType)
  // | CaptureAnimationStart of (AnimationEvent -> 'returnType)
  // | CaptureAnimationEnd of (AnimationEvent -> 'returnType)
  // | CaptureAnimationIteration of (AnimationEvent -> 'returnType)
  // | CaptureTransitionEnd of (TransitionEvent -> 'returnType)

let Render<'returnType> element props (captures: Captures<'returnType> list) (children: ReactElement seq) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      match operationState with
      | Some(result) -> 
        let castReturn: 'returnType = explicitConvert result
        InvokeReturn(castReturn)
      | _ ->
        let convertCaptureToProp capture =
          let x = 
            match capture with
            | CaptureChange (changeFunc) ->
              OnChange(
                fun event -> 
                  let transformedEvent = changeFunc event
                  captureResult (Some(transformedEvent :> obj))
                  ()
              )
            | CaptureClick(clickFunc) ->
              OnClick(
                fun event -> 
                  let transformedEvent = clickFunc event
                  captureResult (Some(transformedEvent :> obj))
                  ()
              )
          x :> IHTMLProp

        let captureProps = List.map convertCaptureToProp captures
        InvokeRender (element (List.append props captureProps) children)
  })

type StateContainer<'state> = 
  {
    Updated: bool;
    ComponentState: 'state option;
  }

let Get<'state> (initialState: 'state option) =
  Perform({ 
    OperationType = StateGet;
    PreProcess = fun operationState -> 
      let castOperationState: StateContainer<'state> option = explicitConvert operationState
      match castOperationState with 
      | None -> 
        Some(
          {
            Updated = false; 
            ComponentState = initialState;
          } :> obj
        )
      | Some(currentState) -> 
        if currentState.Updated then
          Some({currentState with Updated = false} :> obj)
        else 
          None
    GetResult = fun _ operationState -> 
      let stateCast: 'state option = operationState?State
      match stateCast with
      | Some(state) -> InvokeReturn(state)
      | None -> failwith "Please set state before calling Get()"
  })

let Set<'state>(newState: 'state) =
  Perform({
    OperationType = StateSet;
    PreProcess = fun _ -> None;
    GetResult = fun _ _ -> 
      InvokeEffect(
        fun rerender -> 
          let updateState _ = 
            Some(
              {
                Updated = true;
                ComponentState = Some(newState)
              } :> obj
            )

          rerender updateState
          None
      )
  })

let createCombinedEffect eff1Opt eff2Opt = 
  let combinedEffect render =
    let indexedRerender stateLength index stateUpdater = 
      let indexedStateUpdater underlyingState =
        let currentState: (obj option) array = 
          match underlyingState with 
          | None -> Array.create stateLength None
          | Some(x) -> explicitConvert x
        let updatedState = stateUpdater (currentState.[index])
        Array.set
          currentState
          index
          updatedState
        Some(currentState :> obj)
      render indexedStateUpdater

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

    Some(
      fun () -> 
        match disposeOpt1 with 
        | Some(dispose) -> dispose ()
        | _ -> ()

        match disposeOpt2 with
        | Some(dispose) -> dispose ()
        | _ -> ()
        ()
    )
  combinedEffect
      

let Wait2 op1 op2 = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    GetResult = fun capture operationState -> 
      let underlyingStateCast: (obj option) array = 
        match operationState with
        | Some(x) -> explicitConvert x
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
      // | InvokeRender(ren1), InvokeEffect(eff2) -> 
      //   InvokeWait
      // | InvokeEffect(eff1), InvokeRender(ren2) -> 
      //   InvokeWait
      | InvokeEffect(eff1), InvokeEffect(eff2) -> 
        InvokeEffect(createCombinedEffect (Some(eff1)) (Some(eff2)))
      | InvokeEffect(eff1), InvokeReturn(_) -> 
        InvokeEffect(createCombinedEffect (Some(eff1)) None)
      | InvokeReturn(_), InvokeEffect(eff2) -> 
        InvokeEffect(createCombinedEffect None (Some(eff2)))
      | InvokeWait, _ -> InvokeWait
      | _, InvokeWait -> InvokeWait
      | InvokeReturn(ret1), InvokeReturn(ret2) -> InvokeReturn((ret1, ret2))
      | _ -> failwith (sprintf "Incorrect lifecycle or non-waitable effect: %A %A\n" opResult1 opResult2)
  })

let WaitAny2 op1 op2 = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None
    GetResult = fun capture operationState -> 
      let underlyingStateCast: (obj option) array = 
        match operationState with
        | Some(x) -> explicitConvert x
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
      | InvokeRender(ren1), InvokeEffect(eff2) -> 
        InvokeBoth(ren1, createCombinedEffect None (Some(eff2)))
      | InvokeEffect(eff1), InvokeRender(ren2) -> 
        InvokeBoth(ren2, createCombinedEffect (Some(eff1)) None)
      | InvokeEffect(eff1), InvokeEffect(eff2) -> 
        InvokeEffect(createCombinedEffect (Some(eff1)) (Some(eff2)))
      | InvokeWait, InvokeWait -> 
        InvokeWait
      | InvokeReturn(ret1), InvokeWait -> 
        InvokeReturn((Some(ret1), None))
      | InvokeWait, InvokeReturn(ret2) -> 
        InvokeReturn((None, Some(ret2)))
      | InvokeReturn(ret1), InvokeReturn(ret2) -> 
        InvokeReturn((Some(ret1), Some(ret2)))
      | InvokeRender(_), InvokeReturn(ret2) -> 
        InvokeReturn((None, Some(ret2)))
      | InvokeReturn(ret1), InvokeRender(_) -> 
        InvokeReturn((Some(ret1), None))
      | InvokeRender(ren1), InvokeWait -> 
        InvokeRender(ren1)
      | InvokeWait, InvokeRender(ren2) -> 
        InvokeRender(ren2)
      | _ -> failwith "Incorrect lifecycle or non-waitable effect"
  })

let WaitAny3 = End

// Return a series of suspended effect results as a stream
let Next = End

// Return a series of suspended effect results as a stream (in any order)
let NextAny = End

// time operations
let Timeout = End
let Interval = End

// fetch data.
let Fetch = End

// Core operations
let ContextCore<'returnType when 'returnType : equality> (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun operationState -> 
      let currentContext = useContext(context)
      let castOperationState: 'returnType option = explicitConvert operationState
      match castOperationState with
      | Some(existingContext) -> 
        if existingContext <> currentContext then
          Some(currentContext :> obj)
        else 
          None
      | None -> Some(currentContext :> obj)
    GetResult = fun _ operationState -> 
      let castOperationState: 'returnType option = explicitConvert operationState
      match castOperationState with
      | Some(existingContext) -> InvokeReturn(existingContext)
      | None -> failwith "should not happen"
  })

let Context context = ContextCore Hooks.useContext context

let Ref = End

// Call a function passed in through props in an effect.
let Call = End

// Don't auto-dispose an element?
let Background = End

// Error handling

// Handling events