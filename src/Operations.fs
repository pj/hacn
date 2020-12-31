module Hacn.Operations
open Fable.React
open Feliz
open FSharp.Core
open Fable.Core.Util
open Fable.Core

[<ImportMember("./propsCompare.js")>]
let shallowEqualObjects (x: obj) (y: obj): bool = jsNative

type PropsOperationState<'props> =
  {
    Props: 'props;
    PrevProps: 'props option;
  }

let Props<'props when 'props: equality> : Operation<'props, 'props> =
  PerformProps( 
    {
      Changed = fun (prevProps: 'props option) (props: 'props) -> 
        match prevProps with
        | None -> true
        | Some(prevProps) -> 
          // let castPrevProps: 'props = unbox prevProps
          // let castProps: 'props = unbox props
          // let asdf = JSON.stringify castProps
          // let qwer = JSON.stringify castPrevProps
          // console.log (
          //   sprintf 
          //     "castProps: %A, castPrevProps %A, different %b, shallow equal %b" 
          //     asdf 
          //     qwer 
          //     (castProps <> castPrevProps)
          //     (not (shallowEqualObjects castProps castPrevProps))
          //   )
          not (shallowEqualObjects props prevProps)
          // if (castProps <> castPrevProps) then
      // Execute = fun props -> 
      //   PerformContinue(
      //     {
      //       Element = None; 
      //       Effect = None;
      //       LayoutEffect = None
      //     }, 
      //     unbox props
      //   )
    }
  )

type StateContainer<'state> = 
  {
    Updated: bool;
    ComponentState: 'state;
  }

let State<'state> (initialState: 'state) = 
  Perform({
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
          PreProcess = fun _ -> None;
          GetResult = fun _ _ -> 
            let stateSetEffect () =
              captureResult (fun _ -> (Replace({Updated = true; ComponentState = newState} :> obj)))
              None
            PerformWait(
              {
                Element = None
                Effect = Some(stateSetEffect)
                LayoutEffect = None
                OperationState = Keep
              }
            )
        })
      match operationState with
      | Some(currentState) -> 
        let castCurrentState: StateContainer<'state> = unbox currentState
        PerformContinue(
          {
            Element = None; 
            Effect = None;
            LayoutEffect = None
            OperationState = Keep
          }, 
          (castCurrentState.ComponentState, StateSetOperation)
        )
      | None -> failwith "Should not happen"
  })

let createCombinedDispose disposeOpt1 disposeOpt2 =
  let combinedDisposer underlyingState =
    let currentState: (obj option) array = 
      match underlyingState with 
      | None -> FSharp.Collections.Array.create 2 None
      | Some(x) -> unbox x
    // TODO: Fix error handling and wait
    match disposeOpt1 with 
    | Some(dispose) -> 
      let disposeResult = dispose currentState.[0]
      match disposeResult with
      | Replace(state) ->
        FSharp.Collections.Array.set
          currentState
          0
          (Some(state))
      | Erase -> 
        FSharp.Collections.Array.set
          currentState
          0
          None
      | Keep -> ()
    | _ -> ()

    match disposeOpt2 with
    | Some(dispose) -> 
      let disposeResult = dispose currentState.[1]
      match disposeResult with
      | Replace(state) ->
        FSharp.Collections.Array.set
          currentState
          1
          (Some(state))
      | Erase -> 
        FSharp.Collections.Array.set
          currentState
          1
          None
      | Keep -> ()
    | _ -> ()

    Replace(currentState :> obj)
      
  match disposeOpt1, disposeOpt2 with
  | None, None -> None
  | _ -> Some(combinedDisposer)

let createCombinedEffect eff1Opt eff2Opt = 
  match eff1Opt, eff2Opt with
  | None, None -> None
  | eff1Opt, eff2Opt ->
    let combinedEffect () =
      let disposeOpt1 = 
        match eff1Opt with
          | Some(eff1) ->
            eff1 ()
          | _ -> None
      
      let disposeOpt2 =  
        match eff2Opt with
          | Some(eff2) ->
            eff2 ()
          | _ -> None

      createCombinedDispose disposeOpt1 disposeOpt2
    Some(combinedEffect)

let getElement elementOpt1 elementOpt2 =
  match elementOpt1, elementOpt2 with
  | Some(element1), Some(_) -> Some(element1)
  | Some(element1), None -> Some(element1)
  | None, Some(element2) -> Some(element2)
  | None, None -> None
      
let indexedCapture capture index stateUpdater = 
  let indexUpdater underlyingState = 
    let castUnderlyingState: (obj option) array = 
      match underlyingState with
      | Some(x) -> unbox x
      | None -> [|None; None|]
    let updatedState = stateUpdater castUnderlyingState.[index]
    match updatedState with
    | Replace(state) ->
      FSharp.Collections.Array.set
        castUnderlyingState
        index
        (Some state)
      Replace (castUnderlyingState)
    | Erase -> 
      FSharp.Collections.Array.set
        castUnderlyingState
        index
        None
      Replace (castUnderlyingState)
    | keepOrException -> keepOrException
  capture indexUpdater

let Wait2 op1 op2 = 
  Perform({ 
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
          pd1.GetResult (indexedCapture capture 0) opState1
        | _ -> failwith "Can only work with Perform operations"
      let opState2 = underlyingStateCast.[1]
      let opResult2 = 
        match op2 with
        | Perform(pd2) -> 
          pd2.GetResult (indexedCapture capture 1) opState2
        | _ -> failwith "Can only work with Perform operations"
      
      match opResult1, opResult2 with
      | PerformWait({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}), 
        PerformWait({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}) ->
        PerformWait(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          }
        )

      | PerformWait({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}), 
        PerformContinue({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}, _) ->
        PerformWait(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          }
        )

      | PerformContinue({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}, _), 
        PerformWait({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}) ->
        PerformWait(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          }
        )

      | PerformContinue({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}, ret1), 
        PerformContinue({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}, ret2) ->
        PerformContinue(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          },
          (ret1, ret2)
        )
  })

let WaitAny2 op1 op2 = 
  Perform({ 
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
          pd1.GetResult (indexedCapture capture 0) opState1
        | _ -> failwith "Can only work with Perform operations"
      let opState2 = underlyingStateCast.[1]
      let opResult2 = 
        match op2 with
        | Perform(pd2) -> 
          pd2.GetResult (indexedCapture capture 1) opState2
        | _ -> failwith "Can only work with Perform operations"
      
      match opResult1, opResult2 with
      | PerformWait({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}), 
        PerformWait({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}) ->
        PerformWait(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          }
        )

      | PerformWait({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}), 
        PerformContinue({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}, ret2) ->
        PerformContinue(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          },
          (None, Some(ret2))
        )

      | PerformContinue({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}, ret1), 
        PerformWait({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}) ->
        PerformContinue(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          },
          (Some(ret1), None)
        )

      | PerformContinue({Element = element1; Effect = effect1; LayoutEffect = layoutEffect1}, ret1), 
        PerformContinue({Element = element2; Effect = effect2; LayoutEffect = layoutEffect2}, ret2) ->
        PerformContinue(
          {
            Element = (getElement element1 element2)
            Effect = (createCombinedEffect effect1 effect2)
            LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
            OperationState = Keep
          },
          (Some(ret1), Some(ret2))
        )
  })

let WaitAny3 = End

// Return a series of suspended effect results as a stream
// let Next = End

// Return a series of suspended effect results as a stream (in any order)
let NextAny = End

// time operations
let Timeout time = 
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      match operationState with
      | Some(_) -> 
        PerformContinue(
          {
            Element = None; 
            Effect = None;
            LayoutEffect = None
            OperationState = Keep
          }, 
          ()
        )
      | None -> 
        let timeoutEffect () =
          let timeoutCallback () =
            let updateState _ = 
              Replace(() :> obj)
            captureResult updateState
          let timeoutID = Fable.Core.JS.setTimeout timeoutCallback time

          Some(fun _ -> 
            Fable.Core.JS.clearTimeout timeoutID
            Keep
          )
          
        PerformWait(
          {
            Element = None
            Effect = Some(timeoutEffect)
            LayoutEffect = None
            OperationState = Keep
          }
        )
  })

let Interval interval = 
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      match operationState with
      | Some(_) -> 
        PerformContinue(
          {
            Element = None
            Effect = None
            LayoutEffect = None
            OperationState = Keep
          },
          ()
        )
      | None -> 
        let timeoutEffect () =
          let timeoutCallback () =
            let updateState _ = 
              Replace(() :> obj)
            captureResult updateState
          let timeoutID = Fable.Core.JS.setInterval timeoutCallback interval

          Some(fun _ -> 
            Fable.Core.JS.clearInterval timeoutID
            Keep
          )
          
        PerformWait(
          {
            Element = None
            Effect = Some(timeoutEffect)
            LayoutEffect = None
            OperationState = Keep
          }
        )

  })

// fetch data.
let Fetch = End

// Core operations
let ContextCore<'returnType when 'returnType : equality> (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
  Perform({ 
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
      | Some(existingContext) -> 
        PerformContinue(
          {
            Element = None; 
            Effect = None;
            LayoutEffect = None
            OperationState = Keep
          }, 
          existingContext
        )
      | None -> failwith "should not happen"
  })

let Context context = ContextCore Hooks.useContext context

let Ref (initialValue: 'returnType option) =
  Perform({ 
    PreProcess = fun operationState -> 
      let currentRef = Hooks.useRef(initialValue)
      let castOperationState: 'returnType option = unbox operationState
      match castOperationState with
      | Some(_) -> None
      | None -> Some(currentRef :> obj)
    GetResult = fun _ operationState -> 
      let castOperationState: (('returnType option) IRefValue) option = unbox operationState
      match castOperationState with
      | Some(existingRef) -> 
        PerformContinue(
          {
            Element = None
            Effect = None
            LayoutEffect = None
            OperationState = Keep
          }, 
          existingRef
        )
      | None -> failwith "should not happen"
  })

// Call a function passed in through props in an effect.
let Call callable = 
  Perform({ 
    PreProcess = fun _ -> None
    GetResult = fun _ __ -> 
      let callCallable _ =
        callable ()
        None
      PerformContinue(
        {
          Element = None
          Effect = Some(callCallable)
          LayoutEffect = None
          OperationState = Keep
        }, 
        ()
      )
  })

let CallLayout callable = 
  Perform({ 
    PreProcess = fun _ -> None
    GetResult = fun _ __ -> 
      let callCallable _ =
        callable ()
        None
      PerformContinue(
        {
          Element = None; 
          Effect = None;
          LayoutEffect = Some(callCallable)
          OperationState = Keep
        }, 
        ()
      )
  })

// Error handling

// Handling events