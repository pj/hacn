module Hacn.Operations

open Fable.React
open Feliz
open FSharp.Core
open Fable.Core.Util
open Fable.Core

[<ImportMember("./propsCompare.js")>]
let shallowEqualObjects (x: obj) (y: obj) : bool = jsNative

type PropsOperationState<'props> =
  { Props: 'props
    PrevProps: 'props option }

let Props<'returnType > =
  let mutable prevProps = None
  Operation({
      Run = 
        fun _ props ->
          let propsHook props =
            let unboxedProps = unbox<'returnType> props
            match prevProps with
            | None -> Some(unboxedProps)
            | Some (prevPropsValue) ->
              if not (shallowEqualObjects props prevPropsValue) then
                prevProps <- Some (unboxedProps)
                Some (unboxedProps)
              else
                None
          
          OperationContinue({
            ReturnValue = (unbox<'returnType> props)
            Element = None
            Effect = None
            LayoutEffect = None
            Hook = Some(propsHook)
          })
    }
  )

type StateContainer<'state> =
  { Updated: bool
    ComponentState: 'state }

let State<'state> (initialState: 'state) =
  let mutable prevState = None
  Operation(
    { 
      Run =
        fun () ->
          let state = Hooks.useState(initialState)

          let StateSetOperation setState (newState: 'state) : Builder<unit> =
            Operation (
              { 
                Run =
                  fun _ __ ->
                    let stateSetEffect () =
                      setState newState
                      None

                    OperationWait (
                      { Element = None
                        Effect = Some (stateSetEffect)
                        LayoutEffect = None
                        Hook = None 
                      }
                      ) 
                }
            )

          let stateHook _ =
            let stateHook = Hooks.useState(initialState)
            match prevState with
            | Some(prevStateContents) ->
              if (not (shallowEqualObjects (stateHook.current) prevStateContents)) then
                prevState <- Some(stateHook.current)
                Some ((stateHook.current, StateSetOperation stateHook.update)) 
              else
                None
            | None -> 
              prevState <- Some(stateHook.current)
              Some ((stateHook.current, StateSetOperation stateHook.update))

          OperationContinue(
            { 
              ReturnValue = (state.current, StateSetOperation state.update)
              Element = None
              Effect = None
              LayoutEffect = None
              Hook = Some(stateHook) 
              }
          )
    }
  )

let uFunc x = x ()

// let mapEither eff1Opt eff2Opt f =
//   match eff1Opt, eff2Opt with
//   | None, None -> None
//   | eff1Opt, eff2Opt ->
//     f eff1Opt eff2Opt

let createCombinedDispose disposeOpt1 disposeOpt2 =
  let combinedDisposer () =
    Option.iter uFunc disposeOpt1
    Option.iter uFunc disposeOpt2

  match disposeOpt1, disposeOpt2 with
  | None, None -> None
  | _ -> Some(combinedDisposer)

let createCombinedEffect eff1Opt eff2Opt =
  match eff1Opt, eff2Opt with
  | None, None -> None
  | eff1Opt, eff2Opt ->
      let combinedEffect () =
        let disposeOpt1 = Option.flatten (Option.map uFunc eff1Opt)
        let disposeOpt2 = Option.flatten (Option.map uFunc eff2Opt)

        createCombinedDispose disposeOpt1 disposeOpt2

      Some(combinedEffect)

let getElement elementOpt1 elementOpt2 =
  match elementOpt1, elementOpt2 with
  | Some (element1), Some (_) -> Some(element1)
  | Some (element1), None -> Some(element1)
  | None, Some (element2) -> Some(element2)
  | None, None -> None

// let indexedCapture capture index stateUpdater =
//   let indexUpdater underlyingState =
//     let castUnderlyingState : (obj option) array =
//       match underlyingState with
//       | Some (x) -> unbox x
//       | None -> [| None; None |]

//     let updatedState = stateUpdater castUnderlyingState.[index]

//     match updatedState with
//     | Replace (state) ->
//         FSharp.Collections.Array.set castUnderlyingState index (Some state)
//         Replace(castUnderlyingState)
//     | Erase ->
//         FSharp.Collections.Array.set castUnderlyingState index None
//         Replace(castUnderlyingState)
//     | keepOrException -> keepOrException

//   capture indexUpdater

let tuple2SetResult waitRef setResult index =
  fun returnValue -> 
    let existingFirst, existingSecond = !waitRef
    if index = 0 then
      waitRef := (Some (returnValue), existingSecond)
    else
      waitRef := (existingFirst, Some (returnValue))
    let first, second = !waitRef

    let mappedWait = Option.map2 (fun a b -> (a, b)) first second

    Option.iter (fun result -> setResult result) mappedWait

let listSetResult waitRef setResult index =
  fun returnValue -> 
    waitRef := (List.updateAt index (Some (returnValue)) !waitRef)

    if List.forall (fun item -> (Option.isSome item)) !waitRef then
      setResult List.map (fun item -> Option.get item) !waitRef

let getOperationResult op setResult props =
  match op with
  | Operation (opContents) -> opContents.Run setResult props
  | _ -> failwith "Can only work with Perform operations"

// FIXME: Not sure if hooks really make sense here
// let createCombinedHook hook1 hook2 =
//   fun props ->
//     let hookResult1 = hook1 props
//     let hookResult2 = hook2 props

//     match hookResult1, hookResult2 then
//     | None, None -> None

let Wait2 op1 op2 =
  let waitRef = ref (None, None)
  Operation (
    { 
      Run =
        fun setResult props ->
          let opResult1 = getOperationResult op1 (tuple2SetResult waitRef setResult 0) props
          let opResult2 = getOperationResult op2 (tuple2SetResult waitRef setResult 1) props

          match opResult1, opResult2 with
          | OperationWait ({ Element = element1; Effect = effect1; LayoutEffect = layoutEffect1; Hook = hook1 }),
            OperationWait ({ Element = element2; Effect = effect2; LayoutEffect = layoutEffect2; Hook = hook2 }) ->
              if Option.isSome hook1 then
                failwith "Hooks can't be used with Wait"
              if Option.isSome hook2 then
                failwith "Hooks can't be used with Wait"
              OperationWait (
                { Element = (getElement element1 element2)
                  Effect = (createCombinedEffect effect1 effect2)
                  LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
                  Hook = None }
              )

          | OperationWait ({ Element = element1; Effect = effect1; LayoutEffect = layoutEffect1; Hook = hook1 }),
            OperationContinue ({ ReturnValue = returnValue; Element = element2; Effect = effect2; LayoutEffect = layoutEffect2; Hook = hook2 }) ->
              if Option.isSome hook1 then
                failwith "Hooks can't be used with Wait"
              if Option.isSome hook2 then
                failwith "Hooks can't be used with Wait"
              let first, _ = !waitRef
              waitRef := (first, Some returnValue)
              OperationWait (
                { Element = (getElement element1 element2)
                  Effect = (createCombinedEffect effect1 effect2)
                  LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
                  Hook = None
                  }
              )

          | OperationContinue ({ ReturnValue = returnValue; Element = element1; Effect = effect1; LayoutEffect = layoutEffect1; Hook = hook1 }),
            OperationWait ({ Element = element2; Effect = effect2; LayoutEffect = layoutEffect2; Hook = hook2 }) ->
              if Option.isSome hook1 then
                failwith "Hooks can't be used with Wait"
              if Option.isSome hook2 then
                failwith "Hooks can't be used with Wait"
              let _, second = !waitRef
              waitRef := (Some returnValue, second)
              OperationWait (
                { Element = (getElement element1 element2)
                  Effect = (createCombinedEffect effect1 effect2)
                  LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
                  Hook = None
                  }
              )

          | OperationContinue ({ ReturnValue = returnValue1; Element = element1; Effect = effect1; LayoutEffect = layoutEffect1; Hook = hook1 }),
            OperationContinue ({ ReturnValue = returnValue2; Element = element2; Effect = effect2; LayoutEffect = layoutEffect2; Hook = hook2 }) ->
              if Option.isSome hook1 then
                failwith "Hooks can't be used with Wait"
              if Option.isSome hook2 then
                failwith "Hooks can't be used with Wait"
              OperationContinue (
                { ReturnValue = (returnValue1, returnValue2)
                  Element = (getElement element1 element2)
                  Effect = (createCombinedEffect effect1 effect2)
                  LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
                  Hook = None
                  }
              ) }
  )

let WaitAny2 op1 op2 =
  let mutable complete = false
  Operation (
    { 
      Run =
        fun setResult props ->
          let indexedSetResult setResult index =
            fun returnValue -> 
              if not complete then
                if index = 0 then
                  setResult (Some (returnValue), None)
                else
                  setResult (None, Some (returnValue))

          let opResult1 =
            match op1 with
            | Operation (pd1) -> pd1.Run (indexedSetResult setResult 0) props
            | _ -> failwith "Can only work with Operation types"

          let opResult2 =
            match op2 with
            | Operation (pd2) -> pd2.Run (indexedSetResult setResult 1) props
            | _ -> failwith "Can only work with Operation types"

          match opResult1, opResult2 with
          | OperationWait ({ Element = element1; Effect = effect1; LayoutEffect = layoutEffect1 }),
            OperationWait ({ Element = element2; Effect = effect2; LayoutEffect = layoutEffect2 }) ->
              OperationWait (
                { Element = (getElement element1 element2)
                  Effect = (createCombinedEffect effect1 effect2)
                  LayoutEffect = (createCombinedEffect layoutEffect1 layoutEffect2)
                  Hook = None
                  }
              )

          | OperationWait ({ Element = element1 }),
            OperationContinue ({ ReturnValue = returnValue; Element = element2 }) ->
              OperationContinue(
                { ReturnValue = (None, Some (returnValue))
                  Element = (getElement element1 element2)
                  Effect = None
                  LayoutEffect = None
                  Hook = None
                }
              )

          | OperationContinue ({ ReturnValue = returnValue; Element = element1 }),
            OperationWait ({ Element = element2}) ->
              OperationContinue(
                { 
                  ReturnValue = (Some(returnValue), None)
                  Element = (getElement element1 element2)
                  Effect = None
                  LayoutEffect = None
                  Hook = None
                }
              )

          | OperationContinue ({ ReturnValue = returnValue1; Element = element1 }),
            OperationContinue ({ ReturnValue = returnValue2; Element = element2 }) ->
              OperationContinue(
                { 
                  ReturnValue = (Some(returnValue1), Some(returnValue2))
                  Element = (getElement element1 element2)
                  Effect = None
                  LayoutEffect = None
                  Hook = None
                }
                  
              ) 
              }
  )

let WaitAny3 = End

// Return a series of suspended effect results as a stream
// let Next = End

// Return a series of suspended effect results as a stream (in any order)
let NextAny = End

// time operations
let Timeout time =
  Operation(
    { 
      Run =
        fun setResult _ ->
          let timeoutEffect () =
            let timeoutCallback () =
              setResult ()

            let timeoutID =
              Fable.Core.JS.setTimeout timeoutCallback time

            Some
              (fun () ->
                Fable.Core.JS.clearTimeout timeoutID
              )

          OperationWait(
            { 
              Element = None
              Effect = Some(timeoutEffect)
              LayoutEffect = None
              Hook = None
            }
          )
    }
  )

let Interval interval =
  Operation(
    { 
      Run =
        fun setResult _ ->
          let timeoutEffect () =
            let timeoutCallback () =
              setResult ()

            let timeoutID =
              Fable.Core.JS.setInterval timeoutCallback interval

            Some
              (fun () ->
                Fable.Core.JS.clearInterval timeoutID
              )

          OperationContinue(
            { ReturnValue = ()
              Element = None
              Effect = Some(timeoutEffect)
              LayoutEffect = None
              Hook = None
            }
          )
    }
  )

// fetch data.
let Fetch = End

// Core operations
let ContextCore<'returnType when 'returnType: equality>
  (useContext: IContext<'returnType> -> 'returnType)
  (context: IContext<'returnType>)
  =
  let mutable prevContext = None
  Operation(
    { 
      Run =
        fun _ __ ->
          let contextHook _ =
            let currentContext = useContext (context)
            match prevContext with
            | None -> Some(currentContext)
            | Some (prevContextValue) ->
              if not (shallowEqualObjects currentContext prevContextValue) then
                prevContext <- Some (currentContext)
                Some currentContext
              else
                None

          let currentContext = useContext (context)
          prevContext <- Some (currentContext)

          OperationContinue(
            { 
              ReturnValue = currentContext
              Element = None
              Effect = None
              LayoutEffect = None
              Hook = Some (contextHook)
              }
          )
    }
  )

let Context context = ContextCore Hooks.useContext context

let Ref (initialValue: 'returnType option) =
  Operation(
    { 
      Run =
        fun _ __ ->
          let currentRef = Hooks.useRef (initialValue)

          OperationContinue(
            { 
              ReturnValue = currentRef
              Element = None
              Effect = None
              LayoutEffect = None
              Hook = 
                Some (fun _ -> 
                  Hooks.useRef (initialValue)
                  None
                )
              }
          )
    }
  )

// Call a function passed in through props in an effect.
let Call callable =
  Operation(
    { 
      Run =
        fun _ __ ->
          let callCallable _ =
            callable ()
            None

          OperationContinue(
            { 
              ReturnValue = ()
              Element = None
              Effect = Some(callCallable)
              LayoutEffect = None
              Hook = None
              }
          ) }
  )

let CallLayout callable =
  Operation(
    { 
      Run =
        fun _ __ ->
          let callCallable _ =
            callable ()
            None

          OperationContinue(
            { 
              ReturnValue = ()
              Element = None
              Effect = None
              LayoutEffect = Some(callCallable)
              Hook = None
              }
          ) }
  )

// Error handling

// Handling events
