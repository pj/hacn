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
        fun props ->
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
        fun _ ->
          let state = Hooks.useState(initialState)

          let StateSetOperation setState (newState: 'state) : Builder<unit> =
            Operation (
              { 
                Run =
                  fun _ ->
                    let stateSetEffect _ =
                      setState newState

                    OperationWait (
                      { Element = None
                        Effect = Some (stateSetEffect, None)
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

let createCombinedDispose disposeOpt1 disposeOpt2 =
  let combinedDisposer () =
    Option.iter uFunc disposeOpt1
    Option.iter uFunc disposeOpt2

  match disposeOpt1, disposeOpt2 with
  | None, None -> None
  | _ -> Some(combinedDisposer)

let createCombinedEffect (waitRef: (option<'a> * option<'b>) ref) eff1Opt eff2Opt =
  match eff1Opt, eff2Opt with
  | Some (eff1, dispose1), Some (eff2, dispose2) -> 
    let combinedEffect setResult = 
      eff1 (
          fun value1 -> 
            match waitRef.contents with 
            | (_, Some(value2)) ->
              setResult (Some value1, Some value2)
            | _ -> ()
        )
      eff2 (
          fun value2 -> 
            match waitRef.contents with 
            | (Some(value1), _) ->
              setResult (Some value1, Some value2)
            | _ -> ()
        )
    Some (combinedEffect, createCombinedDispose dispose1 dispose2)
  | Some (eff1, dispose1), None ->
    let combinedEffect setResult = 
      eff1 (
          fun value1 -> 
            match waitRef.contents with 
            | (_, Some(value2)) ->
              setResult (Some value1, Some value2)
            | _ -> ()
        )
    Some (combinedEffect, dispose1)
  | None, Some (eff2, dispose2) -> 
    let combinedEffect setResult = 
      eff2 (
          fun value2 -> 
            match waitRef.contents with 
            | (Some(value1), _) ->
              setResult (Some value1, Some value2)
            | _ -> ()
        )
    Some (combinedEffect, dispose2)
  | None, None -> 
    None

let getElement elementOpt1 elementOpt2 =
  match elementOpt1, elementOpt2 with
  | Some (element1), Some (_) -> 
    Some (
      fun setResult -> element1 (fun returnValue -> setResult (Some(returnValue), None))
    )
  | Some (element1), None ->
    Some (
      fun setResult -> element1 (fun returnValue -> setResult (Some(returnValue), None))
    )
  | None, Some (element2) -> 
    Some (fun setResult -> element2 (fun returnValue -> setResult (None, Some(returnValue))))
  | None, None -> None

let getOperationResult op props =
  match op with
  | Operation (opContents) -> opContents.Run props
  | _ -> failwith "Can only work with Perform operations"

let Wait2 op1 op2 =
  let waitRef = ref (None, None)
  Operation (
    { 
      Run =
        fun props ->
          let opResult1 = getOperationResult op1  props
          let opResult2 = getOperationResult op2  props

          match opResult1, opResult2 with
          | OperationWait ({ Element = element1; Effect = effect1; LayoutEffect = layoutEffect1; Hook = hook1 }),
            OperationWait ({ Element = element2; Effect = effect2; LayoutEffect = layoutEffect2; Hook = hook2 }) ->
              if Option.isSome hook1 then
                failwith "Hooks can't be used with Wait"
              if Option.isSome hook2 then
                failwith "Hooks can't be used with Wait"
              OperationWait (
                { Element = (getElement element1 element2)
                  Effect = (createCombinedEffect waitRef effect1 effect2)
                  LayoutEffect = (createCombinedEffect waitRef layoutEffect1 layoutEffect2)
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
                  Effect = (createCombinedEffect waitRef effect1 effect2)
                  LayoutEffect = (createCombinedEffect waitRef layoutEffect1 layoutEffect2)
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
                  Effect = (createCombinedEffect waitRef effect1 effect2)
                  LayoutEffect = (createCombinedEffect waitRef layoutEffect1 layoutEffect2)
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
                { ReturnValue = (Some returnValue1, Some returnValue2)
                  Element = (getElement element1 element2)
                  Effect = (createCombinedEffect waitRef effect1 effect2)
                  LayoutEffect = (createCombinedEffect waitRef layoutEffect1 layoutEffect2)
                  Hook = None
                  }
              ) }
  )

let WaitAny2 (op1: Builder<'a>) (op2: Builder<'b>) : Builder<option<'a> * option<'b>> =
  Operation (
    { 
      Run =
        fun props ->
          let combinedEffect eff1Opt eff2Opt =
            match eff1Opt, eff2Opt with
            | Some (eff1, dispose1), Some (eff2, dispose2) -> 
              let combinedEffect setResult = 
                eff1 (fun value1 -> setResult (Some value1, None))
                eff2 (fun value2 -> setResult (None, Some value2))
              Some (combinedEffect, createCombinedDispose dispose1 dispose2)
            | Some (eff1, dispose1), None ->
              let combinedEffect setResult = 
                eff1 (fun value1 -> setResult (Some value1, None))
              Some (combinedEffect, dispose1)
            | None, Some (eff2, dispose2) -> 
              let combinedEffect setResult = 
                eff2 (fun value2 -> setResult (None, Some value2))
              Some (combinedEffect, dispose2)
            | None, None -> 
              None
          let opResult1 =
            match op1 with
            | Operation (pd1) -> pd1.Run props
            | _ -> failwith "Can only work with Operation types"

          let opResult2 =
            match op2 with
            | Operation (pd2) -> pd2.Run props
            | _ -> failwith "Can only work with Operation types"

          match opResult1, opResult2 with
          | OperationWait ({ Element = element1; Effect = effect1; LayoutEffect = layoutEffect1 }),
            OperationWait ({ Element = element2; Effect = effect2; LayoutEffect = layoutEffect2 }) ->
              OperationWait (
                { Element = (getElement element1 element2)
                  Effect = (combinedEffect effect1 effect2)
                  LayoutEffect = (combinedEffect layoutEffect1 layoutEffect2)
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
  let mutable timeoutID = None
  Operation(
    { 
      Run =
        fun _ ->
          let timeoutEffect setResult =
            let timeoutCallback () =
              setResult ()

            timeoutID <- Some (Fable.Core.JS.setTimeout timeoutCallback time)

          let timeoutDispose () =
            Option.iter Fable.Core.JS.clearTimeout timeoutID

          OperationWait(
            { 
              Element = None
              Effect = Some(timeoutEffect, Some(timeoutDispose))
              LayoutEffect = None
              Hook = None
            }
          )
    }
  )

let Interval interval =
  let mutable timeoutID = None
  Operation(
    { 
      Run =
        fun _ ->
          let timeoutEffect setResult =
            let timeoutCallback () =
              setResult ()

            timeoutID <- Some (Fable.Core.JS.setInterval timeoutCallback interval)

          let timeoutDispose () =
            Option.iter Fable.Core.JS.clearInterval timeoutID

          OperationContinue(
            { ReturnValue = ()
              Element = None
              Effect = Some(timeoutEffect, Some(timeoutDispose))
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
        fun _ ->
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
        fun _ ->
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
        fun _ ->
          let callCallable _ =
            callable ()

          OperationContinue(
            { 
              ReturnValue = ()
              Element = None
              Effect = Some(callCallable, None)
              LayoutEffect = None
              Hook = None
              }
          ) }
  )

let CallLayout callable =
  Operation(
    { 
      Run =
        fun _ ->
          let callCallable _ =
            callable ()

          OperationContinue(
            { 
              ReturnValue = ()
              Element = None
              Effect = None
              LayoutEffect = Some(callCallable, None)
              Hook = None
              }
          ) }
  )

// Error handling

// Handling events
