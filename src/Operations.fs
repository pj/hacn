module Hacn.Operations

open FSharp.Core
open Fable.React
open Fable.Core.Util
open Fable.Core

[<ImportMember("./propsCompare.js")>]
let shallowEqualObjects (x: obj) (y: obj) : bool = jsNative

type PropsOperationState<'props> =
  { Props: 'props
    PrevProps: 'props option }

let Props<'returnType > =
  let mutable prevProps = None
  Operation(
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
      
      OperationContinue(
        (
          (fun _ -> 
            {
              Element = None
              Effect = None
              LayoutEffect = None
              Hook = Some(propsHook)
          }),
          (unbox<'returnType> props)
        )
      )
  )

// type StateContainer<'state> =
//   { Updated: bool
//     ComponentState: 'state }

// let State<'state> (initialState: 'state) =
//   let mutable prevState = None
//   Operation(
//         fun _ ->
//           let state = Hooks.useState(initialState)

//           let StateSetOperation setState (newState: 'state) : Builder<unit> =
//             Operation (
//               { 
//                 Run =
//                   fun _ ->
//                     let stateSetEffect _ =
//                       setState newState

//                     OperationWait (
//                       { Element = None
//                         Effect = Some (stateSetEffect, None)
//                         // LayoutEffect = None
//                         // Hook = None 
//                       }
//                       ) 
//                 }
//             )

//           let stateHook _ =
//             let stateHook = Hooks.useState(initialState)
//             match prevState with
//             | Some(prevStateContents) ->
//               if (not (shallowEqualObjects (stateHook.current) prevStateContents)) then
//                 prevState <- Some(stateHook.current)
//                 Some ((stateHook.current, StateSetOperation stateHook.update)) 
//               else
//                 None
//             | None -> 
//               prevState <- Some(stateHook.current)
//               Some ((stateHook.current, StateSetOperation stateHook.update))

//           OperationContinue(
//             (
//               (fun _ ->
//                 { 
//                   Element = None
//                   Effect = None
//                   LayoutEffect = None
//                   Hook = Some(stateHook) 
//                 }),
//               (state.current, StateSetOperation state.update)
//             )
//           )
//   )

let createCombinedEffect eff1Opt eff2Opt =
  match eff1Opt, eff2Opt with
  | Some (eff1), Some (eff2) -> 
    Some (
      fun () -> 
        let disposer1 = eff1 ()
        let disposer2 = eff2 ()

        Some(
          fun () ->
            Option.iter (fun x -> x ()) disposer1
            Option.iter (fun x -> x ()) disposer2
        )
    )
  | Some (_), None ->
    eff1Opt
  | None, Some (_) -> 
    eff2Opt
  | None, None -> 
    None

let getElement elementOpt1 elementOpt2 =
  match elementOpt1, elementOpt2 with
  | Some (element1), _ ->
    Some (element1)
  | None, Some (element2) -> 
    Some (element2)
  | None, None -> None

let getOperationResult op props =
  match op with
  | Operation (opContents) -> opContents props
  | _ -> failwith "Can only work with Perform operations"

let combinedSetResult wait1Ref wait2Ref (refToSet: Ref<'c option>) setResult (returnValue: 'c) = 
  refToSet.Value <- Some(returnValue)

  match (wait1Ref.contents, wait2Ref.contents) with
  | (Some (value1), Some(value2)) ->
    setResult (value1, value2)
  | _ -> ()

let combinedSideEffects (op1Func: OperationSideEffectsFunction<'a>) (op2Func: OperationSideEffectsFunction<'b>) wait1Ref wait2Ref setResult : OperationSideEffects<'a * 'b>=
  let op1SideEffects = op1Func (combinedSetResult wait1Ref wait2Ref wait1Ref setResult)
  let op2SideEffects = op2Func (combinedSetResult wait1Ref wait2Ref wait2Ref setResult)

  if Option.isSome op1SideEffects.Hook then
    failwith "Hooks can't be used with Wait"
  if Option.isSome op2SideEffects.Hook then
    failwith "Hooks can't be used with Wait"
  { 
    Element = (getElement op1SideEffects.Element op2SideEffects.Element)
    Effect = (createCombinedEffect op1SideEffects.Effect op2SideEffects.Effect)
    LayoutEffect = (createCombinedEffect op1SideEffects.LayoutEffect op2SideEffects.LayoutEffect)
    Hook = None 
  }

let Wait2<'a, 'b> (op1: Builder<'a>) (op2: Builder<'b>) : Builder<'a * 'b> =
  let wait1Ref = ref None
  let wait2Ref = ref None
  Operation (
    fun props ->
      let operationResult1 = getOperationResult op1 props
      let operationResult2 = getOperationResult op2 props

      match operationResult1, operationResult2 with
      | OperationWait (op1Func),
        OperationWait (op2Func) ->
          OperationWait ((combinedSideEffects op1Func op2Func wait1Ref wait2Ref))
      | OperationContinue (op1Func, returnValue),
        OperationWait (op2Func) ->
          wait1Ref.Value <- Some(returnValue)
          let sideEffects = combinedSideEffects op1Func op2Func wait1Ref wait2Ref
          match (wait1Ref.contents, wait2Ref.contents) with
          | (Some (value1), Some(value2)) ->
            OperationContinue (sideEffects, (value1, value2))
          | _ -> 
            OperationWait (sideEffects)
      | OperationWait (op1Func),
        OperationContinue (op2Func, returnValue) ->
          wait2Ref.Value <- Some(returnValue)
          let sideEffects = combinedSideEffects op1Func op2Func wait1Ref wait2Ref
          match (wait1Ref.contents, wait2Ref.contents) with
          | (Some (value1), Some(value2)) ->
            OperationContinue (sideEffects, (value1, value2))
          | _ -> 
            OperationWait (sideEffects)

      | OperationContinue (op1Func, returnValue1),
        OperationContinue (op2Func, returnValue2) ->
          wait1Ref.Value <- Some(returnValue1)
          wait2Ref.Value <- Some(returnValue2)
          let sideEffects = combinedSideEffects op1Func op2Func wait1Ref wait2Ref
          OperationContinue (sideEffects, (returnValue1, returnValue2))
  )

let combinedAnySetResult (wait1Ref: 'a option ref) (wait2Ref: 'b option ref) (refToSet: 'c option ref) (setResult: SetResult<'a option * 'b option>) (returnValue: 'c) = 
  refToSet.Value <- Some(returnValue)

  setResult (wait1Ref.Value, wait2Ref.Value)

let combinedAnySideEffects (op1Func: OperationSideEffectsFunction<'a>) (op2Func: OperationSideEffectsFunction<'b>) wait1Ref wait2Ref setResult : OperationSideEffects<'a option * 'b option>=
  let op1SideEffects = op1Func (combinedAnySetResult wait1Ref wait2Ref wait1Ref setResult)
  let op2SideEffects = op2Func (combinedAnySetResult wait1Ref wait2Ref wait2Ref setResult)

  if Option.isSome op1SideEffects.Hook then
    failwith "Hooks can't be used with Wait"
  if Option.isSome op2SideEffects.Hook then
    failwith "Hooks can't be used with Wait"
  { 
    Element = (getElement op1SideEffects.Element op2SideEffects.Element)
    Effect = (createCombinedEffect op1SideEffects.Effect op2SideEffects.Effect)
    LayoutEffect = (createCombinedEffect op1SideEffects.LayoutEffect op2SideEffects.LayoutEffect)
    Hook = None 
  }

let WaitAny2<'a, 'b> (op1: Builder<'a>) (op2: Builder<'b>) : Builder<'a option * 'b option> =
  let wait1Ref: 'a option ref = ref None
  let wait2Ref: 'b option ref = ref None
  Operation (
    fun props ->
      let operationResult1 = getOperationResult op1 props
      let operationResult2 = getOperationResult op2 props

      match operationResult1, operationResult2 with
      | OperationWait (op1Func),
        OperationWait (op2Func) ->
          OperationWait ((combinedAnySideEffects op1Func op2Func wait1Ref wait2Ref))
      | OperationContinue (op1Func, returnValue),
        OperationWait (op2Func) ->
          wait1Ref.Value <- Some(returnValue)
          let sideEffects = combinedAnySideEffects op1Func op2Func wait1Ref wait2Ref
          OperationContinue (sideEffects, (wait1Ref.Value, wait2Ref.Value))
      | OperationWait (op1Func),
        OperationContinue (op2Func, returnValue) ->
          wait2Ref.Value <- Some(returnValue)
          let sideEffects = combinedAnySideEffects op1Func op2Func wait1Ref wait2Ref
          OperationContinue (sideEffects, (wait1Ref.Value, wait2Ref.Value))
      | OperationContinue (op1Func, returnValue1),
        OperationContinue (op2Func, returnValue2) ->
          wait1Ref.Value <- Some(returnValue1)
          wait2Ref.Value <- Some(returnValue2)
          let sideEffects = combinedAnySideEffects op1Func op2Func wait1Ref wait2Ref
          OperationContinue (sideEffects, (Some(returnValue1), Some(returnValue2)))

  )

// // time operations
// let Timeout time =
//   Operation(
//     { 
//       Run =
//         fun _ ->
//           let bindSetResult setResult =
//             let timeoutEffect () = 
//               let timeoutCallback () =
//                 setResult ()

//               let timeoutID = Fable.Core.JS.setTimeout timeoutCallback time

//               Some(fun () -> Fable.Core.JS.clearTimeout timeoutID)
//             timeoutEffect

//           OperationWait(
//             fun setResult -> {
//               Element = None
//               Effect = Some(bindSetResult setResult)
//             }
//               // LayoutEffect = None
//               // Hook = None
//           )
//     }
//   )

// let Interval interval =
//   Operation(
//     { 
//       Run =
//         fun _ ->
//           let bindSetResult setResult =
//             let timeoutEffect () =
//               let timeoutCallback () =
//                 setResult ()

//               let timeoutID = Fable.Core.JS.setInterval timeoutCallback interval

//               Some(fun () -> Fable.Core.JS.clearInterval timeoutID)
//             timeoutEffect

//           OperationContinue(
//             (
//               fun setResult -> {
//                 Element = None
//                 Effect = Some(bindSetResult setResult)
//               },
//               ()
//             )
//             // { ReturnValue = ()
//             //   Element = None
//             //   Effect = Some(timeoutEffect, Some(timeoutDispose))
//             //   LayoutEffect = None
//             //   Hook = None
//             // }
//           )
//     }
//   )

// // fetch data.
// let Fetch = End

// // Core operations
// let ContextCore<'returnType when 'returnType: equality>
//   (useContext: IContext<'returnType> -> 'returnType)
//   (context: IContext<'returnType>)
//   =
//   let mutable prevContext = None
//   Operation(
//     { 
//       Run =
//         fun _ ->
//           let contextHook _ =
//             let currentContext = useContext (context)
//             match prevContext with
//             | None -> Some(currentContext)
//             | Some (prevContextValue) ->
//               if not (shallowEqualObjects currentContext prevContextValue) then
//                 prevContext <- Some (currentContext)
//                 Some currentContext
//               else
//                 None

//           let currentContext = useContext (context)
//           prevContext <- Some (currentContext)

//           OperationContinue(
//             { 
//               ReturnValue = currentContext
//               Element = None
//               Effect = None
//               LayoutEffect = None
//               Hook = Some (contextHook)
//               }
//           )
//     }
//   )

// let Context context = ContextCore Hooks.useContext context

// let Ref (initialValue: 'returnType option) =
//   Operation(
//     fun _ ->
//       let currentRef = Hooks.useRef (initialValue)

//       OperationContinue(
//         (
//           (fun _ -> { 
//             Element = None
//             Effect = None
//             LayoutEffect = None
//             Hook = 
//               Some (fun _ -> 
//                 Hooks.useRef (initialValue) |> ignore
//                 None
//               )
//             }),
//           currentRef
//         )
//       )
//   )

// // Call a function passed in through props in an effect.
// let Call callable =
//   Operation(
//     fun _ ->
//       let callCallable _ =
//         callable ()
//         None

//       OperationContinue(
//         (
//         (fun _ -> { 
//           Element = None
//           Effect = Some(callCallable)
//           LayoutEffect = None
//           Hook = None
//           })
//           , 
//           ()
//         )
//       ) 
//   )

// let CallLayout callable =
//   Operation(
//     { 
//       Run =
//         fun _ ->
//           let callCallable _ =
//             callable ()

//           OperationContinue(
//             { 
//               ReturnValue = ()
//               Element = None
//               Effect = None
//               LayoutEffect = Some(callCallable, None)
//               Hook = None
//               }
//           ) }
//   )

// // Error handling

// // Handling events
