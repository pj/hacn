module Test
open Fable.Mocha
open Expecto
open Hacn.Core
open Hacn.Operations
open Fable.React
open Fable.ReactServer

type TestProps = { Hello: string}

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

type ContextResponse = { Everyone: string}

let useFakeContext (context) =
  { Everyone = "Everyone"}

let useFakeState _ =
  ()

type FakeHooks = 
  static member useRef initialValue =
    let mutable refValue = initialValue
    { new IRefValue<_> with
        member this.current with get() = refValue and set value = refValue <- value }
  static member useState() = ()
  static member useEffect() = ()
let FakeHooks: IHooks =
  { new IHooks with
    member __.useState(initialState: 'T) =
      { new IStateHook<'T> with
        member __.current = initialState
        member __.update(x: 'T) = ()
        member __.update(f: 'T->'T) = () }
    member __.useEffect(effect, dependencies) = ()
    member __.useRef(initialValue) =
      // let unboxedValue = 
      //   match refValue with 
      //   | Some(value) -> value
      //   | None -> 
      //     refValue <- Some(initialValue)
      //     initialValue
      let mutable refValue = None
      { new IRefValue<_> with
        member this.current with get() = initialValue and set value = refValue <- Some(value) }
    member __.useContext ctx = failwith "Unimplemented"
    member __.useDebugValue(label): unit = failwith "Unimplemented"
    member __.useDebugValue(value, format): unit = failwith "Unimplemented"
    member __.useReducer(reducer,initialState) = failwith "Unimplemented"
    member __.useReducer(reducer, initialArgument, init) = failwith "Unimplemented"
    member __.useEffectDisposable(effect, dependencies) = failwith "Unimplemented"
    member __.useMemo(callback, dependencies) = failwith "Unimplemented"
    member __.useStateLazy(initialState) = failwith "Unimplemented"
  }

let allTests =
  testList "Arithmetic tests" [
    testCase "plus works" <| fun () ->
      let hacnTest = HacnBuilder((render FakeHooks))

      let element = hacnTest {
        let! x = Props()
        do! Render(div [] [str x.Hello; str " World"])
      }

      let createdElement = element {Hello = "Hello"} []

      let htmlNode = castHTMLNode createdElement
      match htmlNode with
      | Node (tag, _, _) ->
        Expect.equal tag "div" "Success"
      | _ -> failwith "Not html node"
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args allTests