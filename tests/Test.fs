module Test
open Fable.Mocha
open Expecto
open Hacn.Core
open Hacn.Operations
open Fable.React
open Fable.ReactServer
open Fable.React.Props
open Hacn.Types
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Dyn

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

type ContextResponse = { Everyone: string}

let useFakeContext (context) =
  { Everyone = "Everyone"}

let useFakeState initialState =
  let mutable value = initialState
  { new IStateHook<'T> with
    member __.current = value
    member __.update(x: 'T) = ()
    member __.update(f: 'T->'T) = () }

let useFakeEffect effectFunction =
  ()

type TestHTMLNode = 
  | Text of string
  | RawText of string
  | Node of string * obj * TestHTMLNode list
  | List of TestHTMLNode list
  | Empty

let rec convertToTestNode (htmlNode: HTMLNode) =
    match htmlNode with
    | HTMLNode.Text str -> Text str
    | HTMLNode.RawText str -> RawText str
    | HTMLNode.Node (tag, attrs, children) ->
      let testNodes = Seq.map (castHTMLNode >> convertToTestNode) children
      TestHTMLNode.Node(tag, attrs, (List.ofSeq testNodes))
    | HTMLNode.List nodes ->
      let testNodes = Seq.map (castHTMLNode >> convertToTestNode) nodes
      TestHTMLNode.List (List.ofSeq testNodes)
    | HTMLNode.Empty -> TestHTMLNode.Empty
  
let convertElementToTestNode element = 
  convertToTestNode (castHTMLNode element)

let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))

type TestProps = { Hello: string}
let propsTest () = 
  let element = hacnTest {
    let! x = Props
    let! y = Render div [] [CaptureClick(fun _ -> "asdfer")] [str x.Hello; str " World"]
    do! Render div [] [] [str "Success!"; str y]
  }

  let helloElement = element {Hello = "Hello"} []

  match convertElementToTestNode helloElement with 
  | Node("div", _, [Text("Hello"); Text(" World")]) -> ()
  | _ -> failwith "node does not match"

  let goodbyeElement = element {Hello = "Goodbye"} []

  match convertElementToTestNode goodbyeElement with 
  | Node("div", _, [Text("Goodbye"); Text(" World")]) -> ()
  | _ -> failwith "node does not match"

let TestOperation<'returnType> (result: 'returnType) = 
  Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let effectFunc rerender =
        rerender (fun _ -> Some(result :> obj))
        None
      match operationState with
      | None -> 
        InvokeEffect(effectFunc)
      | Some(result) -> 
        let castResult: 'returnType = explicitConvert result
        InvokeReturn(castResult)
  })

let anyTest () = 
  let element = hacnTest {
    let! y, x = (WaitAny2 (Render div [] [] [str "asdf"]) (TestOperation "asdf"))
    do! Render div [] [] [str y]
  }

  let helloElement = element {Hello = "Hello"} []

  match convertElementToTestNode helloElement with 
  | Node("div", _, [Text("Hello"); Text(" World")]) -> ()
  | _ -> failwith "node does not match"

let stateTest () = 
  Tests.skiptest "Unimplemented"

let contextTest () = 
  Tests.skiptest "Unimplemented"

let eventCaptureTest () =
  Tests.skiptest "Unimplemented"

let callExternalTest () = 
  Tests.skiptest "Unimplemented"

let refTest () = 
  Tests.skiptest "Unimplemented"

let backgroundTest () = 
  Tests.skiptest "Unimplemented"

let waitTest () = 
  Tests.skiptest "Unimplemented"

let waitMultipleTest () = 
  Tests.skiptest "Unimplemented"

let ifThenTest () = 
  Tests.skiptest "Unimplemented"

let matchTest () = 
  Tests.skiptest "Unimplemented"

let errorHandlingTest () = 
  Tests.skiptest "Unimplemented"

let compositionTest () = 
  Tests.skiptest "Unimplemented"

let allTests =
  testList "All tests" [
    testCase "Test changing props" propsTest;
    testCase "Test waiting any" anyTest;
    testCase "Test setting state" stateTest;
    testCase "Test context" contextTest;
    testCase "Test capturing variables" eventCaptureTest;
    testCase "Test calling external function" callExternalTest;
    testCase "Test ref" refTest;
    testCase "Test background variable" backgroundTest;
    testCase "Test waiting" waitTest;
    testCase "Test multiple waiting" waitMultipleTest;
    testCase "Test if/then" ifThenTest;
    testCase "Test match" matchTest;
    testCase "Test error handling" errorHandlingTest;

    testCase "Test composition" compositionTest;
    // Maybe?
    // testCase "Test workflow" propsTest;
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args allTests