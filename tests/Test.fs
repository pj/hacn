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

let generateFakeRefHook () =
  let mutable refValue = None
  let useFakeRef initialValue =
    match refValue with
    | None -> 
      refValue <- Some(initialValue)
    | _ -> ()

    { new IRefValue<_> with
        member this.current 
          with get() = 
            match refValue with
            | Some(x) -> x
            | None -> failwith "no initial value provided"
          and set value = refValue <- Some(value) }
  useFakeRef

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
  effectFunction ()

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

type TestProps = { Hello: string}

let propsTest () = 
  let useFakeRef = generateFakeRefHook ()
  let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
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

type DelayedStatus =
  | Delayed
  | Returned

type DelayedOperationState = 
  {
    Status: DelayedStatus
  }

let TestDelayedOperation<'returnType> (result: 'returnType) = 
  Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let delayedFunc rerender =
        rerender (fun _ -> Some({Status = Delayed} :> obj))
        None
      let returnedFunc rerender =
        rerender (fun _ -> Some({Status = Returned} :> obj))
        None
      match operationState with
      | None -> 
        InvokeEffect(delayedFunc)
      | Some(status) -> 
        let castStatus: DelayedOperationState = explicitConvert status
        match castStatus with
        | {Status = Delayed} -> InvokeEffect(delayedFunc)
        | {Status = Returned} -> InvokeEffect(returnedFunc)
  })

let anyTest () = 
  let useFakeRef = generateFakeRefHook ()
  let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
  let element = hacnTest {
    let! _, testResponse = WaitAny2 (Render div [] [] [str "Hello"]) (TestOperation "Goodbye")
    match testResponse with
    | Some(testValue) -> 
      do! Render div [] [] [str testValue]
    | _ -> 
      do! Render div [] [] [str "No value..."]
  }

  let helloElement = element [] []
  let helloNode = convertElementToTestNode helloElement

  match helloNode with 
  | Node("div", _, [Text("Hello")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" helloNode)

  let goodbyeElement = element [] []
  let goodbyeNode = convertElementToTestNode goodbyeElement

  match goodbyeNode with 
  | Node("div", _, [Text("Goodbye")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" goodbyeNode)

// type StreamState<'returnType> = 
//   {
//     CurrentIdx: int;
//     Result: 'returnType;
//   }

// let TestStreamOperation<'returnType> (result: 'returnType array) = 
//   Perform({
//     OperationType = NotCore
//     PreProcess = fun _ -> None
//     GetResult = fun _ operationState ->
//       let effectFunc rerender =
//         let nextResult streamState = 
//           match streamState with
//           | None -> 
//             {CurrentIdx = 0; Result = result.[0]}
//         rerender nextResult
//         None
//       match operationState with
//       | None -> 
//         InvokeEffect(effectFunc)
//       | Some(result) -> 
//         let castResult: 'returnType = explicitConvert result
//         InvokeReturn(castResult)
//   })

let streamTest () = 
  Tests.skiptest "Unimplemented"
//   let useFakeRef = generateFakeRefHook ()
//   let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
//   let element = hacnTest {
//     let! _, testResponse = WaitAny2 (Render div [] [] [str "Hello"]) (TestOperation "Goodbye")
//     match testResponse with
//     | Some(testValue) -> 
//       do! Render div [] [] [str testValue]
//     | _ -> 
//       do! Render div [] [] [str "No value..."]
//   }

//   let helloElement = element [] []
//   let helloNode = convertElementToTestNode helloElement

//   match helloNode with 
//   | Node("div", _, [Text("Hello")]) -> ()
//   | _ -> failwith (sprintf "node does not match: %A" helloNode)

//   let goodbyeElement = element [] []
//   let goodbyeNode = convertElementToTestNode goodbyeElement

//   match goodbyeNode with 
//   | Node("div", _, [Text("Goodbye")]) -> ()
//   | _ -> failwith (sprintf "node does not match: %A" goodbyeNode)

let waitSingleTest () = 
  let useFakeRef = generateFakeRefHook ()
  let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
  let element = hacnTest {
    let! testResponse = TestOperation "Hello"
    do! Render div [] [] [str testResponse]
  }

  let emptyElement = element [] []
  let emptyNode = convertElementToTestNode emptyElement

  match emptyNode with 
  | Empty -> ()
  | _ -> failwith (sprintf "node does not match: %A" emptyNode)

  let helloElement = element [] []
  let helloNode = convertElementToTestNode helloElement

  match helloNode with 
  | Node("div", _, [Text("Hello")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" helloNode)

let waitBothTest () = 
  let useFakeRef = generateFakeRefHook ()
  let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
  let element = hacnTest {
    let! hello, world = Wait2 (TestOperation "Hello") (TestOperation "World")
    do! Render div [] [] [str (sprintf "%s, %s!" hello world)]
  }

  let emptyElement = element [] []
  let emptyNode = convertElementToTestNode emptyElement

  match emptyNode with 
  | Empty -> ()
  | _ -> failwith (sprintf "node does not match: %A" emptyNode)

  let helloElement = element [] []
  let helloNode = convertElementToTestNode helloElement

  match helloNode with 
  | Node("div", _, [Text("Hello, World!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" helloNode)

let waitSequentialTest () = 
  let useFakeRef = generateFakeRefHook ()
  let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
  let element = hacnTest {
    let! hello, world = Wait2 (TestOperation "Hello") (TestDelayedOperation "World")
    do! Render div [] [] [str (sprintf "%s, %s!" hello world)]
  }

  let startElement = element [] []
  let startNode = convertElementToTestNode startElement

  match startNode with 
  | Empty -> ()
  | _ -> failwith (sprintf "node does not match: %A" startNode)

  let delayedElement = element [] []
  let delayedNode = convertElementToTestNode delayedElement

  match delayedNode with 
  | Empty -> ()
  | _ -> failwith (sprintf "node does not match: %A" delayedNode)

  let helloElement = element [] []
  let helloNode = convertElementToTestNode helloElement

  match helloNode with 
  | Node("div", _, [Text("Hello, World!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" helloNode)

let waitMultipleTest () = 
  Tests.skiptest "Unimplemented"

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
    testCase "Test waiting single" waitSingleTest;
    testCase "Test waiting both at same time" waitBothTest;
    ftestCase "Test waiting both one after another" waitSequentialTest;
    testCase "Test stream effect" streamTest;
    testCase "Test setting state" stateTest;
    testCase "Test context" contextTest;
    testCase "Test capturing variables" eventCaptureTest;
    testCase "Test calling external function" callExternalTest;
    testCase "Test ref" refTest;
    testCase "Test background variable" backgroundTest;
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