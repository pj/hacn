module Test
open Expecto
open Hacn.Operations
open Fable.React
open Utils

type TestProps = { Hello: string}

let propsTest () = 
  let element = hacnTest () {
    let! x = Props
    let! y = Render div [] [CaptureClick(fun _ -> "asdfer")] [str x.Hello; str " World"]
    do! Render div [] [] [str "Success!"; str y]
  }

  let testData = [
      ({Hello = "Hello"}, Node("div", [], [Text("Hello"); Text(" World")])); 
      ({Hello = "Goodbye"}, Node("div", [], [Text("Goodbye"); Text(" World")])); 
    ]

  runTestSequence element testData

let anyTest () = 
  let element = hacnTest () {
    let! _, testResponse = WaitAny2 (Render div [] [] [str "Hello"]) (TestOperation "Goodbye")
    match testResponse with
    | Some(testValue) -> 
      do! Render div [] [] [str testValue]
    | _ -> 
      do! Render div [] [] [str "No value..."]
  }

  let testData = [
      ({Hello = "Hello"}, Node("div", [], [Text("Hello")])); 
      ({Hello = "Goodbye"}, Node("div", [], [Text("Goodbye")])); 
    ]

  runTestSequence element testData 

let streamTest () = 
  Tests.skiptest "Unimplemented"

let waitSingleTest () = 
  let element = hacnTest () {
    let! testResponse = TestOperation "Hello"
    do! Render div [] [] [str testResponse]
  }
  let testData = [
      ([], Empty); 
      ([], Node("div", [], [Text("Hello")])); 
    ]

  runTestSequence element testData 

let waitBothTest () = 
  let element = hacnTest () {
    let! hello, world = Wait2 (TestOperation "Hello") (TestOperation "World")
    do! Render div [] [] [str (sprintf "%s, %s!" hello world)]
  }

  let testData = [
      ([], Empty); 
      ([], Node("div", [], [Text("Hello, World!")])); 
    ]

  runTestSequence element testData 

let waitSequentialTest () = 
  let element = hacnTest () {
    let! hello, world = Wait2 (TestOperation "Hello") (TestDelayedOperation "World")
    do! Render div [] [] [str (sprintf "%s, %s!" hello world)]
  }

  let testData = [
      ([], Empty); 
      ([], Empty); 
      ([], Node("div", [], [Text("Hello, World!")])); 
    ]

  runTestSequence element testData 

type TestState =
  {
    Current: int
  }

let stateTest () = 
  let testTrigger = TestStreamTrigger ()
  let element = hacnTest () {
    let! componentState = Get({Current = 0})
    do! RenderContinue div [] [str (sprintf "%d!" componentState.Current)]
    let! increment = (TestStreamOperation testTrigger)
    if increment then
      do! Set({Current = componentState.Current + 1})
  }

  let oneElement = element [] []
  let oneNode = convertElementToTestNode oneElement

  match oneNode with 
  | Node("div", _, [Text("0!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" oneNode)

  testTrigger.Call true
  let twoElement = element [] []
  let twoNode = convertElementToTestNode twoElement

  match twoNode with 
  | Node("div", _, [Text("0!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" twoNode)

  testTrigger.Call false
  let sameElement = element [] []
  let sameNode = convertElementToTestNode sameElement

  match sameNode with 
  | Node("div", _, [Text("1!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" sameNode)

  testTrigger.Call true
  let threeElement = element [] []
  let threeNode = convertElementToTestNode threeElement

  match threeNode with 
  | Node("div", _, [Text("1!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" threeNode)

  let threeElement = element [] []
  let threeNode = convertElementToTestNode threeElement

  match threeNode with 
  | Node("div", _, [Text("2!")]) -> ()
  | _ -> failwith (sprintf "node does not match: %A" threeNode)

let eventCaptureTest () =
  let element = hacnTest () {
    let! y = Render div [] [CaptureClick(fun _ -> "World")] [str "Hello"]
    do! Render div [] [] [str y]
  }

  let testData = [
      ([], Node("div", [], [Text("Hello")])); 
      ([], Node("div", [], [Text("World")])); 
    ]

  runTestSequence element testData

let callExternalTest () = 
  Tests.skiptest "Unimplemented"

let backgroundTest () = 
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
    testCase "Test waiting both one after another" waitSequentialTest;
    // ptestCase "Test stream effect" streamTest;
    testCase "Test setting state" stateTest;
    // ptestCase "Test capturing variables" eventCaptureTest;
    // ptestCase "Test calling external function" callExternalTest;
    // ptestCase "Test background variable" backgroundTest;
    // ptestCase "Test composition" compositionTest;
    // ptestCase "Test error handling" errorHandlingTest;
    // ptestCase "Test multiple elements" multipleElementTest;
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args allTests