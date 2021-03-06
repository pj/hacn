module Test
open Hacn.Core
open Hacn.Operations
open Fable.ReactTestingLibrary
open Helpers
open Feliz
open Hacn.Types
open Fable.Mocha
open Browser.Types
open Hacn.Operations
open Hacn.Render
open Browser.Dom

type TestProps = { Message: string }

type TestState =
  {
    Current: int
  }

let props () = 
  let element = react {
    let! x = Props
    do! Render Html.div [prop.testId "test"; prop.text (sprintf "%s World" x.Message)]
  }

  let propsRerenderer = 
    React.functionComponent( fun () -> 
      let state, setState = React.useState("")
      Html.div [
        Html.input [
          prop.type' "text"
          prop.id "rerenderWrapper"
          prop.testId "rerenderWrapper"
          prop.onChange (fun event -> setState(event))
        ]
        yield! [element {Message = state}]
      ]
    )

  let result = RTL.render(propsRerenderer ())
  let inputElement = result.getByTestId "rerenderWrapper"
  RTL.fireEvent.change(inputElement, [event.target [prop.value "Hello"]])
  let testElement = result.getByTestId "test"
  Expect.equal testElement.textContent "Hello World" "Text content equal" 

  RTL.fireEvent.change(inputElement, [event.target [prop.value "Goodbye"]])
  let testElement = result.getByTestId "test"
  Expect.equal testElement.textContent "Goodbye World" "Text content equal" 

let any () = 
  let rerenderTrigger, _, operation = testOperationWithTrigger<string> ()
  let element = react {
    let! _, testResponse = 
      WaitAny2 
        (Render Html.div [prop.testId "test"; prop.text "Hello World"]) 
        operation
    match testResponse with
    | Some(testValue) -> 
      do! Render Html.div [prop.testId "test"; prop.text testValue]
    | _ -> 
      do! Render Html.div [prop.testId "test"; prop.text "No value..."]
  }

  let result = RTL.render(element ())

  let element = result.getByTestId "test"
  Expect.equal element.textContent "Hello World" "Text content equal" 

  RTL.act(fun () -> 
    rerenderTrigger "Goodbye World"
  )

  let element = result.getByTestId "test"
  Expect.equal element.textContent "Goodbye World" "Text content equal" 

let waitSingle () = 
  let rerenderTrigger, _, operation = testOperationWithTrigger<string> ()
  let element = react {
    let! testResponse = operation
    do! Render Html.div [prop.testId "test"; prop.text testResponse]
  }

  let result = RTL.render(element ())

  try
    result.findByTestId "test" |> ignore
  with
  | _ -> Expect.pass () "Throws"

  RTL.act(fun () -> 
    rerenderTrigger "Hello World"
  )

  let element = result.getByTestId "test"
  Expect.equal element.textContent "Hello World" "Text content equal" 

let waitMultiple () = 
  let rerenderTrigger1, _, operation1 = testOperationWithTrigger ()
  let rerenderTrigger2, _, operation2 = testOperationWithTrigger ()
  let element = react {
    let! hello, world = Wait2 operation1 operation2
    do! 
      Render 
        Html.div 
        [
          prop.testId "test"; 
          prop.text (sprintf "%s, %s!" hello world)
        ]
  }

  let result = RTL.render(element ())

  try
    result.getByTestId "test" |> ignore
  with
  | _ -> Expect.pass () "Throws"

  RTL.act(fun () -> 
    rerenderTrigger1 "Hello"
  )

  try
    result.getByTestId "test" |> ignore
  with
  | _ -> Expect.pass () "Throws"

  RTL.act(fun () -> 
    rerenderTrigger2 "World"
  )

  let element = result.getByTestId "test"
  Expect.equal element.textContent "Hello, World!" "Text content equal" 

let state () =
  let rerenderTrigger, _, operation = testOperationWithTrigger ()
  let element = react {
    let! componentState, setComponentState = State {Current = 0}
    do! 
      RenderContinue 
        Html.div 
        [
          prop.testId "test"
          prop.text (sprintf "%d!" componentState.Current)
        ]
    let! increment = operation
    if increment then
      do! setComponentState({Current = componentState.Current + 1})
  }

  let result = RTL.render(element ())

  let element = result.getByTestId "test"
  Expect.equal element.textContent "0!" "Text content equal" 

  RTL.act(fun () -> 
    rerenderTrigger true
  )

  let element = result.getByTestId "test"
  Expect.equal element.textContent "1!" "Text content equal" 

  RTL.act(fun () -> 
    rerenderTrigger false
  )

  let element = result.getByTestId "test"
  Expect.equal element.textContent "1!" "Text content equal" 

  RTL.act(fun () -> 
    rerenderTrigger true
  )

  let element = result.getByTestId "test"
  Expect.equal element.textContent "2!" "Text content equal" 

let around p =
  promise {
    do! p ()
    RTL.cleanup ()
  }

let tests =
  testSequenced (
    testList "Hacn Tests" [
      testCase "props" <| props
      testCase "any" <| any
      testCase "wait single" <| waitSingle
      testCase "wait multiple" <| waitMultiple
      testCase "state" <| state
    ]
  )

Mocha.runTests tests |> ignore