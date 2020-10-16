module Test
open Hacn.Core
open Hacn.Operations
open Fable.React.Standard
open Fable.React.Helpers
open Fable.Jester
open Fable.ReactTestingLibrary
open Fable.React.Props
open Feliz
open Hacn.Types

type TestProps = { Message: string }

let testOperationWithTrigger<'result> () =
  let mutable internalRerender = None
  let operation = Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let effectFunc rerender =
        internalRerender <- Some(rerender)
        Some(fun _ -> 
          None)
      match operationState with
      | None -> 
        InvokeWait(None, Some(effectFunc))
      | Some(result) -> 
        let castResult: 'result = unbox result
        InvokeContinue(None, None, castResult)
  })

  let rerenderTrigger (value: 'result) =
    match internalRerender with
    | Some(rerender) -> rerender(fun _ -> Some(value :> obj))
    | None -> failwith "Should not happen"
  
  rerenderTrigger, operation

type TestState =
  {
    Current: int
  }

Jest.describe("Hacn Tests", fun () ->
  Jest.test("props", fun () ->
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
    Jest.expect(testElement).toHaveTextContent("Hello World")

    RTL.fireEvent.change(inputElement, [event.target [prop.value "Goodbye"]])
    let testElement = result.getByTestId "test"
    Jest.expect(testElement).toHaveTextContent("Goodbye World")
  )

  Jest.test("any", 
    promise {
      let rerenderTrigger, operation = testOperationWithTrigger<string> ()
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

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Hello World")

      RTL.act(fun () -> 
        rerenderTrigger "Goodbye World"
      )

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Goodbye World")
    }
  )

  Jest.test("wait single", 
    promise {
      let rerenderTrigger, operation = testOperationWithTrigger<string> ()
      let element = react {
        let! testResponse = operation
        do! Render Html.div [prop.testId "test"; prop.text testResponse]
      }

      let result = RTL.render(element ())

      do! Jest.expect(result.findByTestId "test").rejects.toThrow();

      RTL.act(fun () -> 
        rerenderTrigger "Hello World"
      )

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Hello World")
    }
  )

  Jest.test("wait multiple", 
    promise {
      let rerenderTrigger1, operation1 = testOperationWithTrigger ()
      let rerenderTrigger2, operation2 = testOperationWithTrigger ()
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

      do! Jest.expect(result.findByTestId "test").rejects.toThrow();

      RTL.act(fun () -> 
        rerenderTrigger1 "Hello"
      )

      do! Jest.expect(result.findByTestId "test").rejects.toThrow();

      RTL.act(fun () -> 
        rerenderTrigger2 "World"
      )

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Hello, World!")
    }
  )

  Jest.test("state", 
    promise {
      let rerenderTrigger, operation = testOperationWithTrigger ()
      let element = react {
        let! componentState = Get {Current = 0}
        do! 
          RenderContinue 
            Html.div 
            [
              prop.testId "test"
              prop.text (sprintf "%d!" componentState.Current)
            ]
        let! increment = operation
        if increment then
          do! Set({Current = componentState.Current + 1})
      }

      let result = RTL.render(element ())

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("0!")

      RTL.act(fun () -> 
        rerenderTrigger true
      )

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("1!")

      RTL.act(fun () -> 
        rerenderTrigger false
      )

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("1!")

      RTL.act(fun () -> 
        rerenderTrigger true
      )

      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("2!")
    }
  )

  Jest.test("capturing", 
    promise {
      let element = react {
        let! changedValue = 
          Render 
            Html.div 
            [
              prop.testId "test"
              prop.text "Say hello!"
              prop.children [
                Html.input
                  [
                    prop.type' "text"
                    prop.testId "input"
                    prop.captureChange
                  ]
              ]
            ]
        printf "%s" changedValue
        if changedValue = "Hello" then
          do! Render Html.div [
              prop.testId "test"
              prop.text "Hi there!"
            ]
      }

      let result = RTL.render(element ())
      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Say hello!")
      let! inputElement = result.findByTestId "input"

      RTL.fireEvent.change(inputElement, [event.target [prop.value "No"]])
      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Say hello!")

      let! inputElement = result.findByTestId "input"
      RTL.fireEvent.change(inputElement, [event.target [prop.value "Okay"]])
      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Say hello!")

      let! inputElement = result.findByTestId "input"
      RTL.fireEvent.change(inputElement, [event.target [prop.value "Hello"]])
      do! Jest.expect(result.findByTestId "test").resolves.toHaveTextContent("Hi there!")
    }
  )
)