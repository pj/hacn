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

let testOperationWithTrigger () =
  let mutable internalRerender = None
  let operation = Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let effectFunc rerender =
        internalRerender <- Some(rerender)
        None
      match operationState with
      | None -> 
        InvokeWait(None, Some(effectFunc))
      | Some(result) -> 
        let castResult: string = unbox result
        InvokeContinue(None, None, castResult)
  })

  let rerenderTrigger value =
    match internalRerender with
    | Some(rerender) -> rerender(fun _ -> Some(value :> obj))
    | None -> failwith "Should not happen"
  
  rerenderTrigger, operation


Jest.describe("Hacn Tests", fun () ->
  Jest.test("props", fun () ->
    let element = hacn {
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
      let rerenderTrigger, operation = testOperationWithTrigger ()
      let element = hacn {
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
)