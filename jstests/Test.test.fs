module Test
open Hacn.Core
open Hacn.Operations
open Fable.React.Standard
open Fable.React.Helpers
open Fable.Jester
open Fable.ReactTestingLibrary
open Fable.React.Props
open Feliz

type TestProps = { Message: string }

Jest.describe("Hacn Tests", fun () ->
  Jest.test("props", fun () ->
    let element = hacn {
      let! x = Props
      do! Render div [HTMLAttr.Custom("data-testid", "test")] [] [str (sprintf "%s World" x.Message)]
    }

    let propsRerenderer = 
      React.functionComponent( fun () -> 
        let state, setState = React.useState("")
        printf "here: %A" state
        Html.div [
          Html.input [
            prop.type' "text"
            prop.id "rerenderWrapper"
            prop.testId "rerenderWrapper"
            prop.onChange (fun event -> setState(event))
          ]
          yield! [element {Message = state} []]
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

    // let result = RTL.render(element {Hello = "Goodbye"} [])
    // let testElement = result.getByTestId("test", [])
    // Jest.expect(testElement).toHaveTextContent("Goodbye World")
  )
)