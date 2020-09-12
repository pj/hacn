module Test
open Hacn.Core
open Hacn.Operations
open Fable.React.Standard
open Fable.React.Helpers
open Fable.Jester
open Fable.ReactTestingLibrary
open Fable.React.Props
open Feliz

type TestProps = { Hello: string }

Jest.describe("Hacn Tests", fun () ->
  Jest.test("props", fun () ->
    let element = hacn {
      let! x = Props
      do! Render div [HTMLAttr.Custom("data-testid", "test")] [] [str (sprintf "%s World" x.Hello)]
    }

    let result = RTL.render(element {Hello = "Hello"} [])
    let testElement = result.getByTestId("test", [])
    Jest.expect(testElement).toHaveTextContent("Hello World")

    let result = RTL.render(element {Hello = "Goodbye"} [])
    let testElement = result.getByTestId("test", [])
    Jest.expect(testElement).toHaveTextContent("Goodbye World")
  )
)