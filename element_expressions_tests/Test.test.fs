module Test
open Fable.React.Standard
open Fable.React.Helpers
open Fable.Jester
open Fable.ReactTestingLibrary
open Fable.React.Props
open ElementExpression

Jest.describe("Hacn Tests", fun () ->
  Jest.test("props", fun () ->
    let element = div {
      className "test"
      testId "test"
      childText "Hello World"
    }

    let result = RTL.render(element)
    let testElement = result.getByTestId "test"
    Jest.expect(testElement).toHaveTextContent("Hello World")

    // let result = RTL.render(element {Hello = "Goodbye"} [])
    // let testElement = result.getByTestId("test", [])
    // Jest.expect(testElement).toHaveTextContent("Goodbye World")
  )
)