module Test
open Hacn.Core
open Hacn.Operations
open Fable.React.Standard
open Fable.React.Helpers
open Fable.Jester
open Fable.ReactTestingLibrary
open Fable.React.Props
open Feliz

// type TestProps = { Hello: string}

// let propsTest () =
//   ()

let testElement = React.functionComponent(fun () ->
    Html.h1 [
        prop.testId "test"
        prop.text "Hello World"
    ]
  )

Jest.describe("Hacn Tests", fun () ->
  Jest.test("props", fun () ->
    // let element = hacn {
    //   let! x = Props
    //   do! Render div [HTMLAttr.Custom("data-test-id", "test")] [] [str (sprintf "%s World" x.Hello)]
    // }
    // let element = div [HTMLAttr.Custom("data-test-id", "test")] []

    let result = RTL.render(testElement())
    result.debug ()
    let testElement = result.getByTestId "test"
    Jest.expect(testElement).toHaveTextContent("Hello World")
  )
)