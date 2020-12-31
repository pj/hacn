module CompositionTest

open Hacn.Core
open Hacn.Operations
open Hacn.Render
open Fable.Jester
open Fable.ReactTestingLibrary
open Feliz
open Browser.Types
open Fable.Mocha
open Fable.Core.JS

let compositionTests =
  testList "Composition tests" [
    testCase "Compose simple render" <| fun () ->
      let clickBlocker () = hacn {
        do! Render Html.div [
          prop.testId "test"
          prop.captureClick ()
        ]
      }
      let App = 
        react {
          let! props = Props
          do! clickBlocker ()
          do! Render Html.div [
            prop.testId "clicked"
            prop.text "Element Clicked!"
          ]
        }
      let result = RTL.render(App ())
      let element = result.getByTestId "test"
      RTL.fireEvent.click(element)
      let element = result.getByTestId "clicked"
      Expect.equal element.textContent "Element Clicked!" "Clicked successfully"
      RTL.cleanup ()
  ]

Mocha.runTests compositionTests |> ignore