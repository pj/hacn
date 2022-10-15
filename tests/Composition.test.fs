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
open Hacn.ElementExpressions

type TestProps = { message: string }

let compositionTests =
  testList
    "Composition tests"
    [ testCase "Compose simple render"
      <| fun () ->
           let clickBlocker () =
             hacn {
               do! Render (div { 
                    testId "test"
                    captureClick () 
                  })
             }

           let App =
             react {
               let! props = Props
               do! clickBlocker ()

               do! Render (div {
                  testId "clicked"
                  text "Element Clicked!" 
                })
             }

           let result = RTL.render (App())
           let element = result.getByTestId "test"
           RTL.fireEvent.click (element)
           let element = result.getByTestId "clicked"
           Expect.equal element.textContent "Element Clicked!" "Clicked successfully"
           RTL.cleanup ()

      testCase "Compose simple return"
      <| fun () ->
           let returner () = hacn { return "Hello" }

           let App =
             react {
               let! props = Props
               let! x = returner ()

               do! Render (div { 
                  testId "test"
                  text (sprintf "%s World!" x) 
                })
             }

           let result = RTL.render (App())
           let element = result.getByTestId "test"
           Expect.equal element.textContent "Hello World!" "Clicked successfully"
           RTL.cleanup ()

      testCase "Compose with render"
      <| fun () ->
           let returner () =
             hacn {
               do! Render (div { 
                    testId "test"
                    captureClick () 
                  })

               return "Hello"
             }

           let App =
             react {
               let! x = returner ()

               do! Render (div { 
                  testId "clicked"
                  text (sprintf "%s World!" x) 
                })
             }

           let result = RTL.render (App())
           let element = result.getByTestId "test"
           RTL.fireEvent.click (element)
           let element = result.getByTestId "clicked"
           Expect.equal element.textContent "Hello World!" "Clicked successfully"
           RTL.cleanup ()

      testCase "Compose with props"
      <| fun () ->
           let propser () =
             hacn {
               let! x = Props
               return x.message
             }

           let App =
             react {
               let! x = propser ()

               do! Render (div { 
                  testId "test"
                  text (sprintf "%s World!" x) 
                })
             }

           let result = RTL.render (App { message = "Hello" })
           let element = result.getByTestId "test"
           Expect.equal element.textContent "Hello World!" "Message correct"
           let element = result.getByTestId "test"
           result.rerender (App { message = "Goodbye" })
           Expect.equal element.textContent "Goodbye World!" "Message correct"
           RTL.cleanup ()

      ]

Mocha.runTests compositionTests |> ignore
