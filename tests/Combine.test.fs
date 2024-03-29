module CombineTest

open Hacn.Core
open Hacn.Operations
open Hacn.Render
open Fable.Jester
open Fable.ReactTestingLibrary
open Helpers
open Feliz
open Browser.Types
open Fable.Mocha
open Fable.Core.JS

type CombineProps = { ShowBlocker: bool }

let combineTests =
  testList
    "Combine tests"
    [ testCase "Combine basic"
      <| fun () ->
           let rerenderTrigger, _, operation = testOperationWithTrigger ()

           let App =
             react {
               let! props = Props

               if props.ShowBlocker then
                 do!
                   Render
                     Html.div
                     [ prop.testId "test"
                       prop.captureClick () ]

                 let! x = operation

                 do!
                   Render
                     Html.div
                     [ prop.testId "test"
                       prop.captureClick () ]

               do!
                 Render
                   Html.div
                   [ prop.testId "clicked"
                     prop.text "Element Clicked!" ]
             }

           let result = RTL.render (App { ShowBlocker = true })
           result.debug ()
           let element = result.getByTestId "test"
           RTL.fireEvent.click (element)
           result.debug ()
           rerenderTrigger "asdf"
           let element = result.getByTestId "test"
           RTL.fireEvent.click (element)
           let element = result.getByTestId "clicked"
           Expect.equal element.textContent "Element Clicked!" "Clicked successfully"
           RTL.cleanup ()
      testCase "Combine rerendering"
      <| fun () ->
           let rerenderTrigger, _, operation = testOperationWithTrigger ()

           let App =
             react {
               let! props = Props

               if props.ShowBlocker then
                 do! Render Html.div [ prop.testId "test" ]

               let! x = operation

               do!
                 Render
                   Html.div
                   [ prop.testId "clicked"
                     prop.text (x: string) ]
             }

           let result = RTL.render (App { ShowBlocker = false })
           rerenderTrigger "Hello World!"
           let element = result.getByTestId "clicked"
           Expect.equal element.textContent "Hello World!" "Clicked successfully"
           rerenderTrigger "Goodbye World!"
           Expect.equal element.textContent "Goodbye World!" "Clicked successfully"
           RTL.cleanup () ]

Mocha.runTests combineTests |> ignore
