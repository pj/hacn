module ExceptionTest

open Hacn.Core
open Hacn.Operations
open Hacn.Render
open Fable.Jester
open Fable.ReactTestingLibrary
open Feliz
open Browser.Types
open Fable.Mocha
open Fable.Core.JS
open System

type TestException(message:string) =
   inherit Exception(message, null)

let exceptionTests =
  testList "Exception tests" [
    testCase "Exception in rendering" <| fun () ->
      let errorComponent = React.functionComponent (
          fun _ -> 
            raise (TestException "Something's wrong")
            []
          )
      let App = 
        react {
          try 
            let! x = Props
            do! Render errorComponent []
          with
          | e -> 
            do! Render Html.div [
              prop.testId "error"
              prop.text e.Exception.Message
            ]
        }
      let result = RTL.render(App ())
      let element = result.getByTestId "error"
      Expect.equal element.textContent "Something's wrong" "Error handled successfully"
      RTL.cleanup ()
  ]

Mocha.runTests exceptionTests |> ignore