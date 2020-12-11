module MemoTest

open Hacn.Core
open Hacn.Operations
open Fable.Jester
open Fable.ReactTestingLibrary
open Helpers
open Feliz
open Hacn.Types
open Browser.Types
open Fable.Core.JS
open Fable.Mocha

type MemoTestProps = {
  TestValue: string
}

// type WaitContinueState = {
//   Complete: bool
// }

let memoTests =
  testList "Memo tests" [
    testCase "simple side effect only" <| fun () ->
      let mutable calledWith = ""
      let App = 
        react {
          let! props = Props
          do! Memo.Once(Call (fun () -> calledWith <- props.TestValue))
        }
      let result = RTL.render(App {TestValue = "first"})
      result.rerender (App {TestValue = "second"})
      Expect.equal calledWith "first" "Called strings equal"
      RTL.cleanup ()

    testCase "No underlying effect" <| fun () ->
      let mutable insideOperationCount = 0
      let TestOperation = 
        Perform({
          PreProcess = fun _ -> None
          GetResult = fun _ __ ->
            insideOperationCount <- insideOperationCount + 1
            PerformContinue(
              {
                Element = None
                Effect = None
                LayoutEffect = None
                OperationState = None
              }, 
              "Hello World!"
            )
        })
      let App = 
        react {
          let! props = Props
          let! message = Memo.Once(TestOperation)
          do! Render Html.div [
            prop.testId "test"
            prop.text message
          ]
        }
      let result = RTL.render(App {TestValue = "first"})
      let testElement = result.getByTestId "test"
      Expect.equal testElement.textContent "Hello World!" "Text content equal" 
      result.rerender (App {TestValue = "second"})
      Expect.equal insideOperationCount 1 "Called operation count is the same"
      Expect.equal testElement.textContent "Hello World!" "Text content equal" 
      RTL.cleanup ()

    testCase "Wait then Continue" <| fun () ->
      let rerenderTrigger, callCount, operation = testOperationWithTrigger ()
      let App = 
        react {
          let! props = Props
          let! message = Memo.Once(operation)
          do! Render Html.div [
            prop.testId "test"
            prop.text(sprintf "%A" message)
          ]
        }
      let result = RTL.render(App {TestValue = "first"})
      let testElement = result.queryByTestId "test"
      Expect.equal testElement None "Element is None" 
      result.rerender (App {TestValue = "second"})
      result.rerender (App {TestValue = "third"})
      result.rerender (App {TestValue = "fourth"})
      rerenderTrigger "Hello World!"
      Expect.equal !callCount 2 "Called operation count is the same"
      let testElement = result.getByTestId "test"
      Expect.equal testElement.textContent "Hello World!" "Element is None" 
      RTL.cleanup ()

    testCase "Change function" <| fun () ->
      let rerenderTrigger, callCount, operation = testOperationWithTrigger ()
      let App = 
        react {
          let! props = Props
          let! message = Memo.Memo operation (Some(fun _ -> props.TestValue = "Bust"))
          do! Render Html.div [
            prop.testId "test"
            prop.text(sprintf "%A" message)
          ]
        }
      let result = RTL.render(App {TestValue = "Something"})
      let testElement = result.queryByTestId "test"
      Expect.equal testElement None "Element is None" 
      result.rerender (App {TestValue = "Something"})
      result.rerender (App {TestValue = "Something"})
      result.rerender (App {TestValue = "Something"})
      rerenderTrigger "Hello World!"
      Expect.equal !callCount 2 "Called operation count is the same"
      let testElement = result.getByTestId "test"
      Expect.equal testElement.textContent "Hello World!" "Element is None" 
      
      result.rerender (App {TestValue = "Bust"})
      result.rerender (App {TestValue = "Something"})
      result.rerender (App {TestValue = "Something"})
      rerenderTrigger "Goodbye!"

      Expect.equal !callCount 4 "Called operation count is the same"
      let testElement = result.getByTestId "test"
      Expect.equal testElement.textContent "Goodbye!" "Element is None" 

      RTL.cleanup ()
  ]

Mocha.runTests memoTests |> ignore