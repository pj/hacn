module UnitTest
open Hacn.Core
open Hacn.Types
open Fable.Mocha
open Fable.React

type TestRef<'refState> (initialValue: 'refState) =
  interface IRefValue<'refState> with
    member val current = initialValue with get, set

type TestProps = {
  Thing: string
}

let testInterpreter componentStateRef triggerRerender delayOperation (props: obj) = 
  let result = runNext (setNext componentStateRef triggerRerender) componentStateRef delayOperation props

  Option.defaultValue null result.Element


let props () = 
  let componentStateRef = TestRef (
      { 
        LastElement = None
        Next = None
        Started = false
        Hooks = []
        PrevProps = None
        Disposers = [||]
        LayoutDisposers = [||]
      }
  )

  let triggerRerender () = ()

  let element = testInterpreter componentStateRef triggerRerender (Delay (fun () -> Execution {Execute = fun _ -> {OperationsToBind = []}})) {Thing = "asdf"}
  Expect.equal 1 1 "Text content equal" 

let tests =
  testSequenced (
    testList "Hacn Tests" [
      testCase "props" <| props
    ]
  )

Mocha.runTests tests |> ignore