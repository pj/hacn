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

type TestBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2

  member _.Run(firstOperation) =
    firstOperation

let testReact = new TestBuilder ()

let TestRender = 
  Operation(
    fun _ -> 
      OperationWait(fun _ -> {
        Element = Some(div [] [ofString "hello"])
        Effect = None
        LayoutEffect = None
        Hook = None
      })
  )


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

  let testBuild = testReact {
    do! TestRender
  }

  let element = 
    testInterpreter 
      componentStateRef 
      triggerRerender 
      testBuild
      {Thing = "asdf"}

  Expect.equal 1 1 "Text content equal" 

let tests =
  testSequenced (
    testList "Hacn Tests" [
      testCase "props" <| props
    ]
  )

Mocha.runTests tests |> ignore