module UnitTest
open Hacn.Core
open Hacn.Types
open Hacn.Operations
open Fable.Mocha
open Fable.React
open Fable.Core.JS

type TestRef<'refState> (initialValue: 'refState) =
  interface IRefValue<'refState> with
    member val current = initialValue with get, set


let createTestRef () =
  TestRef (
      {
        LastElement = null
        Next = None
        Started = false
        Hooks = []
        PrevProps = None
        Disposers = [||]
        LayoutDisposers = [||]
      }
  )


type TestProps = {
  Value: string
}

let testInterpreter componentStateRef triggerRerender firstOperation (props: obj) =
  let result = runNext (setNext componentStateRef triggerRerender) componentStateRef firstOperation props

  result.Element

type TestBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2
  member _.TryWith (operation, f) = tryWith operation f

  member _.Run(firstOperation) =
    firstOperation

let testReact = new TestBuilder ()

let TestRender output =
  Operation(
    fun _ ->
      OperationWait(fun _ -> {
        Element = Some(ofString output)
        Effect = None
        LayoutEffect = None
        Hook = None
      })
  )

let TestContinueRender output =
  Operation(
    fun _ ->
      OperationContinue((
          fun _ -> {
          Element = Some(ofString output)
          Effect = None
          LayoutEffect = None
          Hook = None
        },
        ())
      )
  )

exception TestException of string

let TestExceptionOperation msg =
  Operation(
    fun _ ->
      raise (TestException msg)
  )

let TestEffectExceptionOperation msg =
  Operation(
    fun _ ->
      OperationWait(fun _ -> {
        Element = None
        Effect =
          Some (fun () ->
            raise (TestException msg)
          )
        LayoutEffect = None
        Hook = None
      })
  )

let performTest builder testValues =
  let componentStateRef = createTestRef ()

  let triggerRerender () = ()

  List.iter (
    fun (props, expected) ->
      let element =
        testInterpreter
          componentStateRef
          triggerRerender
          builder
          props

      Expect.equal element (ofString expected) "Text content equal"
    )
    testValues

let testLastElementRendered () =
  performTest
    (
      testReact {
        do! TestContinueRender "Not me!"
        do! TestRender "Hello World!"
      }
    )
    [({Value = "asdf"}, "Hello World!")]

let testRerenderSameElement () =
  performTest
    (
      testReact {
        do! TestRender "Hello World!"
      }
    )
    [({Value = "asdf"}, "Hello World!"); ({Value = "asdf"}, "Hello World!")]

let testProps () =
  performTest
    (
    testReact {
        let! props = Props
        do! TestRender props.Value
      }
    )
    [({Value = "Hello World!"}, "Hello World!"); ({Value = "Goodbye World!"}, "Goodbye World!")]

let testIfElse () =
  performTest
    (
    testReact {
        let! props = Props
        if props.Value = "Hello" then
          do! TestRender "Hello World!"
        else
          do! TestRender "Please Say Hello"
      }
    )
    [({Value = "Hello"}, "Hello World!"); ({Value = "Goodbye"}, "Please Say Hello")]

let testIfCombineContinue () =
  performTest
    (
      testReact {
        let! props = Props
        if props.Value = "Hello" then
          do! TestContinueRender "Hello World!"
        do! TestRender "Don't Say That"
      }
    )
    [({Value = "Hello"}, "Don't Say That"); ({Value = "Goodbye"}, "Don't Say That")]

let testIfCombineWait () =
  performTest
    (
      testReact {
        let! props = Props
        if props.Value = "Hello" then
          do! TestRender "Hello World!"
        do! TestRender "Please Say Hello"
      }
    )
    [({Value = "Hello"}, "Hello World!"); ({Value = "Goodbye"}, "Please Say Hello")]

let testTryWith () =
  performTest
    (
      testReact {
        let! props = Props
        try
          if props.Value = "Goodbye" then
            do! TestExceptionOperation "error!"
          else
            do! TestRender("Hello World!")
        with
          | TestException (msg) ->
            do! TestRender (sprintf "error message: %A" msg)
      }
    )
    [({Value = "Hello"}, "Hello World!"); ({Value = "Goodbye"}, "error message: error!")]

let tests =
  testSequenced (
    testList "Hacn Tests" [
      testCase "testLastElementRendered" <| testLastElementRendered
      testCase "testRerenderSameElement" <| testRerenderSameElement
      testCase "testProps" <| testProps
      testCase "testIfElse" <| testIfElse
      testCase "testIfCombineContinue" <| testIfCombineContinue
      testCase "testIfCombineWait" <| testIfCombineWait
      testCase "testTryWith" <| testTryWith
    ]
  )

Mocha.runTests tests |> ignore