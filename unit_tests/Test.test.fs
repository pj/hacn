module UnitTest
open Fable.Mocha

let props () = 
  Expect.equal 1 1 "Text content equal" 

let tests =
  testSequenced (
    testList "Hacn Tests" [
      testCase "props" <| props
    ]
  )

Mocha.runTests tests |> ignore