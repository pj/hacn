module Test
open Fable.Mocha

let arithmeticTests =
  testList "Arithmetic tests" [
    testCase "plus works" <| fun () ->
      Expect.equal (1 + 1) 2 "plus"

    testCase "Test for falsehood" <| fun () ->
      Expect.isFalse (1 = 2) "false"

    testCaseAsync "Test async code" <|
      async {
          let! x = async { return 21 }
          let answer = x * 2
          Expect.equal 42 answer "async"
      }
  ]

Mocha.runTests arithmeticTests |> ignore