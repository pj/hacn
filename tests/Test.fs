module Test
open Fable.Mocha
open Expecto
open Hacn

let allTests =
  testList "Arithmetic tests" [
    testCase "plus works" <| fun () ->
      Expect.equal (1 + 1) 2 "plus"
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args allTests