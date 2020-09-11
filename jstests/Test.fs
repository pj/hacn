module Test
open Fable.Mocha
// open Hacn
// open Hacn.Core
open Fable.ReactTestingLibrary
// open Hacn.Operations
open Fable.React.Standard
open Fable.React.Helpers
open Fable.Core.JsInterop
open Hacn.Utils

type TestProps = { Hello: string}

let propsTest () =
  let asdf = "asdfasdf" :> obj

  let x: string = castObj asdf
  printf "%s" x
  // let element = hacn {
  //   let! x = Props
  //   do! Render div [] [] [str x.Hello; str " World"]
  // }

  // RTL.render(element [] []) |> ignore
  ()

let hacnTests =
  testList " tests" [
    testCase "Props test" <| propsTest
  ]

Mocha.runTests hacnTests |> ignore