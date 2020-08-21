module Test
open Fable.Mocha
open Expecto
open Hacn.Core
open Hacn.Operations
open Fable.React
open Fable.ReactServer

type TestProps = { Hello: string}

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

type ContextResponse = { Everyone: string}

let useFakeContext (context) =
  { Everyone = "Everyone"}

let useFakeState _ =
  ()

let allTests =
  testList "Arithmetic tests" [
    testCase "plus works" <| fun () ->
      let hacnTest = HacnBuilder((render useFakeRef Hooks.useState Hooks.useEffect))

      let element = hacnTest {
        let! x = Props()
        do! Render(div [] [str x.Hello; str " World"])
      }

      let createdElement = element {Hello = "Hello"} []

      let htmlNode = castHTMLNode createdElement
      match htmlNode with
      | Node (tag, _, _) ->
        Expect.equal tag "div" "Success"
      | _ -> failwith "Not html node"
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args allTests