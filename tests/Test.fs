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

let useFakeState initialState =
  let mutable value = initialState
  { new IStateHook<'T> with
    member __.current = value
    member __.update(x: 'T) = ()
    member __.update(f: 'T->'T) = () }

let useFakeEffect effectFunction =
  ()

let hacnTest = HacnBuilder((render useFakeRef useFakeState useFakeEffect))
let allTests =
  testList "Arithmetic tests" [
    testCase "Props test" <| fun () ->

      let element = hacnTest {
        let! x = Props ()
        // let y = 
        //   if true then
        //     Props ()
        //   else 
        //     Props ()
        do! Render(div [] [str x.Hello; str " World"])
      }

      let helloElement = element {Hello = "Hello"} []

      let helloNode = castHTMLNode helloElement
      match helloNode with
      | Node (tag, _, children) ->
        Expect.equal tag "div" "Correct tag"
        Expect.hasLength children 2 "Correct length"
        match List.ofSeq children with
        | [helloElement; worldElement] -> 
          let helloNode = castHTMLNode helloElement
          let worldNode = castHTMLNode worldElement
          match helloNode with
          | Text helloStr -> Expect.equal helloStr "Hello" "Correct child 1"
          | _ -> failwith "Not text node"
          match worldNode with
          | Text worldStr -> Expect.equal worldStr " World" "Correct child 1"
          | _ -> failwith "Not text node"
        | _ -> failwith "More than 2 children"
      | _ -> failwith "Not html node"

      let goodbyeElement = element {Hello = "Goodbye"} []

      let goodbyeNode = castHTMLNode goodbyeElement
      match goodbyeNode with
      | Node (tag, _, children) ->
        Expect.equal tag "div" "Correct tag"
        Expect.hasLength children 2 "Correct length"
        match List.ofSeq children with
        | [helloElement; worldElement] -> 
          let helloNode = castHTMLNode helloElement
          let worldNode = castHTMLNode worldElement
          match helloNode with
          | Text helloStr -> Expect.equal helloStr "Goodbye" "Correct child 1"
          | _ -> failwith "Not text node"
          match worldNode with
          | Text worldStr -> Expect.equal worldStr " World" "Correct child 1"
          | _ -> failwith "Not text node"
        | _ -> failwith "More than 2 children"
      | _ -> failwith "Not html node"
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args allTests