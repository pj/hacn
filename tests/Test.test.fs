module Test

// open Fable.Mocha
// open Expecto
// open Hopac
// open Logary.Configuration
// open Logary.Targets
// open Logary.Adapters.Facade
// open Fable.React.Helpers
// open Fable.React.Standard
// open Fable.React
// open Fable.React.Props
// open Fable.ReactServer
// open Hacn
// open Browser.Dom
// open Fable.ReactTestingLibrary
open Fable.Jester

// type TestProps =
//   { Hello: string}

// type ContextResponse =
//   { Everyone: string}

// let useFakeRef initialValue =
//   let mutable refValue = initialValue
//   console.log "here"
//   { new IRefValue<_> with
//       member this.current with get() = refValue and set value = refValue <- value }

// let useFakeContext (context) =
//   { Everyone = "Everyone"}

// // let TestContext = ContextNonPartial useFakeContext

// type TestHacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
//   member this.Bind(operation, f) = bind operation f
//   member this.Zero() = zero()
//   member this.Delay(f) = f
//   member this.Run(f) =
//     fun props -> render useRef f props

// let testHacn = TestHacnBuilder(useFakeRef)

// // [<Tests>]
// let tests = testList "A useful test of Hacn" [
//     testCase "props test" <| fun () ->
//       let context = createContext({Everyone = "Everyone"})
//       let x = Hooks.useContext

//       let element = testHacn {
//         let! props = Props()
//         do! Render (div [] [ str props.Hello; str "World"])
//       }

//       let x = element {Hello = "Hello"}
//       RTL.render(x) |> ignore
//       // match x with 
//       //   | Node(elementName, _, _) -> Expect.equal elementName "div" "div name is correct"
//       //   | _ -> failwith "Not a node"
//   ]

// Mocha.runTests tests |> ignore

// // [<EntryPoint>]
// // let main argv =
// //   let logary =
// //     Config.create "MyProject.Tests" "localhost"
// //     |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
// //     |> Config.processing (Events.events |> Events.sink ["console";])
// //     |> Config.build
// //     |> run
// //   LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

// //   // Invoke Expecto:
// //   runTestsInAssemblyWithCLIArgs [] argv

Jest.describe("my tests", fun () ->
  Jest.test("water is wet", fun () ->
    Jest.expect("test").toBe("test")
    Jest.expect("test").not.toBe("somethingElse")
    Jest.expect("hi").toHaveLength(2)
    Jest.expect("hi").not.toHaveLength(3)

    // let context = createContext({Everyone = "Everyone"})
    // let x = Hooks.useContext

    // let element = testHacn {
    //   let! props = Props()
    //   do! Render (div [] [ str props.Hello; str "World"])
    // }

    // element {Hello = "Hello"}
    // ()
  )
)
