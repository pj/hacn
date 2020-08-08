module Test

open Expecto
open Hopac
open Logary.Configuration
open Logary.Targets
open Logary.Adapters.Facade
open Fable.React.Helpers
open Fable.React.Standard
open Fable.React
open Fable.React.Props
open Fable.ReactServer
open Hacn

type TestProps =
  { Hello: string}

type ContextResponse =
  { Everyone: string}

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

let useFakeContext (context) =
  { Everyone = "Everyone"}

let TestContext = ContextNonPartial useFakeContext

type TestHacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(operation, f) = bind operation f
  member this.Zero() = zero()
  member this.Delay(f) = f
  member this.Run(f) =
    fun props -> castHTMLNode (render useRef f props)

let testHacn = TestHacnBuilder(useFakeRef)

[<Tests>]
let tests = 
  test "A useful test of Hacn" {

    let context = createContext({Everyone = "Everyone"})
    let x = Hooks.useContext

    let element = testHacn {
      let! props = Props()
      let! myContext = TestContext context

      let! clicked = Render (button [ OnClick (fun (event) -> failwith "TODO: Logic to capture events here") ] [])

      do! Render (div [] [ str props.Hello; str "World"; str myContext.Everyone ])
    }

    let x = element {Hello = "Hello"}
    match x with 
      | Node(elementName, _, _) -> Expect.equal elementName "div" "div name is correct"
      | _ -> failwith "Not a node"
  }

[<EntryPoint>]
let main argv =
  let logary =
    Config.create "MyProject.Tests" "localhost"
    |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
    |> Config.processing (Events.events |> Events.sink ["console";])
    |> Config.build
    |> run
  LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

  // Invoke Expecto:
  runTestsInAssemblyWithCLIArgs [] argv