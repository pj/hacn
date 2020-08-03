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
open Hacn

type TestProps =
  { Hello: string}

// type RenderResponse =
//   { World: string}

type ContextResponse =
  { Everyone: string}

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

[<Tests>]
let tests = 
  test "A useful test of Hacn" {
    let hacn = HacnBuilder(useFakeRef)

    let context = createContext({Everyone = "Everyone"})

    let element = hacn {
      let! props = Props()

      let! clicked = Render (button [ OnClick (fun (event) -> failwith "TODO: Logic to capture events here") ] [])

      do! Render (div [] [ str "Test Element"; str props.Hello; str "World" ])
    }

    let x = element {Hello = "Hello"} [] 
    Expect.equal 1 1 "One equals one"
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