module Test

open Expecto
open Hopac
open Logary.Configuration
open Logary.Targets
open Logary.Adapters.Facade
open Hacn

[<Tests>]
let tests = 
  test "A useful test of Hacn" {
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