module Test

open Expecto
open Hopac
open Logary.Configuration
open Logary.Adapters.Facade
open Logary.Targets
// open Hacn

type 'a GeneratorNext =
  | Node of 'a * (unit -> GeneratorNext<'a> option)
  | Done

type 'a Generator(delayedFunc: unit -> GeneratorNext<'a>) =
  let mutable nextFunc: GeneratorNext<'a> option = Some(delayedFunc())
  member this.Next() = 
    match nextFunc with 
      | Some(Node(result, asdf)) -> 
        nextFunc <- asdf()
        result
      | Some(Done) ->
        failwith "Generator ended!"
      | None ->
        failwith "Generator ended!"
  member this.Complete() =
    match nextFunc with
      | None -> true
      | _ -> false
        

type GeneratorBuilder() = 
  // member this.Bind(x, f) =
  //   f(x)
  member this.Yield(x) =
    x
  member this.Combine(a, b) =
    Some(Node(a, b))
  // member this.Zero(a) =
  //   Node(a, fun () -> None)
  member this.Delay(f) =
    f
  member this.Run(f) =
    fun () -> Generator(f)

let generator = GeneratorBuilder()

[<Tests>]
let tests =
  test "A simple test" {
    let interator = generator {
        yield 1
        yield 2
        yield 3
      }
    
    let x = interator()

    let one = x.Next()
    Expect.equal one 1 "Should equals 1"
    let two = x.Next()
    Expect.equal two 2 "Should equals 2"
    let three = x.Next()
    Expect.equal three 3 "Should equals 3"
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