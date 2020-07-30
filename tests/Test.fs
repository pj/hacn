module Test

open Expecto
// open Expecto.Logging
// open Expecto.Logging.Message
open Hopac
open Logary.Configuration
open Logary.Adapters.Facade
open Logary.Targets
open Logary
open Fable.React
// open Logary.Message
// open Hacn
open System
open System.IO

type 'a GeneratorNext =
  | Node of 'a * (unit -> GeneratorNext<'a>)
  | Done

type 'a Generator(delayedFunc) =
  let mutable nextFunc = delayedFunc()
  member this.Next() = 
    match nextFunc with 
      | Node(result, asdf) -> 
        nextFunc <- asdf()
        result
      | Done ->
        failwith "Error"
  member this.Complete() =
    match nextFunc with
      | Done -> true
      | _ -> false

type 'a GeneratorBuilder() = 
  // member this.Bind(x, f) =
  //   f(x)
  // member this.Zero() =
  //   Done
  member this.Yield(x) =
    Node(x, fun () -> Done)
  member this.Combine(a: 'a GeneratorNext, b) =
    match a with
      | Node(value, _) ->
        Node(value, b)
      | Done -> failwith "shouldn't happen"
  member this.Delay(f) =
    f
  member this.Run(f) =
    fun () -> Generator(f)

let generator: int GeneratorBuilder = GeneratorBuilder()

[<Tests>]
let tests =
  test "A simple test" {
    // let logger = Log.create "asdf.qwer"
    // let logger = Log.create "asdf"
    let interator = generator {
        yield 1
        yield 2
        yield 3
      }
    
    let x = interator()

    let one = x.Next()
    // logger.logSimple (Message.eventFormat (Debug, "{number}", one))
    // ignore (logger.logWithAck Debug (eventX "asdf"))
    // logger.logSimple ((eventX "asdfsadf") Debug)
    Expect.equal one 1 "Should equals 1"
    let two = x.Next()
    // logger.logSimple (Message.eventFormat (Debug, "{number}", two))
    Expect.equal two 2 "Should equals 2"
    let three = x.Next()
    // logger.logSimple (Message.eventFormat (Debug, "{number}", three))
    Expect.equal three 3 "Should equals 3"
  }

// type HacnOperations = 
//     | Render of ReactElement
//     | Props
//     | State

type Render =
  | Render of ReactElement
type Props =
  | Props
type State =
  | State

type HacnControl =
    | Continue
    | Suspend

// type 'a GeneratorNext =
//   | Node of 'a * (unit -> GeneratorNext<'a>)
//   | Done

type HacnGenerator<'props>(initialFunc) =
  let mutable nextFunc  = None
  let mutable prevProps: 'props option = None
  // member this.Next() = 
  //   match nextFunc with 
  //     | Node(result, asdf) -> 
  //       nextFunc <- asdf()
  //       result
  //     | Done ->
  //       failwith "Error"
  // member this.Complete() =
  //   match nextFunc with
  //     | Done -> true
  //     | _ -> false
  member this.Render(props: 'props) =
    failwith "asdf"

let useFakeRef initialValue =
    { new IRefValue<_> with
        member __.current with get() = initialValue and set _ = () }

type RefState =
  {X: int}

type HacnBuilder<'props>(useRef: RefState -> IRefValue<RefState>) = 
  member this.Bind(x: Props, f: string -> HacnControl) =
    f("heelo")
  member this.Bind(x: Render, f: string -> HacnControl) =
    f("sadf")
  member this.Bind(x: State, f: string -> HacnControl) =
    f("asdfasdf")
  member this.Bind(x: Render, f: unit -> HacnControl) =
    f()
  member this.Zero() =
    failwith "asdf"
  member this.Yield(x) =
    failwith "llll"
  member this.Combine(a, b) =
    failwith "dddd"
  member this.Delay(f) =
    f
  member this.Run(f) =
    let render (props: 'props) =
      let refState = useRef({X = 1})
      div [||] [| str "Hello world!" |]
    let ofHacn (props: 'props) children = 
      ofFunction 
        render
        props 
        children
    ofHacn

let hacnTest = HacnBuilder(useFakeRef)
// let hacn = HacnBuilder(Hooks.useRef)

[<Tests>]
let hacnTests =
  test "A hacn test" {
    let element = hacnTest {
        let! props = Props
        do! Render (div [||] [| str "Test Element"; str props |])
      }
    
    // let ddd = async {
    //   do! Async.Sleep 100
    //   do! File.ReadAllBytesAsync "hello.txt" |> Async.AwaitTask
    // }
    
    let x = element "hello" [||] 

    Expect.equal 3 3 "Should equals 3"
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