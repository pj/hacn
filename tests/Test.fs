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


// Separated types for matching overloads in Bind
type Render =
  | Render of ReactElement
type RenderContinue =
  | RenderContinue of ReactElement
type Props =
  | Props
type State =
  | State
type Context =
  | Context

// Combined operations for storage in HacnControl
// type HacnOperations = 
//   | PropsOperation

type HacnControl<'props> =
    | PropsControl of ('props -> HacnControl<'props>)
    | ContextControl of ('props -> HacnControl<'props>)
    | End

// let Continue underlying = 
//   match underlying with 
//     | Render(element) -> RenderContinue(element)

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

type RefState<'props> =
  {
    NextControl: HacnControl<'props> option; 
    PrevProps: 'props option;
    PropsControl: HacnControl<'props> option;
  }

type HacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(x: Props, f: 'props -> HacnControl<'props>) =
    PropsControl(fun (props: 'props) -> f(props))
  member this.Bind(x: Context, f: 'props -> HacnControl<'props>) =
    ContextControl(fun (props: 'props) -> f(props))
  member this.Bind(x: State, f: string -> HacnControl<'props>) =
    f("asdfasdf")
  member this.Bind(x: Render, f: string -> HacnControl<'props>) =
    f("sadf")
  member this.Bind(x: RenderContinue, f: string -> HacnControl<'props>) =
    f("sadf")
  member this.Bind(x: Render, f: unit -> HacnControl<'props>) =
    f()
  member this.Zero() =
    // handle case of ending here
    End
  // member this.Combine(a: HacnControl, b: unit -> HacnControl) =
  //   // if a is suspend then return operation
  //   failwith "dddd"
  member this.Delay(f: unit -> HacnControl<'props>) =
    f
  member this.Run(delayedFunc: unit -> HacnControl<'props>) =
    let render (props: 'props) =
      let refState = useRef({
        NextControl = None; 
        PrevProps = None;
        PropsControl = None;
      })
      let nextControl = 
        match refState.current with 
          | {NextControl = None} -> 
            let nextControl = delayedFunc()
            refState.current <- {
              refState.current with NextControl = Some(delayedFunc()); 
              }
            nextControl
          | {NextControl = Some(nextControl)} -> nextControl
      
      // Check if props has changed
      match refState.current with 
        | {PrevProps = Some(_); PropsControl = None} ->
          // Not really sure what to do but log a warning here?
          failwith "Can't do anything"


      // Check if state variable has changed
      // Check if callback called
      
      // If next control is End then render the last element

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