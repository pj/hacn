module Test

open Expecto
open Hopac
open Fable.React
open Microsoft.FSharp.Control.CommonExtensions   
open Microsoft.FSharp.Control.WebExtensions

type HacnControl<'returnType> =
    | Render of ReactElement
    | RenderControl of ReactElement * ('returnType -> HacnControl<'returnType>)
    | State
    | Props
    | PropsControl of ('returnType -> HacnControl<'returnType>)
    | Context
    | ContextControl of ('returnType -> HacnControl<'returnType>)
    | End

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

type RefState<'props, 'returnType> =
  {
    NextControl: HacnControl<'returnType> option; 
    PrevProps: 'props option;
    PropsControl: HacnControl<'props> option;
  }

type TestProps =
  { Hello: string}

type HacnBuilder(useRef) = 
  member this.Bind(x: HacnControl<'a>, f) =
    match x with
      | Props ->  PropsControl(fun (props) -> f(props))
      | Render(element) -> RenderControl(element, fun (renderResult) -> f(renderResult))
      | _ -> failwith "Unimplemented"
  member this.Zero() =
    End
  member this.Delay(f) =
    f
  member this.Run(delayedFunc) =
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
        do! Render (div [||] [| str "Test Element"; str props.Hello |])
      }
    
    let url = "https://google.com"
    let asdf = async {                             
        let uri = System.Uri(url)
        let webClient = new System.Net.WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        do! Async.Sleep(1)
        let! data = webClient.AsyncDownloadData(uri)
        return html
      }
    
    let x = element "hello" [||] 

    Expect.equal 3 3 "Should equals 3"
  }

[<EntryPoint>]
let main argv =
  // let logary =
  //   Config.create "MyProject.Tests" "localhost"
  //   |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
  //   |> Config.processing (Events.events |> Events.sink ["console";])
  //   |> Config.build
  //   |> run
  // LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary


  // Invoke Expecto:
  runTestsInAssemblyWithCLIArgs [] argv