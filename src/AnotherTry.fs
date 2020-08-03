module AnotherTry
open Fable.React
open Browser.Types
open Browser
open Fable.Core
open Fable.React.Props

// type HacnControl<'returnType> =
//     | Render of ReactElement
//     | RenderControl of ReactElement * ('returnType -> HacnControl<'returnType>)
//     | State of 'returnType
//     | StateControl of 'returnType * ('returnType -> HacnControl<'returnType>)
//     | Props
//     | PropsControl of ('returnType -> HacnControl<'returnType>)
//     | Context of IContext<'returnType>
//     | ContextControl of IContext<'returnType> * ('returnType -> HacnControl<'returnType>)
//     | End
type RefState<'props> =
  {
    PrevProps: 'props option;
    CurrentProps: 'props;
    Element: ReactElement option;
  }

type Hacn<'props, 'returnType> =
  { 
    Invoke: RefState<'props> -> (RefState<'props> * Hacn<'props, unit> option * 'returnType);
    Change: RefState<'props> -> bool;
  }

let Props() =
  { 
    Invoke = fun refState -> (refState, None, refState.CurrentProps);
    Change = fun (refState) -> 
      match refState.PrevProps with
        | Some(prevProps) -> prevProps <> refState.CurrentProps
        | None -> true;
  }

let Render(element) =
  { 
    Invoke = fun (refState) -> 
      ({refState with Element = Some(element)}, None, ());
    Change = fun (_) -> false
  }

let runOperation refState operation =
  operation.Invoke refState

type TestProps =
  { Hello: string}

type RenderResponse =
  { World: string}

type ContextResponse =
  { Everyone: string}

type HacnBuilder(useRef) = 
  member this.Bind(operation, f) =
    { 
      Invoke = 
        fun (refState) -> 
          let (nextRefState, _, nextValue) = operation.Invoke(refState)
          let nextOperation = f(nextValue)
          (nextRefState, Some(nextOperation), ())
      Change =
        fun (refState) ->
          operation.Change(refState)
    }
  member this.Zero() =
    { 
      Invoke = fun (refState) -> (refState, None, ())
      Change = fun (refState) -> false
    }
  member this.Delay(f) =
    f
  member this.Run(f) =
    let render (props: 'props) =
      let refState = useRef({
        PrevProps = None;
        CurrentProps = props;
        Element = None;
      })
      // TODO: implement rendering logic
      div [||] [| str "Hello world!" |]
    
    let ofHacn (props: 'props) children = 
      ofFunction 
        render
        props 
        children
    ofHacn

let useFakeRef initialValue =
  let mutable refValue = initialValue
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

let hacn = HacnBuilder(useFakeRef)

let context = createContext({Everyone = "Everyone"})

let element = hacn {
  let! props = Props()

  let! clicked = Render (button [| OnClick (fun (event) -> failwith "TODO: Logic to capture events here") |] [||])

  do! Render (div [||] [| str "Test Element"; str props.Hello; str "World" |])
}

let x = element "hello" [||] 