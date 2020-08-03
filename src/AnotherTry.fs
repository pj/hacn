module AnotherTry
open Fable.React
open Browser.Types
open Browser
open Fable.Core

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
  }

type Hacn<'props, 'returnType> =
  { 
    Invoke: RefState<'props> -> (ReactElement option * 'returnType);
    Change: RefState<'props> -> bool;
    Next: 'returnType -> Hacn<'props, _>
  }

let Props() =
  { Invoke = fun (refState) -> (None, refState.CurrentProps);
    Change = fun (refState) -> 
      match refState.PrevProps with
        | Some(prevProps) -> prevProps <> refState.CurrentProps
        | None -> true;
    Next = fun (nextValue, refState)
    ;
  }

let Render(element) =
  { Invoke = fun (_) -> (element, ());
    Change = fun (refState) -> false
  }

let runOperation refState operation =
  operation.Invoke refState

type TestProps =
  { Hello: string}

type RenderResponse =
  { World: string}

type ContextResponse =
  { Everyone: string}

type HacnBuilder() = 
  member this.Bind(operation, f) =
    { 
      Invoke = 
        fun (refState) -> 
          let (element, nextValue) = operation.Invoke(refState)
          f(nextValue)
      Change =
        fun (refState) ->
          operation.Change(refState)
    }
  member this.Zero() =
    { 
      Invoke = fun (refState) -> (None, ())
      Change = fun (refState) -> false
    }
  member this.Delay(f) =
    f
  member this.Run(delayedFunc) =
    let render (props: 'props) =
      // TODO: implement rendering logic
      div [||] [| str "Hello world!" |]
    
    let ofHacn (props: 'props) children = 
      ofFunction 
        render
        props 
        children
    ofHacn

let hacn = HacnBuilder()

let context = createContext({Everyone = "Everyone"})

let element = hacn {
  let! props = Props()

  let! clicked = Render (button [| OnClick (fun (event) -> failwith "TODO: Logic to capture events here") |] [||])

  do! Render (div [||] [| str "Test Element"; str props.Hello; str "World"; str contextResponse.Everyone |])
}

let x = element "hello" [||] 