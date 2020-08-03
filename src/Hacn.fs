module Hacn
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
  { Invoke: RefState<'props> -> 'returnType}

let getProps() =
  { Invoke = fun (refState) -> 
    refState.CurrentProps
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
  member this.Bind(x, f) =
    { Invoke = 
      fun (refState) -> 
        runOperation refState x 
    }
  member this.Zero() =
    { Invoke = 
      fun (refState) ->
        null
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
  // Props corresponds to props in react and would act kind of like a stream. 
  // When the component rerenders props might have changed and so the 
  // computation would restart from here.
  let! props = getProps()

  // Context would work similarly to props.
  let! contextResponse = Context(context)

  // Renders can capture events that happen at a lower level and return them 
  // into the computation.
  let! clicked = Render (button [| OnClick (fun (event) -> failwith "TODO: Logic to capture events here") |] [||])

  // Render everything, not capturing events.
  do! Render (div [||] [| str "Test Element"; str props.Hello; str "World"; str contextResponse.Everyone |])
}

let x = element "hello" [||] 