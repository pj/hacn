module PropsTest
open Hacn.Core
open Hacn.Operations
open Fable.Jester
open Fable.ReactTestingLibrary
open Feliz
open Hacn.Types
open Browser.Types
open Fable.Core.JS
open Fable.Mocha

[<CustomEquality; NoComparison>]
type HeaderProps = { TestFunc: string -> unit }
  with 
    override _.Equals __ = false
    override _.GetHashCode() = 1

let Header = 
  react {
    let! props = Props
    let! ref = Ref None
    let! key = RenderCapture (
      fun capture ->
        Html.header [
          prop.className "header"
          prop.children [
            Html.h1 "todos"
            Html.input [
              prop.ref ref
              prop.className "new-todo"
              prop.testId "test"
              prop.placeholder "What needs to be done?"
              prop.onKeyDown (fun keyEvent -> 
                capture keyEvent.key)
              prop.autoFocus true
              prop.type' "text"
            ]
          ]
        ]
    )

    if key = "Enter" then
      match ref.current with
      | Some(element) -> 
        let inputElement = box element :?> HTMLInputElement
        do! Call (fun () -> (props.TestFunc inputElement.value) )
      | None -> failwith "Ref not set"
  }

type TestState = {Text: string}

let App =
  react {
    let! (state, setState) = Get {Text = ""}

    let! result = 
      RenderCapture (
        fun capture -> Html.div [
          Header { 
            TestFunc = fun result -> 
              capture result 
          }
        ]
      )
    
    do! setState {Text = result}
  }

let propsTests =
    testList "Props tests" [

        testCase "asdf" <| fun () ->
            let result = RTL.render(App ())
            let inputElement = result.getByTestId "test"
            RTL.fireEvent.keyDown(inputElement, [
              (Interop.mkKeyboardEventAttr "key" "H"); 
              (Interop.mkKeyboardEventAttr "code" 72);
              (Interop.mkKeyboardEventAttr "charCode" 72);
            ])
            RTL.fireEvent.keyDown(inputElement, [
              (Interop.mkKeyboardEventAttr "key" "i"); 
              (Interop.mkKeyboardEventAttr "code" 102);
              (Interop.mkKeyboardEventAttr "charCode" 102);
            ])
            RTL.fireEvent.keyDown(inputElement, [
              (Interop.mkKeyboardEventAttr "key" "Enter"); 
              (Interop.mkKeyboardEventAttr "code" 13);
              (Interop.mkKeyboardEventAttr "charCode" 13);
            ])
            RTL.cleanup ()
    ]

Mocha.runTests propsTests |> ignore