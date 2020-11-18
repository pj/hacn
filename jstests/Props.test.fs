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
    console.log "Before props"
    let! props = Props
    console.log "After props"
    let! ref = Ref None
    console.log "Before Header render"
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
                console.log "Capture key down"
                capture keyEvent.key)
              prop.autoFocus true
              prop.type' "text"
            ]
          ]
        ]
    )
    console.log "After Header render"

    if key = "Enter" then
      match ref.current with
      | Some(element) -> 
        let inputElement = box element :?> HTMLInputElement
        console.log("Before calling Header callback.")
        do! Call (fun () -> (props.TestFunc inputElement.value) )
      | None -> failwith "Ref not set"
  }

type TestState = {Text: string}

let App =
  react {
    let! (state, setState) = Get {Text = ""}
    console.log "Before Rendering Main"

    let! result = 
      RenderCapture (
        fun capture -> Html.div [
          Header { 
            TestFunc = fun result -> 
              console.log "App capture"
              capture result 
          }
        ]
      )
    
    console.log "Before Updating State"
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