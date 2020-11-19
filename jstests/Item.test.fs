module Item

open Feliz
open Hacn.Core
open Hacn.Operations
open Browser.Types
open Fable.ReactTestingLibrary
open Fable.Core.JS
open Fable.Mocha

let Item = 
  react {
    let! ref = Ref None
    let! editing, setEditing = Get false
    console.log (sprintf "is editing %b" editing)
    if editing then
      console.log "in editing call"
      do! Call (fun () -> 
        console.log "Focusing element"
      )

    console.log "Before rendering"
    let! isChecked = RenderCapture (
      fun capture -> 
        Html.input [
          prop.ref ref
          prop.className "toggle"
          prop.type' "checkbox"
          prop.testId "test"
          prop.isChecked editing
          prop.onChange (fun (isChecked: bool) -> 
            console.log (sprintf "checked %b" isChecked)
            capture isChecked)
        ]
      )
    console.log (sprintf "updating state %b" isChecked)
    do! setEditing isChecked
  }

let itemTests =
    testList "Props tests" [

        testCase "asdf" <| fun () ->
            let result = RTL.render(Item ())
            let inputElement: HTMLInputElement = unbox (result.getByTestId "test")
            Expect.isFalse inputElement.checked "Box checked" 
            console.log "1"

            RTL.fireEvent.click(inputElement)
            Expect.isTrue inputElement.checked "Box checked" 
            console.log "2"

            RTL.fireEvent.click(inputElement)
            Expect.isFalse inputElement.checked "Box checked" 
            console.log "3"

            RTL.fireEvent.click(inputElement)
            Expect.isTrue inputElement.checked "Box checked" 
            console.log "4"

            RTL.fireEvent.click(inputElement)
            Expect.isFalse inputElement.checked "Box checked" 
            console.log "5"

            RTL.cleanup ()
    ]

Mocha.runTests itemTests |> ignore