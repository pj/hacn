module Item

open Feliz
open Hacn.Core
open Hacn.Operations
open Browser.Types
open Fable.ReactTestingLibrary
open Fable.Core.JS
open Fable.Mocha

// let Item = 
//   react {
//     let! ref = Ref None
//     let! editing, setEditing = Get false
//     if editing then
//       do! Call (fun () -> 
//         console.log "Focusing element"
//       )

//     let! isChecked = RenderCapture (
//       fun capture -> 
//         Html.input [
//           prop.ref ref
//           prop.className "toggle"
//           prop.type' "checkbox"
//           prop.testId "test"
//           prop.isChecked editing
//           prop.onChange (fun (isChecked: bool) -> capture isChecked)
//         ]
//       )
//     do! setEditing isChecked
//   }

// let itemTests =
//     testList "Props tests" [

//         testCase "asdf" <| fun () ->
//             let result = RTL.render(Item ())
//             let inputElement: HTMLInputElement = unbox (result.getByTestId "test")
//             Expect.isFalse inputElement.checked "Box checked" 

//             RTL.fireEvent.click(inputElement)
//             Expect.isTrue inputElement.checked "Box checked" 

//             RTL.fireEvent.click(inputElement)
//             Expect.isFalse inputElement.checked "Box checked" 

//             RTL.fireEvent.click(inputElement)
//             Expect.isTrue inputElement.checked "Box checked" 

//             RTL.fireEvent.click(inputElement)
//             Expect.isFalse inputElement.checked "Box checked" 

//             RTL.cleanup ()
//     ]

// // Mocha.runTests itemTests |> ignore