module Item

open Types
open Browser.Dom
open Feliz
open Fable.React
open Hacn.Core
open Hacn.Operations

let Item = 
  react {
    let! props = Props
    let editing = false
    let editText = props.Title
    do! Render Html.li [
        if props.Completed then
          prop.className "completed"
        if editing then
          prop.className "editing"
        prop.children [
          Html.div [
            prop.className "view"
            prop.children [
              Html.input [
                prop.className "toggle"
                prop.type' "checkbox"
                prop.defaultChecked props.Completed
                // prop.onChange (fun _ -> ())
              ]
              Html.label [
                prop.onDoubleClick (fun _ -> ())
                prop.text props.Title
              ]
              Html.button [
                prop.className "destroy"
                prop.onClick (fun _ -> ())
              ]
            ]
          ]
          Html.input [
            // prop.ref "editField"
            prop.className "edit"
            prop.value editText
            prop.onBlur (fun _ -> ())
            // prop.onChange (fun _ -> ())
            prop.onKeyDown (fun _ -> ())
          ]
        ]
      ]
  }