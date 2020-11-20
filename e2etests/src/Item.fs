module Item

open Types
open Browser.Dom
open Feliz
open Fable.React
open Hacn.Core
open Hacn.Operations
open Browser.Types
// open Focus

type ItemEvent = 
| Toggled
| StartEdit
| Delete
| EditBlured
| EditKey of string
| EditChange of string

type ItemState = {
  Editing: bool
  StartFocus: bool
  EditText: string
}

let Item : ItemProps -> ReactElement =
  react {
    let! props = Props
    let! ref = Ref None
    let! editState, setEditState = Get {
      Editing = false
      StartFocus = false
      EditText = props.Todo.Title
    }
    do! Call (fun () -> 
      if editState.Editing && editState.StartFocus then
        let inputElement = box ref.current :?> HTMLInputElement
        inputElement.setSelectionRange (0, inputElement.value.Length)
        inputElement.focus ()
    )

    let! rowEvent = RenderCapture (
      fun capture -> 
        Html.li [
          if props.Todo.Completed then
            prop.className "completed"
          if editState.Editing then
            prop.className "editing"
          prop.children [
            Html.div [
              prop.className "view"
              prop.children [
                Html.input [
                  prop.className "toggle"
                  prop.type' "checkbox"
                  prop.defaultChecked false
                  prop.isChecked props.Todo.Completed
                  prop.onChange (fun (_: bool) -> capture Toggled)
                ]
                Html.label [
                  prop.onDoubleClick (fun _ -> capture StartEdit)
                  prop.text props.Todo.Title
                ]
                Html.button [
                  prop.className "destroy"
                  prop.onClick (fun _ -> capture Delete)
                ]
              ]
            ]
            Html.input [
              prop.ref ref
              prop.className "edit"
              prop.value editState.EditText
              prop.type' "text"
              prop.onBlur (fun _ -> capture EditBlured)
              prop.onKeyDown (fun keyEvent -> capture (EditKey keyEvent.key))
              prop.onChange (
                fun (keyEvent: Browser.Types.Event) -> 
                  let inputElement = box keyEvent.target :?> HTMLInputElement
                  capture (EditChange inputElement.value)
                )
            ]
          ]
        ]
    )

    match rowEvent with 
    | Toggled -> 
      do! Call (fun () -> props.SendEvent (ToggleTodo props.Todo.Id))
    | StartEdit ->
      do! setEditState {
        Editing = true
        EditText = props.Todo.Title
        StartFocus = true
      }
    | Delete ->
      do! Call (fun () -> 
        props.SendEvent (ClearTodo props.Todo.Id)
      )
    | EditBlured ->
      match ref.current with
      | Some(element) -> 
        let inputElement = box element :?> HTMLInputElement
        do! Call (fun () -> 
          props.SendEvent (SaveTodo(props.Todo.Id, inputElement.value))
        )
        do! setEditState {editState with Editing = false}
      | None -> failwith "Ref not set"
    | EditKey(key) ->
      match key with 
      | "Enter" -> 
        match ref.current with
        | Some(element) -> 
          let inputElement = box element :?> HTMLInputElement
          do! Call (fun () -> 
            props.SendEvent (SaveTodo(props.Todo.Id, inputElement.value))
          )
          do! setEditState {editState with Editing = false; StartFocus = false}
        | None -> failwith "Ref not set"
      | "Escape" ->
        do! setEditState {
          Editing = false
          EditText = props.Todo.Title
          StartFocus = false
        }
      | _ -> ()
    | EditChange(value) ->
        do! setEditState {editState with EditText = value; StartFocus = false}
  }