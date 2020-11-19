module Header

open Feliz
open Hacn.Core
open Hacn.Operations
open Types
open Browser.Types
open Fable.Core.JS

[<CustomEquality; NoComparison>]
type HeaderProps = { AddTodo: Action -> unit }
  with 
    override _.Equals __ = false
    override _.GetHashCode() = 1

let Header : HeaderProps -> ReactElement = 
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
              prop.placeholder "What needs to be done?"
              prop.onKeyDown (fun keyEvent -> capture keyEvent.key)
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
        do! Call (fun () -> 
          props.AddTodo (AddTodo(inputElement.value))
          inputElement.value <- ""
        )
      | None -> failwith "Ref not set"
  }
