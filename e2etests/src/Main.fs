module Main

open Feliz
open Item
open Types

type MainProps = {
  SendEvent: Action -> unit
  Todos: Todo list
  CurrentFilter: Filter
  ActiveTodoCount: int
}

let Main =
  React.functionComponent (
    fun props ->
      Html.section [
        prop.className "main"
        prop.children [
          Html.input [
            prop.id "toggle-all"
            prop.className "toggle-all"
            prop.type' "checkbox"
            prop.onChange (
              fun isChecked -> props.SendEvent (
                if isChecked then SetAllCompleted else SetAllNotCompleted
              )
            )
            prop.isChecked (props.ActiveTodoCount = 0)
          ]
          Html.label [
            prop.for' "toggle-all"
            prop.text "Mark all as complete"
          ]
          Html.ul [
            prop.className "todo-list"
            prop.children [
              for todo in props.Todos 
                do if props.CurrentFilter = All 
                  || (props.CurrentFilter = Completed && todo.Completed) 
                  || (props.CurrentFilter = NotCompleted && not todo.Completed) then
                  Item {SendEvent = props.SendEvent; Todo = todo}
            ]
          ]
        ]
      ]
  )