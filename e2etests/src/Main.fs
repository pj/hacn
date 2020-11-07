module Main

open Feliz
open Item
open Types

type MainProps = {
  MarkCompleted: Action -> unit
  Todos: Todo list
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
              fun isChecked -> props.MarkCompleted (
                if isChecked then SetAllCompleted else SetAllCompleted
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
              for todo in props.Todos do Item todo
            ]
          ]
        ]
      ]
  )