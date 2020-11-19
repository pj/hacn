module Footer

open Types
open Browser.Dom
open Feliz
open Fable.React
open Hacn.Core
open Hacn.Operations

type FooterProps = {
  ActiveTodos: int
  CompletedTodos: int
  CurrentFilter: Filter
  ClearCompleted: Browser.Types.MouseEvent -> unit
  }

let Footer = 
  React.functionComponent (
    fun props ->
      let activeWord = 
        match props.ActiveTodos with
        | 1 -> "todo"
        | _ -> "todos"
      Html.footer [
        prop.className "footer"
        prop.children [
          Html.span [
            prop.className "todo-count"
            prop.children [
              Html.strong (sprintf "%d" props.ActiveTodos)
              Html.text (sprintf " %s left" activeWord)
            ]
          ]
          Html.ul [
            prop.className "filters"
            prop.children [
              Html.li [
                Html.a [
                  prop.href "#/"
                  prop.text "All"
                  if props.CurrentFilter = All then
                    prop.className "selected"
                ]
              ]
              Html.li [
                Html.a [
                  prop.href "#/active"
                  prop.text "Active"
                  if props.CurrentFilter = NotCompleted then
                    prop.className "selected"
                ]
              ]
              Html.li [
                Html.a [
                  prop.href "#/completed"
                  prop.text "Completed"
                  if props.CurrentFilter = Completed then
                    prop.className "selected"
                ]
              ]
            ]
          ]
          if props.CompletedTodos > 0 then
            Html.button [
              prop.className "clear-completed"
              prop.onClick props.ClearCompleted
              prop.text "Clear completed"
            ]
        ]
      ]
  )
