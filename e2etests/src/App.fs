module App

open Browser.Dom
open Feliz
open Fable.React
open Hacn.Core
open Hacn.Operations
open Footer
open Types
open Header
open Main
open Router

type AppState = {
  Todos: Todo list
}

let App = 
  react {
    let! rawRoute = HashRouter ()
    let currentFilter = 
      match rawRoute with
      | Some("#/active") -> NotCompleted
      | Some("#/completed") -> Completed
      | _ -> All 

    let! state = Get {Todos = []}

    let activeTodoCount = List.sumBy (fun item -> if not item.Completed then 1 else 0) state.Todos
    let completedTodoCount = List.sumBy (fun item -> if item.Completed then 1 else 0) state.Todos

    let! result = 
      RenderCapture (
        fun capture -> Html.div [
          Header { 
            AddTodo = fun result -> capture result 
          }

          if state.Todos.Length > 0 then
            Main {
              MarkCompleted = fun result -> capture result
              Todos = state.Todos
              ActiveTodoCount = activeTodoCount
            }

          if activeTodoCount > 0 || completedTodoCount > 0 then
            Footer {
              ActiveTodos = activeTodoCount
              CompletedTodos = completedTodoCount
              CurrentFilter = currentFilter
              ClearCompleted = fun _ -> capture ClearCompleted
            }
        ]
      )
    
    let updatedState =
      match result with 
      | ClearCompleted -> List.filter (fun todo -> todo.Completed) state.Todos
      | SetAllCompleted -> List.map (fun todo -> {todo with Completed = true}) state.Todos
      | SetAllNotCompleted -> List.map (fun todo -> {todo with Completed = false}) state.Todos
      | AddTodo(name) -> 
        let id = System.Random().Next ()
        List.append state.Todos [{Id = id.ToString (); Completed = false; Title = name}]
      | ToggleTodo(id) -> 
        List.map 
          (fun todo -> if todo.Id = id then {todo with Completed = not todo.Completed} else todo)
          state.Todos
      | SaveTodo(id, name) -> 
        List.map 
          (fun todo -> if todo.Id = id then {todo with Title = name} else todo)
          state.Todos
    
    do! Set({Todos = updatedState})
  }

ReactDom.render(
    App (),
    document.getElementById("app"))