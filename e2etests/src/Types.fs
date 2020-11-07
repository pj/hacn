module Types

type Filter =
  | All
  | Completed
  | NotCompleted

type Todo = {Id: string; Title: string; Completed: bool}

type Action =
  | ClearCompleted
  | SetAllCompleted
  | SetAllNotCompleted
  | AddTodo of string
  | ToggleTodo of string
  | SaveTodo of string * string

