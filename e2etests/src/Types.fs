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
  | ClearTodo of string
  | SaveTodo of string * string

[<CustomEquality; NoComparison>]
type ItemProps = { 
  SendEvent: Action -> unit 
  Todo: Todo
} with 
    override _.Equals __ = false
    override _.GetHashCode() = 1

