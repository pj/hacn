[<AutoOpen>]
module Hacn.Types

open Fable.React

type Disposer = unit -> unit

// type HookFunc<'opContents> = obj -> 'opContents option
// and SetResult<'opContents> = 'opContents -> unit
and SetResult<'opContents> = 'opContents -> unit

and OperationSideEffects = {
  Element: ReactElement option
  Effect: (unit -> Disposer option) option
}
and ContinueContents<'opContents> = {
  ReturnValue: 'opContents
  SideEffects: SetResult<'opContents> -> OperationSideEffects

  // Element: (SetResult<'opContents> -> ReactElement) option
  // Effect: (SetResult<'opContents> -> Disposer option) option
  // LayoutEffect: ((SetResult<'opContents> -> unit) * Disposer option) option
  // Hook: HookFunc<'opContents> option
}
and WaitContents<'opContents> = {
  SideEffects: SetResult<'opContents> -> OperationSideEffects
}
and OperationResult<'opContents> =
  | OperationWait of WaitContents<'opContents>
  | OperationContinue of ContinueContents<'opContents>
and OperationContents<'opContents> = {
  Run: obj -> OperationResult<'opContents>
}
// {
//   Element: (SetResult<'opContents> -> ReactElement) option
//   Effect: (SetResult<'opContents> -> Disposer option) option
//   LayoutEffect: ((SetResult<'opContents> -> unit) * Disposer option) option
//   Hook: HookFunc<'opContents> option
// }

// and GetNext = obj -> (ExecutionStuff list) option
// and Effect = (((obj -> ExecutionStuff list) -> unit) -> unit) * (Disposer option)
// and GetElement = ((obj -> ExecutionStuff list) -> unit) -> ReactElement
// and ExecutionStuff = {
//   Element: GetElement option
//   Effect: Effect option
//   LayoutEffect: Effect option
//   Hook: GetNext option
// }

and SetNext = (obj -> OperationSideEffects list) -> unit
and BindOperation = SetNext -> OperationSideEffects
and ExecutionResult<'opContents> = {
  ReturnValue: 'opContents option
  OperationsToBind: BindOperation list
}
and ExecutionContents<'opContents> = {
  Execute: obj -> ExecutionResult<'opContents>
}
and Builder<'opContents> =
  | Delay of (unit -> Builder<'opContents>)
  | Operation of OperationContents<'opContents>
  | Execution of ExecutionContents<'opContents>
  | Return of 'opContents
  | End