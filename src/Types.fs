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
  LayoutEffect: (unit -> Disposer option) option
}
and SideEffectsFunction<'opContents> = SetResult<'opContents> -> OperationSideEffects
  // Element: (SetResult<'opContents> -> ReactElement) option
  // Effect: (SetResult<'opContents> -> Disposer option) option
  // LayoutEffect: ((SetResult<'opContents> -> unit) * Disposer option) option
  // Hook: HookFunc<'opContents> option
and OperationResult<'opContents> =
  | OperationWait of SideEffectsFunction<'opContents>
  | OperationContinue of (SetResult<'opContents> -> OperationSideEffects) * 'opContents
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

and NextResult = {
  OperationsToBind: (SetNext -> OperationSideEffects) list
}
and SetNext = (obj -> NextResult) -> unit
and ExecutionResult<'opContents> = {
  ReturnValue: 'opContents option
  OperationsToBind: (SetNext -> OperationSideEffects) list
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