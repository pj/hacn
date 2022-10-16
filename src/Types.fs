[<AutoOpen>]
module Hacn.Types

open Fable.React

type Disposer = unit -> unit

type HookFunc<'opContents> = obj -> 'opContents option
and SetResult<'opContents> = 'opContents -> unit

and OperationContents<'opContents> = {
  Run: obj -> OperationResult<'opContents>
}
and OperationResult<'opContents> =
  | OperationWait of WaitContents<'opContents>
  | OperationContinue of ContinueContents<'opContents>
and ContinueContents<'opContents> = {
  ReturnValue: 'opContents
  Element: (SetResult<'opContents> -> ReactElement) option
  Effect: ((SetResult<'opContents> -> unit) * Disposer option) option
  LayoutEffect: ((SetResult<'opContents> -> unit) * Disposer option) option
  Hook: HookFunc<'opContents> option
}
and WaitContents<'opContents> = {
  Element: (SetResult<'opContents> -> ReactElement) option
  Effect: ((SetResult<'opContents> -> unit) * Disposer option) option
  LayoutEffect: ((SetResult<'opContents> -> unit) * Disposer option) option
  Hook: HookFunc<'opContents> option
}

and GetNext = obj -> (ExecutionStuff list) option
and Effect = (((unit -> ExecutionStuff list) -> unit) -> unit) * (Disposer option)
and GetElement = ((unit -> ExecutionStuff list) -> unit) -> ReactElement
and ExecutionContents<'opContents> = {
  Execute: obj -> ExecutionResult<'opContents>
}
and ExecutionStuff = {
  Element: GetElement option
  Effect: Effect option
  LayoutEffect: Effect option
  Hook: GetNext option
}

and ExecutionResult<'opContents> = {
  ReturnValue: 'opContents option
  ThingsToCapture: ExecutionStuff list
}
and Builder<'opContents> =
  | Delay of (unit -> Builder<'opContents>)
  | Operation of OperationContents<'opContents>
  | Execution of ExecutionContents<'opContents>
  | Return of 'opContents
  | End