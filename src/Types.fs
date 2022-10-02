[<AutoOpen>]
module Hacn.Types

open Fable.React

type Disposer = unit -> unit
type Effect = unit -> Disposer option

type OperationContents<'opContents> = {
  Run: ('opContents -> unit) -> obj -> OperationResult<'opContents>
}
and ExecutionContents<'opContents> = {
  Execute: int -> obj -> NextSetter -> ExecutionResult<'opContents>
}
and HookFunc<'opContents> = obj -> 'opContents option
and ContinueContents<'opContents> = {
  ReturnValue: 'opContents
  Element: ReactElement option
  Effect: Effect option
  LayoutEffect: Effect option
  Hook: HookFunc<'opContents> option
}
and WaitContents<'opContents> = {
  Element: ReactElement option
  Effect: Effect option
  LayoutEffect: Effect option
  Hook: HookFunc<'opContents> option
}
and OperationResult<'opContents> =
  | OperationWait of WaitContents<'opContents>
  | OperationContinue of ContinueContents<'opContents>
and NextResult = {
  Element: ReactElement option
  Effects: (int * Effect) list
  LayoutEffects: (int * Effect) list
  // PropsNext: (int * GetNext) option
  Hooks: (int * GetNextHook) list
}
and NextValue = {
  Next: GetNext option
  Index: int
}
and NextValueHooks = {
  Next: GetNext
  Index: int
}
and NextSetter = NextValue -> unit
and GetNext = obj -> NextResult
and GetNextHook = obj -> NextResult option
and ExecutionResult<'opContents> = {
  ReturnValue: 'opContents option
  Element: ReactElement option
  Effects: (int * Effect) list
  LayoutEffects: (int * Effect) list
  // PropsNext: (int * GetNext) option
  Hooks: (int * GetNextHook) list
}
and Builder<'opContents> =
  | Delay of (unit -> Builder<'opContents>)
  | Operation of OperationContents<'opContents>
  | Execution of ExecutionContents<'opContents>
  // | Props
  | End