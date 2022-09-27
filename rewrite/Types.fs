[<AutoOpen>]
module Rewrite.Types

open Fable.React

type Disposer = unit -> unit
type Effect = unit -> Disposer option

type OperationContents<'opContents> = {
  Run: ('opContents -> unit) -> OperationResult<'opContents>
}
and ExecutionContents<'opContents> = {
  Execute: int -> obj -> NextSetter -> ExecutionResult<'opContents>
}
and ContinueContents<'opContents> = {
  ReturnValue: 'opContents
  Element: ReactElement option
  Effect: Effect option
  LayoutEffect: Effect option
  Hook: (unit -> unit) option
}
and WaitContents = {
  Element: ReactElement option
  Effect: Effect option
  LayoutEffect: Effect option
  Hook: (unit -> unit) option
}
and OperationResult<'opContents> =
  | OperationWait of WaitContents
  | OperationContinue of ContinueContents<'opContents>
and NextResult = {
  Element: ReactElement option
  Effects: (int * Effect) list
  LayoutEffects: (int * Effect) list
  PropsNext: (int * GetNext) option
  Hooks: (int * (unit -> unit)) list
}
and NextValue = {
  Next: GetNext option
  Index: int
}
and NextSetter = NextValue -> unit
and GetNext = obj -> NextResult
and ExecutionResult<'opContents> = {
  ReturnValue: 'opContents option
  Element: ReactElement option
  Effects: (int * Effect) list
  LayoutEffects: (int * Effect) list
  PropsNext: (int * GetNext) option
  Hooks: (int * (unit -> unit)) list
}
and Builder<'opContents> =
  | Delay of (unit -> Builder<'opContents>)
  | Operation of OperationContents<'opContents>
  | Execution of ExecutionContents<'opContents>
  | Props
  | End