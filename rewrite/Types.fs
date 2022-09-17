[<AutoOpen>]
module Rewrite.Types

open Fable.React

type Effect = unit -> unit

type OperationContents<'returnType> = {
  Run: ('returnType -> unit) -> OperationResult<'returnType>
}
and ExecutionContents<'returnType, 'props> = {
  Execute: int -> NextSetter<'props> -> ExecutionResult<'returnType>
}
and ContinueContents<'returnType> = {
  ReturnValue: 'returnType
  Element: ReactElement option
  Effect: Effect option
  LayoutEffect: Effect option
}
and WaitContents = {
  Element: ReactElement option
  Effect: Effect option
  LayoutEffect: Effect option
}
and OperationResult<'returnType> =
  | OperationWait of WaitContents
  | OperationContinue of ContinueContents<'returnType>
and NextResult = {
  Element: ReactElement option
  Effects: Effect list
  LayoutEffects: Effect list
}
and NextValue<'props> = {
  Next: GetNext option
  PropsNext: GetNextProps<'props> option
  Index: int
}
and NextSetter<'props> = NextValue<'props> -> unit
and GetNextProps<'props> = 'props -> NextResult
and GetNext = unit -> NextResult
and ExecutionResult<'returnType> = {
  ReturnValue: 'returnType option
  Element: ReactElement option
  Effects: Effect list
  LayoutEffects: Effect list
}
and Builder<'returnType, 'props> =
  | Delay of (unit -> Builder<'returnType, 'props>)
  | Operation of OperationContents<'returnType>
  | Execution of ExecutionContents<'returnType, 'props>
  | Props of ExecutionContents<'returnType, 'props>
  | End