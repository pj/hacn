[<AutoOpen>]
module Rewrite.Types

open Fable.React

type Disposer = unit -> unit
type Effect = unit -> Disposer option

type OperationContents<'returnType> = {
  Run: ('returnType -> unit) -> OperationResult<'returnType>
}
and ExecutionContents<'returnType> = {
  Execute: int -> NextSetter<'returnType> -> ExecutionResult<'returnType>
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
  Effects: (int * Effect) list
  LayoutEffects: (int * Effect) list
}
and NextValue<'props> = {
  Next: GetNext option
  PropsNext: GetNextProps<'props> option
  Index: int
}
and NextSetter<'returnType> = NextValue<'returnType> -> unit
and GetNextProps<'returnType> = 'returnType -> NextResult
and GetNext = unit -> NextResult
and ExecutionResult<'returnType> = {
  ReturnValue: 'returnType option
  Element: ReactElement option
  Effects: (int * Effect) list
  LayoutEffects: (int * Effect) list
}
and Builder<'returnType> =
  | Delay of (unit -> Builder<'returnType>)
  | Operation of OperationContents<'returnType>
  | Execution of ExecutionContents<'returnType>
  | Props
  // | PropsExecution of ExecutionContents<'returnType>
  | End