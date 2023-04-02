[<AutoOpen>]
module Hacn.Types

open Fable.React

type Disposer = unit -> unit
and SetResult<'returnType> = 'returnType -> unit
and OperationSideEffects<'returnType> = {
  Element: ReactElement option
  Effect: (unit -> Disposer option) option
  LayoutEffect: (unit -> Disposer option) option
  Hook: (obj -> 'returnType option) option
}
and ExecutionSideEffects = {
  Element: ReactElement option
  Effect: (unit -> Disposer option) option
  LayoutEffect: (unit -> Disposer option) option
  Hook: (obj -> ExecutionResult option) option
}
and OperationSideEffectsFunction<'returnType> = SetResult<'returnType> -> OperationSideEffects<'returnType>
and OperationResult<'returnType> =
  | OperationWait of OperationSideEffectsFunction<'returnType>
  | OperationContinue of (SetResult<'returnType> -> OperationSideEffects<'returnType>) * 'returnType
and SetNext = (obj -> ExecutionResult) -> unit
and ExecutionResult = {
  OperationsToBind: (SetNext -> ExecutionSideEffects) list
}
and ExecutionContents = {
  Execute: obj -> ExecutionResult
}
and Builder<'returnType> =
  | Delay of (unit -> Builder<'returnType>)
  | Operation of (obj -> OperationResult<'returnType>)
  | Execution of ExecutionContents
  | Return of 'returnType
  | End