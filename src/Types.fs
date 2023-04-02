[<AutoOpen>]
module Hacn.Types

open Fable.React

type Disposer = unit -> unit
and SetResult<'returnType> = 'returnType -> unit
and OperationSideEffects<'returnType> = {
  Element: ReactElement option
  Effect: (unit -> Disposer option) option
  LayoutEffect: (unit -> Disposer option) option
  Hook: (obj -> 'returnType) option
}
and SideEffectsFunction<'returnType> = SetResult<'returnType> -> OperationSideEffects<'returnType>
and OperationResult<'returnType> =
  | OperationWait of SideEffectsFunction<'returnType>
  | OperationContinue of (SetResult<'returnType> -> OperationSideEffects<'returnType>) * 'returnType
and NextResult<'returnType> = {
  OperationsToBind: (SetNext<'returnType> -> OperationSideEffects<'returnType>) list
}
and SetNext<'returnType> = (obj -> NextResult<'returnType>) -> unit
and ExecutionResult<'returnType> = {
  ReturnValue: 'returnType option
  OperationsToBind: (SetNext<'returnType> -> OperationSideEffects<'returnType>) list
}
and ExecutionContents<'returnType> = {
  Execute: obj -> ExecutionResult<'returnType>
}
and Builder<'returnType> =
  | Delay of (unit -> Builder<'returnType>)
  | Operation of (obj -> OperationResult<'returnType>)
  | Execution of ExecutionContents<'returnType>
  | Return of 'returnType
  | End