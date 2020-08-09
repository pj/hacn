[<AutoOpen>]
module Hacn.Types

open Fable.React

type OperationState = 
  new() = {}

type InvokeResult<'props> =
  {
    NextOperation: Operation<'props, unit> option;
    Element: ReactElement option;
    UpdatedOperationState: OperationState option;
  }
and Operation<'props, 'returnType> =
  { 
    InitialOperationState: 'props -> OperationState
    NeedsPreprocess: unit -> bool;
    PreProcess: OperationState -> (OperationState option);
    Invoke: OperationState -> (InvokeResult<'props> * 'returnType);
  }
