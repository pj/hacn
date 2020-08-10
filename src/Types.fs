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
    // InitialOperationState: 'props -> OperationState
    NeedsPreprocess: bool;
    PreProcess: OperationState option -> (OperationState option);
    Invoke: OperationState option -> (InvokeResult<'props> * 'returnType);
    IsProps: bool;
  }
