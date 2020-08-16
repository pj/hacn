[<AutoOpen>]
module Hacn.Types

open Fable.React

type OperationState = 
  new() = {}

type InvokeResult<'props> =
  {
    // The next operation to perform after this, can be Suspend, if the 
    // component is waiting for some effect too complete.
    NextOperation: Operation<'props, unit> option;
    // An element to render.
    Element: ReactElement option;
    // Any modifications to the operation state made as a result of invoking the 
    // operation.
    UpdatedOperationState: OperationState option;
    // A function to run to perform some type of effect using the useEffect hook,
    // after the component has rendered.
    Effect: ((unit -> OperationState option) -> (OperationState option -> unit) -> unit) option
  }
and Operation<'props, 'returnType> =
  // Righthand side of the bind, contains the logic for the operation
  | Perform of OperationData<'props, 'returnType>
  // Tell the runtime to suspend, probably waiting for an Effect to complete.
  | Suspend
  // End marker for the components operations
  | End
and OperationData<'props, 'returnType> =
  { 
    // NeedsPreprocess: bool;
    // Hack needed to determine if the operation is props and therefore needs
    // special handling by the runtime.
    IsPropsOperation: bool;
    // For hooks like useContext and useRef, we have to run them at the 
    // beginning in order on every render.
    PreProcess: OperationState option -> OperationState option;
    // Invoke this operation with the current operation state, and return the 
    // result of the operation to be passed to the next operations and various 
    // things to return to the runtime.
    Invoke: OperationState option -> (InvokeResult<'props> * 'returnType);
  }