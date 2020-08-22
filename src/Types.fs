[<AutoOpen>]
module Hacn.Types

open Fable.React

type CoreOperationTypes =
  | PropsOperation
  | StateGet
  | StateSet
  | NotCore

type InvokeResult<'props, 'returnType> =
  // An element to render.
  | Render of ReactElement
  // Any modifications to the operation state made as a result of invoking the 
  // operation.
  // | UpdateState of obj
  // A function to run to perform some type of effect using the useEffect hook,
  // after the component has rendered.
  | Effect of ((unit -> obj option) -> (obj option -> unit) -> unit)
  | Result of 'returnType
  // The next operation to perform after this, can be Suspend, if the 
  // component is waiting for some effect too complete.
  | NextOperation of Operation<'props, unit>
and Operation<'props, 'returnType> =
  // Righthand side of the bind, contains the logic for the operation
  | Perform of OperationData<'props, 'returnType>
  // End marker for the components operations
  | End
and OperationData<'props, 'returnType> =
  { 
    // NeedsPreprocess: bool;
    // Hack needed to determine if the operation is props and therefore needs
    // special handling by the runtime.
    OperationType: CoreOperationTypes;
    // For hooks like useContext and useRef, we have to run them at the 
    // beginning in order on every render.
    PreProcess: obj option -> obj option;
    // Invoke this operation with the current operation state, and return the 
    // result of the operation to be passed to the next operations and various 
    // things to return to the runtime.
    Invoke: obj option -> InvokeResult<'props, 'returnType>;
  }
// and ControlData<'props> =
//   {
//     // NeedsPreprocess: bool;
//     // Hack needed to determine if the operation is props and therefore needs
//     // special handling by the runtime.
//     OperationType: CoreOperationTypes option;
//     // For hooks like useContext and useRef, we have to run them at the 
//     // beginning in order on every render.
//     PreProcess: obj option -> obj option;

//     Next: obj option -> Operation<'props, unit>
//   }