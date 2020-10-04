[<AutoOpen>]
module Hacn.Types

open Fable.React

type CoreOperationTypes =
  | PropsOperation
  | StateGet
  | StateSet
  | NotCore

type ReRender = (obj option -> obj option) -> unit
type Dispose = (obj option -> obj option) option
type Effect = ReRender -> Dispose

type CaptureReturn = (obj option -> unit)

type InvokeResult<'props, 'returnType> =
  // | InvokeBoth of ReactElement * Effect
  // | InvokeRender of ReactElement
  // | InvokeEffect of Effect
  // | InvokeReturn of 'returnType
  | InvokeWait of ReactElement option * Effect option
  | InvokeContinue of ReactElement option * Effect option * 'returnType
and PerformData<'props, 'returnType> =
  { 
    OperationType: CoreOperationTypes;
    PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> InvokeResult<'props, 'returnType>;
  }

and ControlResult<'props> =
//   | ControlBoth of ReactElement * Effect
//   | ControlRender of ReactElement
//   | ControlEffect of Effect
//   | ControlNextOperation of Operation<'props, unit>
//   | ControlWait
  | ControlWait of ReactElement option * Effect option
  | ControlNext of ReactElement option * Effect option * Operation<'props, unit>
and ControlData<'props> =
  { 
    OperationType: CoreOperationTypes;
    PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> ControlResult<'props>;
  }
and Operation<'props, 'returnType> =
  | Perform of PerformData<'props, 'returnType>
  | Control of ControlData<'props>
  | End