[<AutoOpen>]
module Hacn.Types

open Fable.React

type CoreOperationTypes =
  | PropsOperation
  | StateGet
  | StateSet
  | NotCore

type ReRender = (obj option -> obj option) -> unit
type Dispose = (unit -> unit) option
type Effect = ReRender -> Dispose

type CaptureReturn = (obj option -> unit)

type InvokeResult<'props, 'returnType> =
  | InvokeBoth of ReactElement * Effect
  | InvokeRender of ReactElement
  | InvokeEffect of Effect
  | InvokeReturn of 'returnType
  | InvokeWait
and PerformData<'props, 'returnType> =
  { 
    OperationType: CoreOperationTypes;
    PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> InvokeResult<'props, 'returnType>;
  }

and ControlResult<'props> =
  | ControlBoth of ReactElement * Effect
  | ControlRender of ReactElement
  | ControlEffect of Effect
  // | ControlEffect of (EffectControl -> unit) list
  | ControlNextOperation of Operation<'props, unit>
  | ControlWait
and ControlData<'props> =
  { 
    OperationType: CoreOperationTypes;
    PreProcess: obj option -> obj option;
    RunOperation: CaptureReturn -> obj option -> ControlResult<'props>;
  }
and Operation<'props, 'returnType> =
  | Perform of PerformData<'props, 'returnType>
  | Control of ControlData<'props>
  | End