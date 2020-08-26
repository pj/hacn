[<AutoOpen>]
module Hacn.Types

open Fable.React

type CoreOperationTypes =
  | PropsOperation
  | StateGet
  | StateSet
  | NotCore

type EffectControl =
  {
    GetOperationState: unit -> obj option;
    Rerender: obj option -> unit;
  }

type EffectFunction = EffectControl -> unit
type EffectControlWrapper = EffectFunction -> unit

type ContinuedEffect = EffectControlWrapper

type InvokeResult<'props, 'returnType> =
  | InvokeRender of ReactElement * (EffectControl -> unit) list
  | InvokeEffect of (EffectControl -> unit) list
  | InvokeReturn of 'returnType
  | InvokeWait
and PerformData<'props, 'returnType> =
  { 
    OperationType: CoreOperationTypes;
    PreProcess: obj option -> obj option;
    GetResult: obj option -> InvokeResult<'props, 'returnType>;
  }

and ControlResult<'props> =
  | ControlRender of ReactElement * (EffectControl -> unit) list
  | ControlEffect of (EffectControl -> unit) list
  | ControlNextOperation of Operation<'props, unit>
  | ControlWait
and ControlData<'props> =
  { 
    OperationType: CoreOperationTypes;
    PreProcess: obj option -> obj option;
    RunOperation: EffectControlWrapper -> obj option -> ControlResult<'props>;
  }
and Operation<'props, 'returnType> =
  | Perform of PerformData<'props, 'returnType>
  | Control of ControlData<'props>
  | End