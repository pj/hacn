[<AutoOpen>]
module Hacn.Types

open Fable.React

type ReRender = (obj option -> obj option) -> unit
type Dispose = (obj option -> obj option) option
type Effect = ReRender -> Dispose

type CaptureReturn = (obj option -> unit)

type OperationData =
  {
    Element: ReactElement option
    Effect:  Effect option 
    LayoutEffect: Effect option
    // Useful for memoize/state control operations, where we don't want to 
    // trigger a rerender. Extra option layer to indicate whether to update 
    // state or not.
    OperationState: (obj option) option
  }
and PerformResult<'props, 'returnType when 'props: equality> =
  | PerformWait of OperationData
  | PerformContinue of OperationData * 'returnType
and PerformData<'props, 'returnType when 'props: equality> =
  { 
    PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> PerformResult<'props, 'returnType>;
  }
and ControlResult<'props when 'props: equality> =
  | ControlWait of OperationData
  | ControlNext of OperationData * Operation<'props, unit>
and ControlData<'props when 'props: equality> =
  { 
    PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> ControlResult<'props>;
  }
and PerformPropsData<'props> =
  { 
    Changed: 'props option -> 'props -> bool
  }
and ControlPropsData<'props when 'props: equality> =
  { 
    Changed: 'props option -> 'props -> bool
    Execute: 'props -> Operation<'props, unit>
  }

and Operation<'props, 'returnType when 'props: equality> =
  | Perform of PerformData<'props, 'returnType>
  | PerformProps of PerformPropsData<'props>
  | Control of ControlData<'props>
  | ControlProps of ControlPropsData<'props>
  | End