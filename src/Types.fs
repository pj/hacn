[<AutoOpen>]
module Hacn.Types

open Fable.React
open System

type [<AllowNullLiteral>] InfoComponentObject =
  abstract componentStack: string with get

type ExceptionDetails = {
  Exception: exn
  InfoComponent: InfoComponentObject option
}

type UpdateState =
| Keep
| Erase
| Replace of obj
| SetException of ExceptionDetails

type StateUpdater = (obj option -> UpdateState)

type Dispose = StateUpdater option
type Effect = unit -> Dispose

type CaptureReturn = (StateUpdater -> unit)

type OperationData =
  {
    // To capture render errors we need the captureFunc used for the rendered 
    // element. This makes it easy to ensure that we can update the error state
    // of the exact rendered element.
    Element: (ReactElement * CaptureReturn) option
    Effect:  Effect option 
    LayoutEffect: Effect option
    // Useful for memoize/state control operations, where we don't want to 
    // trigger a rerender. 
    OperationState: UpdateState
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
    GetResult: CaptureReturn -> obj option -> 'props -> ControlResult<'props>;
  }
and ComposeSideEffects<'props when 'props: equality> =
  {
    Effects: (int * Effect) list
    LayoutEffects: (int * Effect) list
    Element: (ReactElement * CaptureReturn) option
    OperationState: (obj option) option
  }
and ExceptionData<'props when 'props: equality> =
  { 
    // PreProcess: obj option -> obj option;
    GetResult: CaptureReturn -> obj option -> 'props -> ControlResult<'props>;
  }
// and ComposeResult<'props when 'props: equality> =
//   | ComposeWait of ComposeSideEffects<'props>
//   | ComposeFinished of ComposeSideEffects<'props> * Operation<'props, unit>
// and ComposeData<'props when 'props: equality> =
//   { 
//     PreProcess: obj option -> obj option;
//     ExecuteCompose: CaptureReturn -> obj option -> 'props -> ControlResult<'props>;
//   }
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
  // For composition
  | ComposeStart of (unit -> Operation<'props, unit>)
  | ComposeReturn of 'returnType
  | Compose of ControlData<'props>

  // Delay
  | Delay of (unit -> Operation<'props, unit>)

  // Exception
  | TryWith of ExceptionData<'props>
  | End