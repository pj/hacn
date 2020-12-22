module Hacn.Render

open Fable.React
// open Utils
open Feliz
open Fable.Core.JsInterop
open FSharp.Core
open Fable.Core.JS
open Fable.Core.Util
open Fable.Core
open Fable.Core.JsInterop
open Hacn.Core
open Browser.Types

type prop with
  static member withCurrentCapture f = 
    match implicitCapture with 
    | Some(capture) -> f (fun v -> capture (fun _ -> Replace(v :> obj)))
    | None -> failwith "No current capture"

  // Shortcut for simply returning a value from a click event
  static member inline captureClick value = 
    prop.withCurrentCapture (fun capture -> 
      prop.onClick (fun event -> capture value)
    )

  static member inline captureClickEvent (func : MouseEvent -> 'a) = 
    prop.withCurrentCapture (fun capture -> 
      prop.onClick (fun event -> capture (func event))
    )

  static member inline captureCheckChange = 
    prop.withCurrentCapture prop.onCheckedChange

  static member inline captureValueChange = 
    prop.withCurrentCapture (fun capture ->
      prop.onChange (fun (value: string) -> capture value)
    )

  static member inline captureChange (func : Event -> 'a)= 
    prop.withCurrentCapture (fun capture ->
      prop.onChange (fun (value: Event) -> capture (func value))
    )

  static member inline captureKeyDown = 
    prop.withCurrentCapture (fun capture ->
      prop.onKeyDown (fun keyEvent -> capture (keyEvent.key))
    )

// let Render (element: IReactProperty list -> ReactElement) (props: IReactProperty list) =
//   Perform({ 
//     PreProcess = fun _ -> None;
//     GetResult = fun _ __ -> 
//       PerformWait(
//         {
//           Element = Some(element props)
//           Effect = None;
//           LayoutEffect = None
//           OperationState = None
//         }
//       )
//   })

let Render (element: IReactProperty list -> ReactElement) (props: IReactProperty list) =
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun _ operationState -> 
      let eraseCapturedResult _ =
        Some(fun _ -> Erase)
      match operationState with
      | Some(result) -> 
        let castReturn: 'returnType = unbox result
        PerformContinue(
          {
            Element = Some(element props)
            Effect = Some(eraseCapturedResult)
            LayoutEffect = None
            OperationState = Keep
          }, 
          castReturn
        )
      | _ ->
        PerformWait(
          {
            Element = Some(element props)
            Effect = None
            LayoutEffect = None
            OperationState = Keep
          }
        )
  })

let RenderContinue element (props: IReactProperty list) =
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      let renderedElement = element props
      PerformContinue(
        {
          Element = Some(renderedElement)
          Effect = None
          LayoutEffect = None
          OperationState = Keep
        }, 
        ()
      )
  })

let RenderCapture<'returnType> captureElement =
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      let captureResultInternal v =
        captureResult (fun _ -> Replace(v))
      let eraseCapturedResult _ =
        Some(fun _ -> Erase)
      match operationState with
      | Some(result) -> 
        let castReturn: 'returnType = unbox result
        PerformContinue(
          {
            Element = Some(captureElement captureResultInternal)
            Effect = Some(eraseCapturedResult)
            LayoutEffect = None
            OperationState = Keep
          }, 
          castReturn
        )
      | _ ->
        PerformWait(
          {
            Element = Some(captureElement captureResultInternal)
            Effect = None
            LayoutEffect = None
            OperationState = Keep
          }
        )
  })
