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

type CaptureEvent () = 
  member val Capture = None with get, set
  member this.CaptureValue value = 
    match this.Capture with
    | None -> failwith "Capture must be set"
    | Some(captureFunc) -> captureFunc (fun _ -> (Replace(value :> obj)))

let mutable captureObjects: CaptureEvent array = [||]

type prop with
  static member CreateCaptureObject = 
    let captureObj = CaptureEvent()
    captureObjects <- FSharp.Collections.Array.append captureObjects [|captureObj|]
    captureObj

  // Shortcut for simply returning a value from a click event
  static member inline captureClick value = 
    let captureObj = prop.CreateCaptureObject
    prop.onClick (fun event -> captureObj.CaptureValue value)

  static member inline captureClickEvent (func : MouseEvent -> 'a) = 
    let captureObj = prop.CreateCaptureObject
    prop.onClick (fun event -> captureObj.CaptureValue (func event))

  static member inline captureCheckChange = 
    let captureObj = prop.CreateCaptureObject
    prop.onCheckedChange (fun (value: bool) -> captureObj.CaptureValue value)

  static member inline captureValueChange = 
    let captureObj = prop.CreateCaptureObject
    prop.onChange (fun (value: string) -> captureObj.CaptureValue value)

  static member inline captureChange (func : Event -> 'a)= 
    let captureObj = prop.CreateCaptureObject
    prop.onChange (fun (value: Event) -> captureObj.CaptureValue (func value))

  static member inline captureKeyDown = 
    let captureObj = prop.CreateCaptureObject
    prop.onKeyDown (fun keyEvent -> captureObj.CaptureValue (keyEvent.key))

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

let bindCapture captureResult = 
  for captureObj in captureObjects do
    captureObj.Capture <- Some(captureResult)
  captureObjects <- [||]

let Render (element: IReactProperty list -> ReactElement) (props: IReactProperty list) =
  Perform({ 
    PreProcess = fun _ -> None;
    GetResult = fun captureResult operationState -> 
      let eraseCapturedResult _ =
        Some(fun _ -> Erase)
      bindCapture captureResult
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
      bindCapture captureResult
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
      bindCapture captureResult
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
