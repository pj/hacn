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
  member this.CaptureValue (value: obj) = 
    match this.Capture with
    | None -> failwith "Capture must be set"
    | Some(captureFunc) -> 
      captureFunc value

let mutable captureObjects: CaptureEvent array = [||]

type prop with
  static member CreateCaptureObject = 
    let captureObj = CaptureEvent()
    captureObjects <- FSharp.Collections.Array.append captureObjects [|captureObj|]
    captureObj

  // Shortcut for simply returning a value from a click event
  static member inline captureClick value = 
    let captureObj = prop.CreateCaptureObject
    prop.onClick (fun event -> 
      captureObj.CaptureValue value)

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

let bindCapture captureResult = 
  for captureObj in captureObjects do
    captureObj.Capture <- Some(captureResult)
  captureObjects <- [||]

let Render<'returnType> (element: IReactProperty list -> ReactElement) (props: IReactProperty list) =
  Operation ({ 
    Run = 
      fun (setResult: 'returnType -> unit) _ -> 
        bindCapture setResult
        OperationWait (
          {
            Element = Some (element props)
            Effect = None
            LayoutEffect = None
            Hook = None
          }
        )
  })

let RenderContinue element (props: IReactProperty list) =
  Operation ({ 
    Run = 
      fun setResult _ -> 
        bindCapture setResult
        OperationContinue (
          {
            ReturnValue = ()
            Element = Some (element props)
            Effect = None
            LayoutEffect = None
            Hook = None
          }
        )
  })

let RenderCapture captureElement =
  Operation ({ 
    Run = fun setResult props -> 
      bindCapture setResult
      OperationWait (
        {
          Element = Some (captureElement setResult)
          Effect = None
          LayoutEffect = None
          Hook = None
        }
      )
  })
