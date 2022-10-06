module Hacn.Render

open Fable.React
// open Utils
open Feliz
open Fable.Core.JsInterop
open FSharp.Core

let Render<'returnType> (builder: Ref<('returnType -> unit) option> -> ReactElement) =
  Operation ({ 
    Run = 
      fun (setResult: 'returnType -> unit) _ -> 
        OperationWait (
          {
            Element = Some (builder (ref (Some setResult)))
            Effect = None
            LayoutEffect = None
            Hook = None
          }
        )
  })

// let RenderContinue element (props: IReactProperty list) =
//   Operation ({ 
//     Run = 
//       fun setResult _ -> 
//         bindCapture setResult
//         OperationContinue (
//           {
//             ReturnValue = ()
//             Element = Some (element props)
//             Effect = None
//             LayoutEffect = None
//             Hook = None
//           }
//         )
//   })

let RenderCapture captureElement =
  Operation ({ 
    Run = fun setResult props -> 
      OperationWait (
        {
          Element = Some (captureElement setResult)
          Effect = None
          LayoutEffect = None
          Hook = None
        }
      )
  })