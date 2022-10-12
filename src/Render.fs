module Hacn.Render

open Feliz
open FSharp.Core

let Render<'returnType> (builder: ('returnType -> unit) -> ReactElement) =
  Operation ({ 
    Run = 
      fun (setResult: 'returnType -> unit) _ -> 
        let element = builder setResult
        OperationWait (
          {
            Element = Some (element)
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