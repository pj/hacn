module Rewrite.Operations

open Fable

let Render element =
  Operation (
    {
      Run =
        fun setResult ->
          OperationWait (
            { Element = Some element
              Effect = None
              LayoutEffect = None
              }
          ) 
    }
  )

let RenderContinue element =
  Operation (
    {
      Run =
        fun setResult ->
          OperationContinue (
            { ReturnValue = ()
              Element = Some element
              Effect = None
              LayoutEffect = None
              }
          ) 
    }
  )

let Timeout time =
  Operation (
    {
      Run =
        fun setResult ->
          let timeoutEffect () =
            let timeoutCallback () =
              setResult ()

            let timeout = Fable.Core.JS.setTimeout timeoutCallback time
            Some (fun () -> 
              Fable.Core.JS.clearTimeout timeout
            )

          OperationWait (
            { Element = None
              Effect = Some(timeoutEffect)
              LayoutEffect = None
              }
          ) 
    }
  )
