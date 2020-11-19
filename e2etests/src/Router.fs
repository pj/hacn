module Router

open Browser.Dom
open Hacn.Types

let HashRouter () = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun _ operationState -> 
      match operationState with
      | Some(value) -> 
        let castVal: string = unbox value
        InvokeContinue(None, None, None, castVal)
      | None -> 
        let hashChangeEffect rerender =
          let hashChangeListener (event: Browser.Types.Event) =
            let hce: Browser.Types.HashChangeEvent = unbox event
            rerender (fun _ -> Some(hce.newURL :> obj))

          window.addEventListener("hashchange", hashChangeListener, false)

          Some(fun _ -> 
            window.removeEventListener("hashchange", hashChangeListener, false)
            None
          )
          
        InvokeContinue(None, Some(hashChangeEffect), None, "")
  })