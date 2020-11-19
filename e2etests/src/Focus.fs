module Focus

open Browser.Dom
open Hacn.Types
open Fable.React
open Browser.Types

let Focus (ref: IRefValue<option<HTMLElement>>) = 
  Perform({ 
    OperationType = NotCore;
    PreProcess = fun _ -> None;
    GetResult = fun _ operationState -> 
      let focusLayoutEffect rerender =
        match ref.current with
        | Some(element) -> 
          let inputElement = box element :?> HTMLInputElement
          // inputElement.setSelectionRange (0, inputElement.value.Length)
          inputElement.focus ()
        | None -> failwith "Ref not set"
        None
        
      InvokeContinue(None, None, Some(focusLayoutEffect), ())
  })