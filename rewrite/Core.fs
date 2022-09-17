module Rewrite.Core

open Fable.React

let ConsIf a b = 
  match a with
  | Some x -> x :: b
  | None -> b

let FirstOrSecond a b =
  match a with
  | Some x -> a
  | None -> b

let bind underlyingOperation f =
  match underlyingOperation with
  | Operation (underlyingOperationData) ->
    Execution {
      Execute = fun index setNext -> 
        // Take a return value from an effect and set the next execution to a function that
        // will call the rest of the flow.
        let setResult returnValue = 
          setNext ({
              PropsNext = None
              Next = Some (fun () ->
                let nextExecution = f returnValue

                match nextExecution with
                | Execution contents -> 
                  let executionResult = contents.Execute (index+1) setNext
                  {
                    Element = None
                    Effects = executionResult.Effects
                    LayoutEffects = executionResult.LayoutEffects
                  }
                | _ -> 
                  failwith (sprintf "Can't bind execution %A" nextExecution)
              )
              Index = index
          })
            
        let result = underlyingOperationData.Run setResult
        match result with 
        | OperationWait {
            Element = element 
            Effect = effect 
            LayoutEffect = layoutEffect
          } -> 
            {
              ReturnValue = None
              Element = element
              Effects = ConsIf effect []
              LayoutEffects = ConsIf layoutEffect []
            }
        | OperationContinue {
            ReturnValue = returnValue
            Element = element 
            Effect = effect 
            LayoutEffect = layoutEffect
          } -> 
          let nextExecution = f returnValue

          match nextExecution with
          | Execution contents -> 
            let executionResult = contents.Execute (index+1) setNext
            {
              ReturnValue = executionResult.ReturnValue
              Element = FirstOrSecond executionResult.Element element
              Effects = ConsIf effect executionResult.Effects
              LayoutEffects = ConsIf layoutEffect executionResult.LayoutEffects
            }
          | End ->
            {
              ReturnValue = None
              Element = element
              Effects = ConsIf effect []
              LayoutEffects = ConsIf layoutEffect []
            }
          | _ -> 
            failwith (sprintf "Can't bind execution %A" nextExecution)
            
    }
  | Props contents ->
    Execution {
      Execute = fun index setNext -> 
        setNext ({
          Next = None
          PropsNext = Some (fun props ->
            let nextExecution = f props

            match nextExecution with
            | Execution contents -> 
              let executionResult = contents.Execute (index+1) setNext
              {
                Element = None
                Effects = executionResult.Effects
                LayoutEffects = executionResult.LayoutEffects
              }
            | _ -> 
              failwith (sprintf "Can't bind execution %A" nextExecution)

          )
          Index = index
        })
            
        {
          ReturnValue = None
          Element = element
          Effects = ConsIf effect []
          LayoutEffects = ConsIf layoutEffect []
        }
    }
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

type ExperimentState<'props> = {
  LastElement: ReactElement option
  Next: (unit -> NextResult) option
  Index: int option
  PropsNext: ('props -> NextResult) option
  Started: bool
  Hooks: (unit -> unit) list
}

let runNext (componentStateRef : IRefValue<ExperimentState<'props>>) (props: 'props) =
  match componentStateRef.current.Next with
  | Some(nextFunc) -> 
    let result = nextFunc ()
    componentStateRef.current <- {
      componentStateRef.current with 
        Next = None
        Index = None
    }
    (result.Element, result.Effects, result.LayoutEffects)
  | None -> 
    (componentStateRef.current.LastElement, [], [])

let interpreter delayOperation (props: 'props) = 
  let componentStateRef =
    Fable.React.HookBindings.Hooks.useRef (
      { 
        LastElement = None
        Next = None
        Index = None 
        PropsNext = None 
        Started = false
        Hooks = []
        }
    )

  // Force an update when an effect completes
  let state = Fable.React.HookBindings.Hooks.useState ("asdf")

  let rerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)
  
  let setNext (nextValues: NextValue<'props>) =
    match componentStateRef.current.Index with
    | Some currentIndex  when nextValues.Index > currentIndex ->
        printf "Got set next greater than current index, effect not properly disposed?"
    | _ -> 
      componentStateRef.current <- {
        componentStateRef.current with 
          Next = nextValues.Next
          PropsNext = nextValues.PropsNext
          Index = Some nextValues.Index}
      rerender ()
  
  if not componentStateRef.current.Started then
    let firstNext () = 
      match delayOperation with
      | Delay (f) -> 
        let firstOperation = f ()
        match firstOperation with
        | Execution executionContents ->
          let executionResult = executionContents.Execute 0 setNext
          {
            Element = executionResult.Element
            Effects = executionResult.Effects
            LayoutEffects = executionResult.LayoutEffects
          }
        | _ -> failwith (sprintf "Delayed operation must be execution type, got %A" firstOperation)
      | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)

    componentStateRef.current <- {
      componentStateRef.current with 
        Next = Some firstNext
        Started = true
        Index = Some 0
      }
    
  let (element, effects, layoutEffects) = runNext componentStateRef props

  let handleEffects effects () =
    let handleEffect effect =
      effect ()
    List.iter handleEffect effects
    
  Fable.React.HookBindings.Hooks.useEffect (handleEffects effects)
  Fable.React.HookBindings.Hooks.useLayoutEffect (handleEffects layoutEffects)

  Option.defaultWith (fun () -> null) element

type ReactBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f

  member _.Run(firstOperation) =
    Fable.React.FunctionComponent.Of (fun (props: 'props) -> interpreter firstOperation props)

let react = ReactBuilder()
