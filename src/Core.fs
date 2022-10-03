module Hacn.Core

open Fable.React

let ConsEffect index effect effects = 
  match effect with
  | Some effect -> (index, effect) :: effects
  | None -> effects

let FirstOrSecond a b =
  match a with
  | Some x -> a
  | None -> b

let bind (underlyingOperation: Builder<'a>) (f: 'a -> Builder<'b>) : Builder<'b> =
  match underlyingOperation with
  | Operation (underlyingOperationData) ->
    Execution {
      Execute = fun index props setNext -> 
        // Take a return value from an effect and set the next execution to a function that
        // will call the rest of the flow.
        let setResult returnValue = 
          setNext ({
              Next = Some (fun props ->
                let nextExecution = f returnValue

                match nextExecution with
                | Execution contents -> 
                  let executionResult = contents.Execute (index+1) props setNext
                  {
                    Element = None
                    Effects = executionResult.Effects
                    LayoutEffects = executionResult.LayoutEffects
                    Hooks = executionResult.Hooks
                  }
                | _ -> 
                  failwith (sprintf "Can't bind execution %A" nextExecution)
              )
              Index = index
          })
        
        let wrapHook hook props =
          let returnValue = hook props
          match returnValue with
          | Some (returnValue) -> 
            let nextExecution = f returnValue

            match nextExecution with
            | Execution contents -> 
              let executionResult = contents.Execute (index+1) props setNext
              Some({
                Element = None
                Effects = executionResult.Effects
                LayoutEffects = executionResult.LayoutEffects
                Hooks = executionResult.Hooks
              })
            | _ -> 
              failwith (sprintf "Can't bind execution %A" nextExecution)
          | None -> None
            
        let result = underlyingOperationData.Run setResult props
        match result with 
        | OperationWait {
            Element = element 
            Effect = effect 
            LayoutEffect = layoutEffect
            Hook = hook
          } -> 
            {
              ReturnValue = None
              Element = element
              Effects = ConsEffect index effect []
              LayoutEffects = ConsEffect index layoutEffect []
              Hooks = ConsEffect index (Option.map wrapHook hook) []
            }
        | OperationContinue {
            ReturnValue = returnValue
            Element = element 
            Effect = effect 
            LayoutEffect = layoutEffect
            Hook = hook
          } -> 
          let nextExecution = f returnValue

          match nextExecution with
          | Execution contents -> 
            let executionResult = contents.Execute (index+1) props setNext
            {
              ReturnValue = executionResult.ReturnValue
              Element = FirstOrSecond executionResult.Element element
              Effects = ConsEffect index effect executionResult.Effects
              LayoutEffects = ConsEffect index layoutEffect executionResult.LayoutEffects
              Hooks = ConsEffect index (Option.map wrapHook hook) []
            }
          | End ->
            {
              ReturnValue = None
              Element = element
              Effects = ConsEffect index effect []
              LayoutEffects = ConsEffect index layoutEffect []
              Hooks = ConsEffect index (Option.map wrapHook hook) []
            }
          | _ -> 
            failwith (sprintf "Can't bind execution %A" nextExecution)
    }
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

let combine firstOperation secondOperation = 
  Execution {
    Execute =
      fun index setNext props ->
        {
          ReturnValue = Some (())
          Element = None
          Effects = []
          LayoutEffects = []
          Hooks = []
        }
  }

type ExperimentState<'props> = {
  LastElement: ReactElement option
  Next: GetNext option
  Index: int option
  // PropsNext: (int * GetNext) option
  Started: bool
  Hooks: (int * GetNextHook) list
  PrevProps: 'props option
  Disposers: (int * Disposer) list
  LayoutDisposers: (int * Disposer) list
}

let disposeFrom index disposers=
  let foldDisposer disposers indexedDisposer =
    let (disposerIndex, disposer) = indexedDisposer
    if disposerIndex > index then 
      disposer ()
      disposers
    else
      indexedDisposer :: disposers
    
  List.fold 
    foldDisposer
    []
    disposers

let rec runHooks hooks props =
  match hooks with 
  | [] -> None
  | (index, hook) :: t -> 
    let result = hook props
    match result with
    | Some (result) ->
      Some (index, result)
    | None -> 
      runHooks t props

let runNext (componentStateRef : IRefValue<ExperimentState<'props>>) (props: 'props) =
  // Run hooks and see if there are any results we should trigger off
    
  let foundHook = runHooks componentStateRef.current.Hooks props

  match foundHook with
  | Some (index, result) ->
    componentStateRef.current <- {
      componentStateRef.current with 
        Next = None
        Index = None
        Disposers = (disposeFrom index componentStateRef.current.Disposers)
    }
    Some result
  | None -> 
    match componentStateRef.current.Next with
    | Some(nextFunc) -> 
      let result = nextFunc props 
      componentStateRef.current <- {
        componentStateRef.current with 
          Next = None
          Index = None
      }
      Some result
    | None -> 
      None

let getFirst delayOperation setNext =
  match delayOperation with
  | Delay (f) ->
    let firstOperation = f ()
    match firstOperation with
    | Execution executionContents ->
      let execNext props = 
        let executionResult = executionContents.Execute 0 props setNext
        {
          Element = executionResult.Element
          Effects = executionResult.Effects
          LayoutEffects = executionResult.LayoutEffects
          Hooks = executionResult.Hooks
          // PropsNext = executionResult.PropsNext
        }
      execNext
    | _ -> failwith (sprintf "Delayed operation must be execution type, got %A" firstOperation)
  | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)

let rec processDisposers existingDisposers newDisposers =
  match (existingDisposers, newDisposers) with
  | ([], []) -> []
  | ([], newDisposers) -> newDisposers
  | (existingDisposers, []) ->
    List.iter (fun (_, disposer) -> disposer ()) existingDisposers
    []
  | ((edIdx, ed) :: existingDisposersTail, (ndIdx, nd) :: newDisposersTail) ->
    if edIdx < ndIdx then
      (edIdx, ed) :: processDisposers existingDisposersTail newDisposers
    else 
      ed ()
      (ndIdx, nd) :: processDisposers existingDisposersTail newDisposersTail
    
let processEffects existingDisposers effects =
  let handleEffect effects (index, effect) =
    match effect () with
    | Some disposer -> (index, disposer) :: effects
    | None -> effects

  let disposers = List.fold handleEffect [] effects
  
  processDisposers existingDisposers disposers

let interpreter delayOperation props = 
  let componentStateRef =
    Fable.React.HookBindings.Hooks.useRef (
      { 
        LastElement = None
        Next = None
        Index = None 
        // PropsNext = None 
        Started = false
        Hooks = []
        PrevProps = None
        Disposers = []
        LayoutDisposers = []
        }
    )

  // Force an update when an effect completes
  let state = Fable.React.HookBindings.Hooks.useState ("asdf")

  let rerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)
  
  let setNext (nextValues: NextValue) =
    match componentStateRef.current.Index with
    | Some currentIndex  when nextValues.Index > currentIndex ->
        printf "Got set next greater than current index, effect not properly disposed?"
    | _ ->
      componentStateRef.current <- {
        componentStateRef.current with 
          Next = nextValues.Next
          Index = Some nextValues.Index
          Disposers = disposeFrom nextValues.Index componentStateRef.current.Disposers}
      rerender ()
  
  if not componentStateRef.current.Started then
    componentStateRef.current <- {
      componentStateRef.current with 
        Next = Some (getFirst delayOperation setNext)
        }

  let result = 
      runNext componentStateRef props

  match result with
  | Some (result) -> 
    if not componentStateRef.current.Started then
      componentStateRef.current <- {
        componentStateRef.current with 
          Started = true
          // PropsNext = result.PropsNext
          Hooks = result.Hooks
          }
    let handleEffects effects () =
      componentStateRef.current <- {
        componentStateRef.current with 
          Disposers = processEffects componentStateRef.current.Disposers effects
      }

    let handleLayoutEffects effects () =
      componentStateRef.current <- {
        componentStateRef.current with 
          LayoutDisposers = processEffects componentStateRef.current.LayoutDisposers effects
      }
      
    Fable.React.HookBindings.Hooks.useEffect (handleEffects result.Effects)
    Fable.React.HookBindings.Hooks.useLayoutEffect (handleLayoutEffects result.LayoutEffects)

    Option.defaultValue null result.Element
  | None -> 
    if not componentStateRef.current.Started then
      failwith "component not started and result empty"
    null

type ReactBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2

  member _.Run(firstOperation) =
    Fable.React.FunctionComponent.Of (fun props -> interpreter firstOperation props)

let react = ReactBuilder()

type HacnBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2
  member _.Return (value) = Return (value)

  // member _.Run(firstOperation) = Fable.React.FunctionComponent.Of (fun props -> interpreter firstOperation props)

let hacn = HacnBuilder()