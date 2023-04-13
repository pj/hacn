module Hacn.Core

open Fable.React

let bindSetNext f setNext returnValue= 
  setNext (
    fun props ->
      let nextExecution = f returnValue

      match nextExecution with
      | Execution contents -> 
        let executionResult = contents.Execute props
        {
          OperationsToBind = executionResult.OperationsToBind
        }
      | _ -> 
        failwith (sprintf "Can't bind execution %A" nextExecution)
    )
let wrapHook f hook = 
  fun props ->
    let returnValue = hook props
    match returnValue with
    | Some (returnValue) -> 
      let nextExecution = f returnValue

      match nextExecution with
      | Execution contents -> 
        let executionResult = contents.Execute props
        Some (executionResult)
      | _ -> 
        failwith (sprintf "Can't bind execution %A" nextExecution)
    | None -> None


let bind<'a, 'b> (underlyingOperation: Builder<'a>) (f: 'a -> Builder<'b>) : Builder<'b> =
  match underlyingOperation with
  | Operation underlyingOperationData ->
    Execution {
      Execute = fun props -> 
        
        let result = underlyingOperationData props
        match result with 
        | OperationWait sideEffects ->
          {
            OperationsToBind = (
              fun setNext ->  
                let setResult = bindSetNext f setNext
                let operationSideEffects = sideEffects setResult
                {
                  Element = operationSideEffects.Element
                  Effect = operationSideEffects.Effect
                  LayoutEffect = operationSideEffects.LayoutEffect
                  Hook = Option.map (wrapHook f) operationSideEffects.Hook
                }
            ) :: []
          }
        | OperationContinue (sideEffects, returnValue) ->
          let nextExecution = f returnValue

          match nextExecution with
          | Execution contents -> 
            let executionResult = contents.Execute props
            {
              OperationsToBind = (
                fun setNext ->  
                  let setResult = bindSetNext f setNext
                  let operationSideEffects = sideEffects setResult
                  {
                    Element = operationSideEffects.Element
                    Effect = operationSideEffects.Effect
                    LayoutEffect = operationSideEffects.LayoutEffect
                    Hook = Option.map (wrapHook f) operationSideEffects.Hook
                  }
              ) :: executionResult.OperationsToBind
            }
          | End ->
            {
              OperationsToBind = (
                fun setNext ->  
                  let setResult = bindSetNext f setNext
                  let operationSideEffects = sideEffects setResult
                  {
                    Element = operationSideEffects.Element
                    Effect = operationSideEffects.Effect
                    LayoutEffect = operationSideEffects.LayoutEffect
                    Hook = Option.map (wrapHook f) operationSideEffects.Hook
                  }
              ) :: []
            }
          | _ -> 
            failwith (sprintf "Can't bind execution %A" nextExecution)
    }
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

let combine firstOperation secondOperation = 
  match firstOperation with
  | Execution executionContents ->
    Execution {
      Execute = fun props -> 
        let combineSetNext secondOperation setNext returnValue= 
          setNext (
            fun props ->
              let nextExecution = secondOperation ()

              match nextExecution with
              | Execution contents -> 
                let executionResult = contents.Execute props
                {
                  OperationsToBind = executionResult.OperationsToBind
                }
              | _ -> 
                failwith (sprintf "Can't bind execution %A" nextExecution)
            )

        let firstExecution = executionContents.Execute props
        {
          OperationsToBind = (
            fun setNext ->  
              let setResult = combineSetNext secondOperation setNext
              let operationSideEffects = List.map firstExecution.OperationsToBind setResult
              {
                Element = operationSideEffects.Element
                Effect = operationSideEffects.Effect
                LayoutEffect = operationSideEffects.LayoutEffect
                Hook = Option.map (wrapHook f) operationSideEffects.Hook
              }
          ) :: []
        }
    }
        
  | _ -> failwith (sprintf "Can't combine operation %A" firstOperation)

let tryWith delayOperation f = 
  End


type ExperimentState = {
  LastElement: ReactElement
  Next: (obj -> ExecutionResult) option
  Started: bool
  Hooks: (obj -> ExecutionResult option) list
  PrevProps: obj option
  Disposers: array<Disposer option>
  LayoutDisposers: array<Disposer option>
}

type RunResult = {
  Element: ReactElement
  Effects: (unit -> Disposer option) list
  LayoutEffects: (unit -> Disposer option) list
  Hooks: (obj -> ExecutionResult option) list
}

type Result = {
  Element: ReactElement option
  Effects: (unit -> Disposer option) list
  LayoutEffects: (unit -> Disposer option) list
  Hooks: (obj -> ExecutionResult option) list
}

let rec runHooks hooks props =
  match hooks with 
  | [] -> None
  | hook :: t -> 
    let result = hook props
    let hooksResult = runHooks t props
    Option.orElse hooksResult result

let rec processResults disposerIndex setNext started (results: (SetNext -> ExecutionSideEffects) list) =
  match results with
  | [] -> 
    {
      Element = None
      Effects = []
      LayoutEffects = []
      Hooks = []
    }
  | head :: tail ->
    let processedResults = processResults (disposerIndex + 1) setNext started tail

    let boundSetNext = setNext disposerIndex

    let sideEffects = head boundSetNext

    let element = 
      if (Option.isSome processedResults.Element) then
        processedResults.Element
      else 
        match sideEffects.Element with
        | Some (element) -> Some element
        | None -> None
    
    let effects =
      match sideEffects.Effect with
      | Some(effect) -> 
        effect :: processedResults.Effects
      | None -> processedResults.Effects

    let layoutEffects =
      match sideEffects.LayoutEffect with
      | Some(effect) -> 
        effect :: processedResults.LayoutEffects
      | None -> processedResults.LayoutEffects
    
    let hooks = 
      if not started then
        match sideEffects.Hook with
        | None -> processedResults.Hooks
        | Some (hook) -> hook :: processedResults.Hooks
      else 
        []

    {
      Element = element
      Effects = effects
      LayoutEffects = layoutEffects
      Hooks = hooks
    }

let getFirst delayOperation =
  match delayOperation with
  | Delay (f) ->
    let firstOperation = f ()
    match firstOperation with
    | Execution executionContents ->
      let execNext props = 
        let executionResult = executionContents.Execute props
        {
          OperationsToBind = executionResult.OperationsToBind
        }
      execNext
    | _ -> failwith (sprintf "Delayed operation must be execution type, got %A" firstOperation)
  | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)


let runNext setNext (componentStateRef : IRefValue<ExperimentState>) delayOperation props : RunResult =
  if not componentStateRef.current.Started then
    componentStateRef.current <- {
      componentStateRef.current with 
        Next = Some (getFirst delayOperation)
    }

  let foundHook = runHooks componentStateRef.current.Hooks props

  let result = 
    match foundHook with
    | Some (result) ->
      processResults (Array.length componentStateRef.current.Disposers+1) setNext componentStateRef.current.Started result.OperationsToBind
    | None -> 
      match componentStateRef.current.Next with
      | Some(nextFunc) -> 
        let results = nextFunc props
        processResults (Array.length componentStateRef.current.Disposers+1) setNext componentStateRef.current.Started results.OperationsToBind
      | None -> 
        {
          Element = None
          Effects = []
          LayoutEffects = []
          Hooks = []
        }

  let resultElement = Option.defaultValue componentStateRef.current.LastElement result.Element 
  componentStateRef.current <- {
    componentStateRef.current with 
      Next = None
      LastElement = resultElement
  }

  if not componentStateRef.current.Started then
    componentStateRef.current <- {
      componentStateRef.current with 
        Started = true
        Hooks = result.Hooks
        }

  {
      Element = resultElement
      Effects = result.Effects
      LayoutEffects = result.LayoutEffects
      Hooks = result.Hooks
  }

let setNext (componentStateRef: IRefValue<ExperimentState>) triggerUpdate disposerIndex nextValues =
  let disposerLength = Array.length componentStateRef.current.Disposers
  let (updatedDisposers, updatedLayoutDisposers) =
    if disposerIndex < disposerLength then
      for i in disposerIndex .. (Array.length componentStateRef.current.Disposers) do
        Option.iter (fun d -> d ()) componentStateRef.current.Disposers.[i]
        Option.iter (fun d -> d ()) componentStateRef.current.LayoutDisposers.[i]
      (
        componentStateRef.current.Disposers[disposerIndex..], 
        componentStateRef.current.LayoutDisposers[disposerIndex..]
      )
    else
      (
        componentStateRef.current.Disposers,
        componentStateRef.current.LayoutDisposers
      )

  componentStateRef.current <- {
    componentStateRef.current with 
      Next = Some (nextValues)
      Disposers = updatedDisposers
      LayoutDisposers = updatedLayoutDisposers
  }

  triggerUpdate ()

let handleEffects (componentStateRef: IRefValue<ExperimentState>) effects isLayout () =
  let newDisposers = List.map (fun e -> e ()) effects
  if isLayout then
    componentStateRef.current <- {
      componentStateRef.current with 
          LayoutDisposers = Array.append componentStateRef.current.LayoutDisposers (List.toArray newDisposers)
        }
  else
    componentStateRef.current <- {
      componentStateRef.current with 
          Disposers = Array.append componentStateRef.current.Disposers (List.toArray newDisposers)
        }

let interpreter delayOperation (props: obj )= 
  let componentStateRef =
    Fable.React.HookBindings.Hooks.useRef (
      { 
        LastElement = null
        Next = None
        Started = false
        Hooks = []
        PrevProps = None
        Disposers = [||]
        LayoutDisposers = [||]
      }
    )

  // Force an update when an effect completes
  let state = Fable.React.HookBindings.Hooks.useState ("asdf")
  // Trigger rerender
  let triggerRerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)
  
  let result = runNext (setNext componentStateRef triggerRerender) componentStateRef delayOperation props

  Fable.React.HookBindings.Hooks.useEffect (handleEffects componentStateRef result.Effects false)
  Fable.React.HookBindings.Hooks.useLayoutEffect (handleEffects componentStateRef result.LayoutEffects true)

  result.Element

type ReactBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  // member _.Combine (f1, f2) = combine f1 f2
  member _.TryWith (operation, f) = tryWith operation f

  member _.Run(firstOperation) =
    // Fable.React.FunctionComponent.Of (fun props -> interpreter firstOperation props)
    // Feliz.React.functionComponent (fun props -> interpreter firstOperation props)
    Fable.React.Helpers.ofFunction (fun props -> interpreter firstOperation props)

let react = ReactBuilder()

type HacnBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2
  member _.Return (value) = Return (value)
  member _.TryWith (operation, f) = tryWith operation f

  // member _.Run(firstOperation) = Fable.React.FunctionComponent.Of (fun props -> interpreter firstOperation props)

let hacn = HacnBuilder()