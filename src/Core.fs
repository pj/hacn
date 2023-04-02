module Hacn.Core

open Fable.React

let bindSetNext<'a, 'b> f (setNext: SetNext<'b>) (returnValue: 'a) : unit= 
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

let bind (underlyingOperation: Builder<'a>) (f: 'a -> Builder<'b>) : Builder<'b> =
  match underlyingOperation with
  | Operation underlyingOperationData ->
    Execution {
      Execute = fun props -> 
        
        // let wrapHook hook = 
        //   fun props ->
        //     let returnValue = hook props
        //     match returnValue with
        //     | Some (returnValue) -> 
        //       let nextExecution = f returnValue

        //       match nextExecution with
        //       | Execution contents -> 
        //         let executionResult = contents.Execute props
        //         Some (executionResult.ThingsToCapture)
        //       | _ -> 
        //         failwith (sprintf "Can't bind execution %A" nextExecution)
        //     | None -> None

        let result = underlyingOperationData props
        match result with 
        | OperationWait sideEffects ->
          {
            ReturnValue = None
            OperationsToBind = (
              fun setNext ->  
                let setResult = bindSetNext f setNext
                sideEffects setResult
            ) :: []
          }
        | OperationContinue (sideEffects, returnValue) ->
          let nextExecution = f returnValue

          match nextExecution with
          | Execution contents -> 
            let executionResult = contents.Execute props
            {
              ReturnValue = executionResult.ReturnValue
              OperationsToBind = (
                fun setNext ->  
                  let setResult = bindSetNext setNext
                  sideEffects setResult
              ) :: []
            }
          | End ->
            {
              ReturnValue = None
              OperationsToBind = (
                fun setNext ->  
                  let setResult = bindSetNext setNext
                  sideEffects setResult
              ) :: []
            }
          | _ -> 
            failwith (sprintf "Can't bind execution %A" nextExecution)
    }
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

let combine firstOperation secondOperation = 
  Execution {
    Execute =
      fun props ->
        {
          ReturnValue = Some (())
          OperationsToBind = []
        }
  }

type ExperimentState<'returnType> = {
  LastElement: ReactElement option
  Next: (obj -> NextResult<'returnType>) option
  Started: bool
  // Hooks: (obj -> ((ExecutionStuff list) option)) list
  PrevProps: obj option
  Disposers: array<Disposer option>
  LayoutDisposers: array<Disposer option>
}

type Result = {
  Element: ReactElement option
  Effects: (unit -> Disposer option) list
  LayoutEffects: (unit -> Disposer option) list
  // Hooks: GetNext list
}

// let rec runHooks hooks props =
//   match hooks with 
//   | [] -> None
//   | hook :: t -> 
//     let result = hook props
//     let hooksResult = runHooks t props
//     Option.orElse hooksResult result

let rec processResults<'returnType> (disposerIndex: int) (setNext: int -> (obj -> NextResult<'returnType>) -> unit) (started) (results: (SetNext<'returnType> -> OperationSideEffects<'returnType>) list) =
  match results with
  | [] -> 
    {
      Element = None
      Effects = []
      LayoutEffects = []
      // Hooks = []
    }
  | head :: tail ->
    let processedResults = processResults (disposerIndex + 1) setNext started tail

    let boundSetNext = setNext disposerIndex

    let sideEffects = head boundSetNext

    let element = 
      if (Option.isSome sideEffects.Element) then
        sideEffects.Element
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
    
    // let hooks = 
    //   if not started then
    //     ConsOpt head.Hook processedResults.Hooks
    //   else 
    //     []

    {
      Element = element
      Effects = effects
      // Disposers = ref (Some({Disposer = ref None; Next = processedResults.Disposers}))
      LayoutEffects = layoutEffects
      // Disposers = disposers
      // Hooks = hooks

    }

let runNext<'returnType> (setNext: int -> (obj -> NextResult<'returnType>) -> unit) (componentStateRef : IRefValue<ExperimentState<'returnType>>) (props: obj) =
  // let foundHook = runHooks componentStateRef.current.Hooks props

  // match foundHook with
  // | Some (result) ->
  //   processResults setNext componentStateRef.current.Started result
  // | None -> 
    match componentStateRef.current.Next with
    | Some(nextFunc) -> 
      let results = nextFunc props
      processResults (Array.length componentStateRef.current.Disposers+1) setNext componentStateRef.current.Started results.OperationsToBind
    | None -> 
      {
        Element = None
        Effects = []
        // Disposers = ref None
        LayoutEffects = []
        // Disposers = []
        // Hooks = []
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

let interpreter delayOperation (props: obj )= 
  let componentStateRef =
    Fable.React.HookBindings.Hooks.useRef (
      { 
        LastElement = None
        Next = None
        Started = false
        // Hooks = []
        PrevProps = None
        Disposers = [||]
        LayoutDisposers = [||]
      }
    )

  // Force an update when an effect completes
  let state = Fable.React.HookBindings.Hooks.useState ("asdf")

  let setNext disposerIndex nextValues =
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

    // Trigger rerender
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)
  
  if not componentStateRef.current.Started then
    componentStateRef.current <- {
      componentStateRef.current with 
        Next = Some (getFirst delayOperation)
    }

  let result = runNext setNext componentStateRef props

  componentStateRef.current <- {
    componentStateRef.current with 
      Next = None
  }

  if not componentStateRef.current.Started then
    componentStateRef.current <- {
      componentStateRef.current with 
        Started = true
        // Hooks = result.Hooks
        }

  let handleEffects effects isLayout () =
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

  Fable.React.HookBindings.Hooks.useEffect (handleEffects result.Effects false)
  Fable.React.HookBindings.Hooks.useLayoutEffect (handleEffects result.LayoutEffects true)

  Option.defaultValue null result.Element

type ReactBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2

  member _.Run(firstOperation) =
    // Fable.React.FunctionComponent.Of (fun props -> interpreter firstOperation props)
    Feliz.React.functionComponent (fun props -> interpreter firstOperation props)

let react = ReactBuilder()

type HacnBuilder () =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f
  member _.Combine (f1, f2) = combine f1 f2
  member _.Return (value) = Return (value)

  // member _.Run(firstOperation) = Fable.React.FunctionComponent.Of (fun props -> interpreter firstOperation props)

let hacn = HacnBuilder()