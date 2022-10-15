module Hacn.Core

open Fable.React

let ConsOpt opt optList = 
  match opt with
  | Some effect -> effect :: optList
  | None -> optList

let FirstOrSecond a b =
  match a with
  | Some x -> a
  | None -> b

let bind (underlyingOperation: Builder<'a>) (f: 'a -> Builder<'b>) : Builder<'b> =
  match underlyingOperation with
  | Operation (underlyingOperationData) ->
    Execution {
      Execute = fun props -> 
        let setResult (setNext: (unit -> ExecutionStuff list) -> unit) returnValue = 
          setNext (
            fun () ->
              let nextExecution = f returnValue

              match nextExecution with
              | Execution contents -> 
                let executionResult = contents.Execute props
                executionResult.ThingsToCapture
              | _ -> 
                failwith (sprintf "Can't bind execution %A" nextExecution)
            )
        
        // let wrapHook hook props =
        //   let returnValue = hook props
        //   match returnValue with
        //   | Some (returnValue) -> 
        //     let nextExecution = f returnValue

        //     match nextExecution with
        //     | Execution contents -> 
        //       let executionResult = contents.Execute props setNext
        //       Some({
        //         Element = executionResult.Element
        //         Effects = executionResult.Effects
        //         LayoutEffects = executionResult.LayoutEffects
        //         Hooks = executionResult.Hooks
        //       })
        //     | _ -> 
        //       failwith (sprintf "Can't bind execution %A" nextExecution)
        //   | None -> None

        let result = underlyingOperationData.Run ()
        match result with 
        | OperationWait {
            Element = element 
            Effect = effect 
            LayoutEffect = layoutEffect
            Hook = hook
          } -> 
            {
              ReturnValue = None
              ThingsToCapture = (
                {
                  Element = Option.map (fun e -> (fun setNext -> e (setResult setNext))) element
                  Effect = Option.map (fun (e, d) -> ((fun setNext -> e (setResult setNext)), d)) effect
                  LayoutEffect = Option.map (fun (e, d) -> ((fun setNext -> e (setResult setNext)), d)) layoutEffect
                  Hook = None
                }
              ) :: []
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
            let executionResult = contents.Execute props
            {
              ReturnValue = executionResult.ReturnValue
              ThingsToCapture = (
                {
                  Element = Option.map (fun e -> (fun setNext -> e (setResult setNext))) element
                  Effect = Option.map (fun (e, d) -> ((fun setNext -> e (setResult setNext)), d)) effect
                  LayoutEffect = Option.map (fun (e, d) -> ((fun setNext -> e (setResult setNext)), d)) layoutEffect
                  Hook = None
                }
              ) :: executionResult.ThingsToCapture
            }
          | End ->
            {
              ReturnValue = None
              ThingsToCapture = (
                {
                  Element = Option.map (fun e -> (fun setNext -> e (setResult setNext))) element
                  Effect = Option.map (fun (e, d) -> ((fun setNext -> e (setResult setNext)), d)) effect
                  LayoutEffect = Option.map (fun (e, d) -> ((fun setNext -> e (setResult setNext)), d)) layoutEffect
                  Hook = None
                }
              ) :: []
            }
          | _ -> 
            failwith (sprintf "Can't bind execution %A" nextExecution)
    }
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

let combine firstOperation secondOperation = 
  Execution {
    Execute =
      fun () ->
        {
          ReturnValue = Some (())
          ThingsToCapture = []
        }
  }

type ExperimentState<'props> = {
  LastElement: ReactElement option
  Next: (unit -> ExecutionStuff list) option
  Started: bool
  Hooks: (unit -> ((ExecutionStuff list) option)) list
  PrevProps: 'props option
}

type Result = {
  Element: ReactElement option
  Effects: (unit -> unit) list
  LayoutEffects: (unit -> unit) list
  Disposers: Disposer list
  Hooks: obj list
}

let rec runHooks hooks props =
  match hooks with 
  | [] -> None
  | hook :: t -> 
    let result = hook props
    match result with
    | Some (result) ->
      Some (result)
    | None -> 
      runHooks t props

let rec processResults setNext started (results: ExecutionStuff list) =
  match results with
  | [] -> 
    {
      Element = None
      Effects = []
      LayoutEffects = []
      Disposers = []
      Hooks = []
    }
  | head :: tail ->
    let processedResults = processResults setNext started tail

    let setResult = setNext processedResults.Disposers

    let element = 
      if (Option.isSome processedResults.Element) then
        processedResults.Element
      else 
        match head.Element with
        | Some (element) -> Some (element setResult)
        | None -> None
    
    let effects, effectDisposers =
      match head.Effect with
      | Some((effect, disposer)) -> 
        (
          (fun () -> effect setResult) :: processedResults.Effects,
          ConsOpt disposer processedResults.Disposers
        )
      | None -> 
        (
          processedResults.Effects,
          processedResults.Disposers
        )

    let layoutEffects, disposers =
      match head.LayoutEffect with
      | Some((effect, disposer)) -> 
        (
          (fun () -> effect setResult) :: processedResults.LayoutEffects,
          ConsOpt disposer effectDisposers
        )
      | None -> 
        (
          processedResults.LayoutEffects,
          processedResults.Disposers
        )
    
    let hooks = []

    {
      Element = element
      Effects = effects
      LayoutEffects = layoutEffects
      Disposers = disposers
      Hooks = hooks
    }

let runNext setNext (componentStateRef : IRefValue<ExperimentState<'props>>) (props: 'props) =
  let foundHook = runHooks componentStateRef.current.Hooks props

  match foundHook with
  | Some (result) ->
    processResults setNext componentStateRef.current.Started result
  | None -> 
    match componentStateRef.current.Next with
    | Some(nextFunc) -> 
      processResults setNext componentStateRef.current.Started (nextFunc props)
    | None -> 
      {
        Element = None
        Effects = []
        LayoutEffects = []
        Disposers = []
        Hooks = []
      }

let getFirst delayOperation =
  match delayOperation with
  | Delay (f) ->
    let firstOperation = f ()
    match firstOperation with
    | Execution executionContents ->
      let execNext props = 
        let executionResult = executionContents.Execute ()
        executionResult.ThingsToCapture
      execNext
    | _ -> failwith (sprintf "Delayed operation must be execution type, got %A" firstOperation)
  | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)


let interpreter delayOperation props = 
  let componentStateRef =
    Fable.React.HookBindings.Hooks.useRef (
      { 
        LastElement = None
        Next = None
        Started = false
        Hooks = []
        PrevProps = None
      }
    )

  // Force an update when an effect completes
  let state = Fable.React.HookBindings.Hooks.useState ("asdf")

  let rerender () =
    let asdf = Fable.Core.JS.Math.random ()
    state.update (sprintf "blah%f" asdf)
  
  let setNext disposers (nextValues: unit -> ExecutionStuff list) =
    List.iter (fun d -> d ()) disposers
    componentStateRef.current <- {
      componentStateRef.current with 
        Next = Some (nextValues)
    }
    rerender ()
  
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
        Hooks = [] // result.Hooks
        }

  let handleEffects effects () =
    List.iter (fun e -> e ()) effects

  Fable.React.HookBindings.Hooks.useEffect (handleEffects result.Effects)
  Fable.React.HookBindings.Hooks.useLayoutEffect (handleEffects result.LayoutEffects)

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