module Hacn
open Fable.React

type RefState<'props> =
  {
    PrevProps: 'props option;
    CurrentProps: 'props;
    Element: ReactElement option;
    AllOperations: Hacn<'props, unit> list;
  }
and InvokeResult<'props> =
  {
    NextState: RefState<'props>;
    NextOperation: Hacn<'props, unit> option;
    Element: ReactElement option;
  }
and Hacn<'props, 'returnType> =
  { 
    // PreProcess: RefState<'props> -> (IContext<'returnType> -> 'returnType) -> unit;
    PreProcess: RefState<'props> -> unit;
    Invoke: RefState<'props> -> (InvokeResult<'props> * 'returnType);
    Changed: RefState<'props> -> bool;
  }

let Props() =
  { 
    PreProcess = fun _ -> ();
    Invoke = fun refState -> (({
      NextState = refState; 
      NextOperation = None; 
      Element = None;
    }, refState.CurrentProps));
    Changed = fun (refState) -> 
      match refState.PrevProps with
        | Some(prevProps) -> prevProps <> refState.CurrentProps
        | None -> true;
  }

let Render(element) =
  { 
    PreProcess = fun _ -> ();
    Invoke = fun (refState) -> 
      (({
        NextState = refState; 
        NextOperation = None; 
        Element = Some(element);
      }, ()));
    Changed = fun (_) -> false
  }

let ContextNonPartial (useContext: IContext<'returnType> -> 'returnType) (context: IContext<'returnType>) =
  // TODO: figure out how to remove nasty mutable state.
  let mutable returnType = None
  { 
    PreProcess = fun refState -> 
      returnType <- Some(useContext(context));
    Invoke = fun refState -> 
      let returnVar = 
        match returnType with
        | Some(v) -> v
        | None -> failwith "PreProcess not called before invoking"
      ({
        NextState = refState; 
        NextOperation = None; 
        Element = None;
      }, returnVar);
    Changed = fun (refState) -> 
      match refState.PrevProps with
        | Some(prevProps) -> prevProps <> refState.CurrentProps
        | None -> true;
  }

type HacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(operation, f) =
    { 
      PreProcess = fun (refState) -> operation.PreProcess(refState);
      Invoke = 
        fun (refState) ->
          let (operationResult, returnType) = operation.Invoke(refState)
          let nextOperation = f(returnType)
          ({
            operationResult with 
              NextOperation = Some(nextOperation); 
          }, ());
      Changed =
        fun (refState) ->
          operation.Changed(refState)
    }
  member this.Zero() =
    { 
      PreProcess = fun (_) -> ();
      Invoke = fun (refState) -> 
        (({
          NextState = refState; 
          NextOperation = None; 
          Element = None;
        }, ()));
      Changed = fun (refState) -> false
    }
  member this.Delay(f) =
    f
  member this.Run(f) =
    let render props =
      let refState = useRef({
        PrevProps = None;
        CurrentProps = props;
        Element = None;
        AllOperations = [];
      })

      for op in refState.current.AllOperations do
        op.PreProcess refState.current
      
      let (currentOperation, appendNext) = 
        let changeOperation = List.tryFind (fun op -> op.Changed(refState.current)) refState.current.AllOperations
        match changeOperation with
          | Some(op) -> (op, false)
          | None -> 
            let lastOp = List.tryLast refState.current.AllOperations
            match lastOp with
              | Some(op) -> (op, true)
              | None -> 
                let firstOp = f()
                refState.current <- {
                  refState.current with AllOperations = List.append refState.current.AllOperations [firstOp]
                  }
                (firstOp, true)
      
      let (invokeResult, _) = currentOperation.Invoke(refState.current)

      let withOp = 
        match invokeResult.NextOperation with
          | Some(op) when appendNext -> 
            {invokeResult.NextState with AllOperations = List.append invokeResult.NextState.AllOperations [op]}
          | _ -> invokeResult.NextState
      refState.current <- withOp

      match refState.current.Element with
        | Some(element) -> element
        | None -> null
    
    fun props children -> 
      ofFunction 
        render
        props 
        children