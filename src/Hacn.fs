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
    Invoke: RefState<'props> -> (InvokeResult<'props> * 'returnType);
    Changed: RefState<'props> -> bool;
  }

let Props() =
  { 
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
    Invoke = fun (refState) -> 
      (({
        NextState = refState; 
        NextOperation = None; 
        Element = Some(element);
      }, ()));
    Changed = fun (_) -> false
  }

type HacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(operation, f) =
    { 
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
    let render (props: 'props) =
      let refState: IRefValue<RefState<'props>> = useRef({
        PrevProps = None;
        CurrentProps = props;
        Element = None;
        AllOperations = [];
      })
      
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
    
    fun (props: 'props) children -> 
      ofFunction 
        render
        props 
        children