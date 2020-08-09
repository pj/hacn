module Hacn
open Fable.React
open Hacn.Types

type RefState<'props> =
  {
    PrevProps: 'props option;
    CurrentProps: 'props;
    Element: ReactElement option;
    NextOperation: (string * Operation<'props, unit>) option;
    PreProcessOperations: Operation<'props, unit> list;
    PropsOperation: Operation<'props, unit> option;
  }

let bind operation f = 
  {
    NeedsPreprocess = fun () -> operation.NeedsPreprocess();
    PreProcess = fun (operationState) -> operation.PreProcess(operationState);
    Invoke = 
      fun (refState) ->
        let (operationResult, returnType) = operation.Invoke(refState)
        let nextOperation = f(returnType)
        ({
          operationResult with 
            NextOperation = Some(nextOperation); 
        }, ());
  }

let zero() =
  { 
    NeedsPreprocess = fun () -> false;
    PreProcess = fun () -> ();
    Invoke = fun (refState) -> 
      (({
        NextOperation = None; 
        Element = None;
      }, ()));
  } 

let render (useRef: RefState<'props> -> IRefValue<RefState<'props>>) delayedFunc props = 
  let refState = useRef({
    PrevProps = None;
    CurrentProps = props;
    Element = None;
    PreProcessOperations = [];
    PropsOperation = None;
    NextOperation = None;
  })

  // for op in refState.current.AllOperations do
  //   op.PreProcess()
  
  // let (currentOperation, appendNext) = 
  //   let changeOperation = List.tryFind (fun op -> op.Changed(refState.current)) refState.current.AllOperations
  //   match changeOperation with
  //     | Some(op) -> (op, false)
  //     | None -> 
  //       let lastOp = List.tryLast refState.current.AllOperations
  //       match lastOp with
  //         | Some(op) -> (op, true)
  //         | None -> 
  //           let firstOp = delayedFunc()
  //           refState.current <- {
  //             refState.current with AllOperations = List.append refState.current.AllOperations [firstOp]
  //             }
  //           (firstOp, true)
  
  // let (invokeResult, _) = currentOperation.Invoke(refState.current)

  // let withOp = 
  //   match invokeResult.NextOperation with
  //     | Some(op) when appendNext -> 
  //       {invokeResult.NextState with AllOperations = List.append invokeResult.NextState.AllOperations [op]}
  //     | _ -> invokeResult.NextState
  // refState.current <- withOp

  match refState.current.Element with
    | Some(element) -> element
    | None -> null

type HacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(operation, f) = bind operation f
  member this.Zero() = zero()
  member this.Delay(f) = f
  member this.Run(f) =
    fun props children -> 
      ofFunction 
        (render useRef f)
        props 
        children

let hacn<'a> = HacnBuilder<'a>(Hooks.useRef)