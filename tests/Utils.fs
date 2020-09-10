module Utils
open Fable.Mocha
open Expecto
open Hacn.Core
open Hacn.Operations
open Fable.React
open Fable.ReactServer
open Fable.React.Props
open Hacn.Types
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Dyn

let generateFakeRefHook () =
  let mutable refValue = None
  let useFakeRef initialValue =
    match refValue with
    | None -> 
      refValue <- Some(initialValue)
    | _ -> ()

    { new IRefValue<_> with
        member this.current 
          with get() = 
            match refValue with
            | Some(x) -> x
            | None -> failwith "no initial value provided"
          and set value = refValue <- Some(value) }
  useFakeRef

type ContextResponse = { Everyone: string}

let useFakeContext (context) =
  { Everyone = "Everyone"}

let useFakeState initialState =
  let mutable value = initialState
  { new IStateHook<'T> with
    member __.current = value
    member __.update(x: 'T) = ()
    member __.update(f: 'T->'T) = () }

let useFakeEffect effectFunction =
  effectFunction ()

let hacnTest () =
  let useFakeRef = generateFakeRefHook ()
  HacnBuilder((render useFakeRef useFakeState useFakeEffect))

type TestHTMLNode = 
  | Text of string
  | RawText of string
  | Node of string * obj * TestHTMLNode list
  | List of TestHTMLNode list
  | Empty

let rec convertToTestNode (htmlNode: HTMLNode) =
    match htmlNode with
    | HTMLNode.Text str -> Text str
    | HTMLNode.RawText str -> RawText str
    | HTMLNode.Node (tag, attrs, children) ->
      let testNodes = Seq.map (castHTMLNode >> convertToTestNode) children
      TestHTMLNode.Node(tag, attrs, (List.ofSeq testNodes))
    | HTMLNode.List nodes ->
      let testNodes = Seq.map (castHTMLNode >> convertToTestNode) nodes
      TestHTMLNode.List (List.ofSeq testNodes)
    | HTMLNode.Empty -> TestHTMLNode.Empty
  
let convertElementToTestNode element = 
  convertToTestNode (castHTMLNode element)

let rec matchNode test node =
    match test, node with
    | Text str1, Text str2 when str1 = str2 -> ()
    | RawText str1, RawText str2 when str1 = str2 -> ()
    | Node(tag1, _, children1), Node(tag2, _, children2) when tag1 = tag2 ->
      matchNode (List(children1)) (List(children2))
    | List nodes1, List nodes2 when nodes1.Length = nodes2.Length ->
      for x, y in (List.zip nodes1 nodes2) do
        matchNode x y 
    | Empty, Empty -> ()
    | _ -> failwith (sprintf "Test doesn't match node: %A %A\n" test node)

let runTestSequence element testPairs =
  for props, test in testPairs do
    let generatedElement = element props []
    matchNode test (convertElementToTestNode generatedElement)

let TestOperation<'returnType> (result: 'returnType) = 
  Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let effectFunc rerender =
        printf "Calling effect func\n"
        rerender (fun _ -> Some(result :> obj))
        None
      match operationState with
      | None -> 
        InvokeWait(None, Some(effectFunc))
      | Some(result) -> 
        let castResult: 'returnType = explicitConvert result
        InvokeContinue(None, None, castResult)
  })

type DelayedOperationState = 
  {
    Status: bool
  }

let TestDelayedOperation<'returnType> (result: 'returnType) = 
  Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let delayedFunc rerender =
        rerender (fun _ -> Some({Status = true} :> obj))
        None
      let returnedFunc rerender =
        rerender (fun _ -> Some({Status = false} :> obj))
        None
      match operationState with
      | None -> 
        InvokeWait(None, Some(delayedFunc))
      | Some(status) -> 
        let castStatus: DelayedOperationState = explicitConvert status
        match castStatus with
        | {Status = true} -> InvokeWait(None, Some(returnedFunc))
        | {Status = false} -> InvokeContinue(None, None, result)
  })

type TestStreamTrigger () = 
  let mutable trigger = None
  member this.Trigger with set value = trigger <- Some(value)
  member this.Call value = 
    // printf "calling asdf\n"
    match trigger with
    | Some(underlying) -> 
      // printf "calling underlying\n"
      underlying value
    | None -> failwith "Should not happen"

let TestStreamOperation<'returnType> (trigger: TestStreamTrigger) =
  Perform({
    OperationType = NotCore
    PreProcess = fun _ -> None
    GetResult = fun _ operationState ->
      let effectFunc rerender =
        let returnTrigger value = 
          rerender (fun _ -> Some(value :> obj))
        trigger.Trigger <- returnTrigger
        None
      match operationState with
      | None -> 
        InvokeWait(None, Some(effectFunc))
      | Some(result) -> 
        let castResult: 'returnType = explicitConvert result
        InvokeContinue(None, None, castResult)
  })
