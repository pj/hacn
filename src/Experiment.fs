module Hacn.Experiment

open Feliz
open Fable.React

type TestInterpreterObj =
    abstract member Eval<'a> : 'a -> unit

type OperationResult<'returnType> =
  | OperationWait
  | OperationContinue of 'returnType
and InterpreterResult =
  | InterpreterWait
  | InterpreterContinue of TestInterpreterObj
and ExperimentOperation<'returnType> =
  | ExperimentPerform of (unit -> OperationResult<'returnType>)
  | InterpreterPerform of (unit -> InterpreterResult)
  | Delay of (unit -> ExperimentOperation<unit>)
  | Wait
  | End

// let rec asdf next =
//   match next with
//   | Wait ->
//     next
//   | End ->
//     next
//   | ExperimentPerform (x) ->
//     let ffff = x ()
//     match ffff with 
//     | OperationWait -> 
//       Wait
//     | OperationContinue 
//       Wait

let bind underlyingOperation f =
  match underlyingOperation with
  | ExperimentPerform (underlyingOperationData) ->
      let experimentResult = underlyingOperationData ()
      match experimentResult with
      | OperationWait ->
        InterpreterPerform (fun () -> InterpreterWait)
      | OperationContinue (result) ->
        f result
        // match next with
        // | Wait ->
        //   Wait
        // | End
        //   End
        // InterpreterPerform (fun () -> InterpreterContinue doNext)
  | _ -> failwith (sprintf "Can't bind operation %A" underlyingOperation)

let TestOperation () = 
  let mutable test = 10

  let asdf () =
    OperationContinue test
  ExperimentPerform asdf

type ExperimentState = {
  Element: ReactElement option
}

let interpreter delayOperation props = 
    let firstOperation =
      match delayOperation with
      | Delay (f) -> f ()
      | _ -> failwith (sprintf "First operation from builder must be of type Delay: %A" delayOperation)

    let componentStateRef =
      Hooks.useRef (
        { Element = None }
      )
    
    firstOperation
    null

type ExperimentBuilder() =
  member _.Bind(operation, f) = bind operation f
  member _.Zero() = End
  member _.Delay(f) = Delay f

  member _.Run(firstOperation) =
    React.functionComponent<'props> (fun (props: 'props) -> interpreter firstOperation props)

let exp = ExperimentBuilder()

let testfunc () =
  Delay (
    fun () ->
      // Pre code lives here
      bind TestOperation () (
        fun result -> 
          

      )
  )