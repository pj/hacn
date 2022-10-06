module Hacn.ElementExpressions

open Fable.React.Props
open Fable.React

type ElementSequence<'returnType> =
  | ElementSequence of IHTMLProp list * ReactElement list * Ref<('returnType -> unit) option>

type ElementBuilder(elementConstructor) =
  [<CustomOperation("onClick")>]
  member _.OnClick(ElementSequence(props, children, captureRef), func) = 
    let clickProp = OnClick(func) :> IHTMLProp
    ElementSequence(clickProp :: props, children, captureRef) // (MouseEvent -> unit)

  [<CustomOperation("className")>]
  member _.ClassName(ElementSequence(props, children, captureRef), name) =
    let classNameProp = ClassName(name) :> IHTMLProp
    ElementSequence(classNameProp :: props, children, captureRef)

  [<CustomOperation("testId")>]
  member _.TestId(ElementSequence(props, children, captureRef), name) =
    let classNameProp = HTMLAttr.Custom("data-testid", name) :> IHTMLProp
    ElementSequence(classNameProp :: props, children, captureRef)

  [<CustomOperation("childText")>]
  member _.ChildText(ElementSequence(props, children, captureRef), text) =
    ElementSequence(props, List.append children [str text], captureRef)

  [<CustomOperation("children")>]
  member _.Children(ElementSequence(props, children, captureRef), moreChildren) =
    let refChildren = List.map (fun c -> c captureRef) moreChildren
    ElementSequence(props, List.append children refChildren, captureRef)

  [<CustomOperation("captureClick")>]
  member _.CaptureClick(ElementSequence(props, children, captureRef)) =
    let captureClickProp = DOMAttr.OnClick (fun _ -> (Option.get captureRef.contents) ()) :> IHTMLProp
    ElementSequence(captureClickProp :: props, children, captureRef)

  [<CustomOperation("captureClickEvent")>]
  member _.CaptureClickEvent(ElementSequence(props, children, captureRef)) =
    let captureClickProp = DOMAttr.OnClick (fun event -> (Option.get captureRef.contents) event) :> IHTMLProp
    ElementSequence(captureClickProp :: props, children, captureRef)

  member _.Yield(value) = 
    printf "Yield: %A" value
    ElementSequence([], [], ref None)

  // member _.For(value) =
  //   printf "For: %A" value
  //   ElementSequence([], [])
  
  member _.Zero () =
    printf "Yield"
    ElementSequence([], [], ref None)
  
  member _.Run(ElementSequence(props, children, captureRef)) =
    fun (setResultRef: Ref<('returnType -> unit) option>) -> 
      captureRef := setResultRef.contents
      elementConstructor props children

let div = ElementBuilder(div)
let a = ElementBuilder(a)
let main = ElementBuilder(main)

// let asdf = 
//   div {
//     children [
//       div {
//         childText "asdf"
//       }
//       div {
//         childText "asdfasdf"
//         captureClickEvent
//       }
//     ] 
//     testId "erer"
//   }
