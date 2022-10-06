module Hacn.ElementExpressions

open Fable.React.Props
open Fable.React

type ElementSequence<'returnType> =
  | ElementSequence of IHTMLProp list * ReactElement list * Ref<('returnType -> unit) option>

type ElementBuilder(elementConstructor) =
  [<CustomOperation("id")>]
  member _.Id(ElementSequence(props, children, captureRef), id) =
    let idProp = Id(id) :> IHTMLProp
    ElementSequence(idProp :: props, children, captureRef)

  [<CustomOperation("className")>]
  member _.ClassName(ElementSequence(props, children, captureRef), name) =
    let classNameProp = ClassName(name) :> IHTMLProp
    ElementSequence(classNameProp :: props, children, captureRef)

  [<CustomOperation("type'")>]
  member _.Type(ElementSequence(props, children, captureRef), type') =
    let typeProp = Type(type') :> IHTMLProp
    ElementSequence(typeProp :: props, children, captureRef)

  [<CustomOperation("testId")>]
  member _.TestId(ElementSequence(props, children, captureRef), name) =
    let classNameProp = HTMLAttr.Custom("data-testid", name) :> IHTMLProp
    ElementSequence(classNameProp :: props, children, captureRef)

  [<CustomOperation("text")>]
  member _.Text(ElementSequence(props, children, captureRef), text) =
    ElementSequence(props, List.append children [str text], captureRef)

  [<CustomOperation("children")>]
  member _.Children(ElementSequence(props, children, captureRef), moreChildren) =
    let refChildren = List.map (fun c -> c captureRef) moreChildren
    ElementSequence(props, List.append children refChildren, captureRef)

  [<CustomOperation("captureClick")>]
  member _.CaptureClick(ElementSequence(props, children, captureRef), value) =
    let captureClickProp = DOMAttr.OnClick (fun _ -> (Option.get captureRef.contents) value) :> IHTMLProp
    ElementSequence(captureClickProp :: props, children, captureRef)

  [<CustomOperation("captureClickEvent")>]
  member _.CaptureClickEvent(ElementSequence(props, children, captureRef)) =
    let captureClickProp = DOMAttr.OnClick (fun event -> (Option.get captureRef.contents) event) :> IHTMLProp
    ElementSequence(captureClickProp :: props, children, captureRef)

  [<CustomOperation("captureValueChange")>]
  member _.CaptureValueChange(ElementSequence(props, children, captureRef)) =
    let captureClickProp = DOMAttr.OnChange (fun event -> (Option.get captureRef.contents) event.Value) :> IHTMLProp
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
  
  // abstract Run: ElementSequence<'returnType> -> (Ref<('returnType -> unit) option> -> ReactElement)
  member _.Run(ElementSequence(props, children, captureRef)) =
    fun (setResultRef: Ref<('returnType -> unit) option>) -> 
      captureRef := setResultRef.contents
      elementConstructor props children

// type InputBuilder(elementConstructor) =
//   inherit ElementBuilder(elementConstructor)

//   override _.Run(ElementSequence(props, _, captureRef)) =
//     fun (setResultRef: Ref<('returnType -> unit) option>) -> 
//       captureRef := setResultRef.contents
//       elementConstructor props

let div = ElementBuilder(div)
let a = ElementBuilder(a)
let main = ElementBuilder(main)
let input = ElementBuilder(fun props _ -> input props)

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
