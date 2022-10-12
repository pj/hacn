module Hacn.ElementExpressions

open Fable.React.Props
open Fable.React
open Fable.Core.JsInterop
open Fable.Core

type ElementSequence<'returnType> =
  | ElementSequence of IHTMLProp list * (('returnType -> unit) -> IHTMLProp) list * (('returnType -> unit) -> ReactElement) list

type ElementBuilder(elementConstructor) =
  [<CustomOperation("id")>]
  member _.Id(ElementSequence(props, captures, children), id) =
    let idProp = Id(id) :> IHTMLProp
    ElementSequence(idProp :: props, captures, children)

  [<CustomOperation("className")>]
  member _.ClassName(ElementSequence(props, captures, children), name) =
    let classNameProp = ClassName(name) :> IHTMLProp
    ElementSequence(classNameProp :: props, captures, children)

  [<CustomOperation("type'")>]
  member _.Type(ElementSequence(props, captures, children), type') =
    let typeProp = Type(type') :> IHTMLProp
    ElementSequence(typeProp :: props, captures, children)

  [<CustomOperation("testId")>]
  member _.TestId(ElementSequence(props, captures, children), name) =
    let classNameProp = HTMLAttr.Custom("data-testid", name) :> IHTMLProp
    ElementSequence(classNameProp :: props, captures, children)

  [<CustomOperation("text")>]
  member _.Text(ElementSequence(props, captures, children), text) =
    ElementSequence(props, captures, List.append children [(fun _ -> str text)])

  [<CustomOperation("placeholder")>]
  member _.PlaceHolder(ElementSequence(props, captures, children), text) =
    let placeholderProp = HTMLAttr.Placeholder text :> IHTMLProp
    ElementSequence(placeholderProp :: props, captures, children)

  [<CustomOperation("autoFocus")>]
  member _.AutoFocus(ElementSequence(props, captures, children), autofocus) =
    let autofocusProp = HTMLAttr.AutoFocus autofocus :> IHTMLProp
    ElementSequence(autofocusProp :: props, captures, children)

  [<CustomOperation("children")>]
  member _.Children(ElementSequence(props, captures, children), moreChildren) =
    // let refChildren = List.map (fun c -> c captureRef) moreChildren
    ElementSequence(props, captures, List.append children moreChildren)

  [<CustomOperation("ref")>]
  member _.Ref(ElementSequence(props, captures, children), ref) =
    let refProp = unbox ("ref", ref) :> IHTMLProp
    ElementSequence(refProp :: props, captures, children)

  [<CustomOperation("captureClick")>]
  member _.CaptureClick(ElementSequence(props, captures, children), value) =
    let captureClickProp = fun setResult -> DOMAttr.OnClick (fun _ -> setResult value) :> IHTMLProp
    ElementSequence(props, captureClickProp :: captures, children)

  [<CustomOperation("captureClickEvent")>]
  member _.CaptureClickEvent(ElementSequence(props, captures, children)) =
    let captureClickProp = fun setResult -> DOMAttr.OnClick (fun event -> setResult event) :> IHTMLProp
    ElementSequence(props, captureClickProp :: captures, children)

  [<CustomOperation("captureValueChange")>]
  member _.CaptureValueChange(ElementSequence(props, captures, children)) =
    let captureClickProp = fun setResult -> DOMAttr.OnChange (fun event -> setResult event.Value) :> IHTMLProp
    ElementSequence(props, captureClickProp :: captures, children)

  [<CustomOperation("captureKeyDown")>]
  member _.CaptureKeyDown(ElementSequence(props, captures, children)) =
    let captureClickProp = fun setResult -> DOMAttr.OnKeyDown (fun event -> setResult event.key) :> IHTMLProp
    ElementSequence(props, captureClickProp :: captures, children)

  member _.Yield(value) = 
    ElementSequence([], [], [])

  member _.Zero () =
    ElementSequence([], [], [])

  member _.Delay (f) = 
    f

  member _.Run(getElementSequence: (unit -> ElementSequence<'returnType>)) =
    fun (setResult: 'returnType -> unit) -> 
      let elementSequence = getElementSequence ()
      match elementSequence with 
      | ElementSequence(props, captures, children) ->
        let allProps = List.append props (List.map (fun c -> c setResult) captures)
        let captureChildren = List.map (fun c -> c setResult) children
        elementConstructor allProps captureChildren

// type InputBuilder(elementConstructor) =
//   inherit ElementBuilder(elementConstructor)

//   override _.Run(ElementSequence(props, _, captureRef)) =
//     fun (setResultRef: Ref<('returnType -> unit) option>) -> 
//       captureRef := setResultRef.contents
//       elementConstructor props

let div = ElementBuilder(div)
let a = ElementBuilder(a)
let main = ElementBuilder(main)
let header = ElementBuilder(header)
let input = ElementBuilder(fun props _ -> input props)
let button = ElementBuilder(button)
let h1 = ElementBuilder(h1)
