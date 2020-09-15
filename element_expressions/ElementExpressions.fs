module ElementExpression
open Fable.React.Props
open Fable.React

type ElementSequence =
  | ElementSequence of IHTMLProp list * ReactElement list

type ElementBuilder(elementConstructor) =
  [<CustomOperation("onClick")>]
  member _.OnClick(ElementSequence(props, children), func) = 
    let clickProp = OnClick(func) :> IHTMLProp
    ElementSequence(clickProp :: props, children) // (MouseEvent -> unit)

  [<CustomOperation("className")>]
  member _.ClassName(ElementSequence(props, children), name) =
    let classNameProp = ClassName(name) :> IHTMLProp
    ElementSequence(classNameProp :: props, children)

  [<CustomOperation("testId")>]
  member _.TestId(ElementSequence(props, children), name) =
    let classNameProp = HTMLAttr.Custom("data-testid", name) :> IHTMLProp
    ElementSequence(classNameProp :: props, children)

  [<CustomOperation("childText")>]
  member _.ChildText(ElementSequence(props, children), text) =
    ElementSequence(props, List.append children [str text])

  member _.Yield(value) = 
    printf "Yield: %A" value
    ElementSequence([], [])

  member _.For(value) =
    printf "For: %A" value
    ElementSequence([], [])
  
  member _.Zero () =
    ElementSequence([], []) 
  
  member _.Run(ElementSequence(props, children)) =
    printf "Props: %A Children: %A" props children
    elementConstructor props children

let div = ElementBuilder(div)
let a = ElementBuilder(a)
let main = ElementBuilder(main)
