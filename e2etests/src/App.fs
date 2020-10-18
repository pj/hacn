module App

open Browser.Dom
open Feliz
open Fable.React
open Hacn.Core
open Hacn.Operations

type TestProps = {Test: string}

let Test = 
  react {
    let! props = Props
    do! RenderContinue Html.div [
      prop.testId "test"
      prop.text "Wait for it!"
    ]
    do! Timeout 2000
    do! Render Html.div [
      prop.testId "test"
      prop.text (sprintf "Say %s!" props.Test)
    ]
  }

ReactDom.render(
    Test {Test = "goodbye"},
    document.getElementById("app"))