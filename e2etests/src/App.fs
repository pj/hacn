module App

open Browser.Dom
open Fable.Import
open Feliz
open Fable.React

let Test = 
  React.functionComponent<'props>(
    fun () -> 
      Html.div 
        [
          prop.testId "test"
          prop.text "Say hello!"
        ]
  ) 

ReactDom.render(
    Test (),
    document.getElementById("app"))