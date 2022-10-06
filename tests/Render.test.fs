module RenderTest

open Hacn.Core
open Hacn.Operations
open Hacn.Render
open Hacn.ElementExpressions
open Fable.Jester
open Fable.ReactTestingLibrary
open Feliz
open Browser.Types
open Fable.Mocha

type ClickTypes =
| Nothing
| One
| Two
| Three

let renderTests =
  testList "Render tests" [
    testCase "Render with capture as first operation" <| fun () ->
      let mutable clicked = false
      let App = 
        react {
          do! Render (
              div {
                testId "test"
                captureClick ()
              })
          clicked <- true
        }
      let result = RTL.render(App ())
      let element = result.getByTestId "test"
      RTL.fireEvent.click(element)
      Expect.isTrue clicked "Clicked is true"
      RTL.cleanup ()

    testCase "Render after several ops" <| fun () ->
      let mutable clicked = false
      let App = 
        react {
          let! props = Props
          let! state, setState = State ""
          do! Render (
              div {
                testId "test"
                captureClick ()
              })
          clicked <- true
        }
      let result = RTL.render(App ())
      let element = result.getByTestId "test"
      RTL.fireEvent.click(element)
      Expect.isTrue clicked "Clicked is true"
      RTL.cleanup ()

    testCase "Render with nested change capture" <| fun () ->
      let mutable setValue = ""
      let App = 
        react {
          let! props = Props
          let! state, setState = State ""
          let! value = Render div {
              id "test"
              children [
                main {
                  children [
                    input {
                      testId "test"
                      type' "text"
                      captureValueChange
                    }
                  ]
                }
              ] 
            }

          // Html.div [
          //   prop.id "asdf"
          //   prop.children [
          //     Html.main [
          //       prop.children [
          //         Html.input [
          //           prop.testId "test"
          //           prop.type' "text"
          //           prop.captureValueChange
          //         ]
          //       ]
          //     ]
          //   ]
          // ]
          setValue <- value
        }
      let result = RTL.render(App ())
      let element = result.getByTestId "test"

      RTL.fireEvent.change(element, [event.target [prop.value "Hello"]])
      Expect.equal setValue "Hello" "set value is correct"

      RTL.fireEvent.change(element, [event.target [prop.value "World"]])
      Expect.equal setValue "World" "set value is correct"

      RTL.cleanup ()

    testCase "Render multiple click types using capture typed union" <| fun () ->
      let mutable clickedButton = Nothing
      let App = 
        react {
          let! click = Render div {
              children [
                button {
                  testId "One"
                  captureClick One
                }
                button {
                  testId "Two"
                  captureClick Two
                }
                button {
                  testId "Three"
                  captureClick Three
                }
              ]
            }
          
          // Html.div [
          //   prop.children [
          //     Html.button [
          //       prop.testId "One"
          //       prop.captureClick One
          //     ]
          //     Html.button [
          //       prop.testId "Two"
          //       prop.captureClick Two
          //     ]
          //     Html.button [
          //       prop.testId "Three"
          //       prop.captureClick Three
          //     ]
          //   ]
          // ]
          clickedButton <- click
        }
      let result = RTL.render(App ())

      let element = result.getByTestId "One"
      RTL.fireEvent.click(element)
      Expect.equal clickedButton One "Clicked button is correct"

      let element = result.getByTestId "Two"
      RTL.fireEvent.click(element)
      Expect.equal clickedButton Two "Clicked button is correct"

      let element = result.getByTestId "Three"
      RTL.fireEvent.click(element)
      Expect.equal clickedButton Three "Clicked button is correct"

      let element = result.getByTestId "One"
      RTL.fireEvent.click(element)
      Expect.equal clickedButton One "Clicked button is correct"

      RTL.cleanup ()

    testCase "Render sequential captures and renders" <| fun () ->
      let App = 
        react {
          do! Render div {
            testId "click"
            captureClick ()
          }
          
          // Html.div [
          //   prop.testId "click"
          //   prop.captureClick ()
          // ]

          let! value = Render input {
            testId "change"
            type' "text"
            captureValueChange
          } 
          // Html.input [
          //   prop.testId "change"
          //   prop.type' "text"
          //   prop.captureValueChange
          // ]

          do! Render div {
            testId "finished"
            text (sprintf "Finished %s" value)
          }
          // Html.div [
          //   prop.testId "finished"
          //   prop.text (sprintf "Finished %s" value)
          // ]
        }
      let result = RTL.render(App ())

      let element = result.getByTestId "click"
      RTL.fireEvent.click(element)

      let element = result.getByTestId "change"
      RTL.fireEvent.change(element, [event.target [prop.value "Things"]])

      let element = result.getByTestId "finished"
      Expect.equal element.textContent "Finished Things" "Text is same"

      RTL.cleanup ()

    testCase "RenderCapture" <| fun () ->
      let element = react {
        let! changedValue = 
          RenderCapture(
            fun capture ->
              Html.div [
                prop.testId "test"
                prop.children [
                  Html.text "Say hello!"
                  Html.input [
                      prop.type' "text"
                      prop.testId "input"
                      prop.onChange (
                        fun (event: Browser.Types.Event) -> 
                          let element: HTMLInputElement = unbox event.target
                          capture element.value
                        )
                    ]
                ]
              ]
          )
        if changedValue = "Hello" then
          do! Render div {
            testId "test"
            text "Hi there!"
          }
          // Html.div [
          //     prop.testId "test"
          //     prop.text "Hi there!"
          //   ]
      }

      let result = RTL.render(element ())
      let element = result.getByTestId "test"
      Expect.equal element.textContent "Say hello!" "Text content equal" 

      let inputElement = result.getByTestId "input"
      RTL.fireEvent.change(inputElement, [event.target [prop.value "No"]])

      let element = result.getByTestId "test"
      Expect.equal element.textContent "Say hello!" "Text content equal" 

      let inputElement = result.getByTestId "input"
      RTL.fireEvent.change(inputElement, [event.target [prop.value "Okay"]])

      let element = result.getByTestId "test"
      Expect.equal element.textContent "Say hello!" "Text content equal" 

      let inputElement = result.getByTestId "input"
      RTL.fireEvent.change(inputElement, [event.target [prop.value "Hello"]])

      let element = result.getByTestId "test"
      Expect.equal element.textContent "Hi there!" "Text content equal" 
  ]

Mocha.runTests renderTests |> ignore