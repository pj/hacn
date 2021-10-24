module Examples

open Hacn.Core
open Hacn.Operations
open Hacn.Types
open Feliz
open Hacn.Render
open Browser.Types

let Counter interval =
  printf "Setting interval: %A" interval
  Perform(
    { PreProcess = fun _ -> None
      GetResult =
        fun captureResult operationState ->
          let castOperationState : int option = unbox operationState

          // printf "Current State: %A" castOperationState

          match castOperationState with
          | Some (currentCount) ->
              PerformContinue(
                { Element = None
                  Effect = None
                  LayoutEffect = None
                  OperationState = Keep },
                currentCount
              )
          | None ->
              let timeoutEffect () =
                printf "Counter effect called"
                let timeoutCallback () =
                  let updateState existingState =
                    let castOperationState : int option = unbox existingState
                    // printf "Timer calling back state: %A" castOperationState

                    match castOperationState with
                    | Some (currentCount) -> Replace(currentCount + 1 :> obj)
                    | None -> Replace(1 :> obj)

                  captureResult updateState

                let timeoutID =
                  Fable.Core.JS.setInterval timeoutCallback interval

                Some
                  (fun _ ->
                    printf "Counter disposed: %A" timeoutID
                    Fable.Core.JS.clearInterval timeoutID
                    Erase)

              PerformWait(
                { Element = None
                  Effect = Some(timeoutEffect)
                  LayoutEffect = None
                  OperationState = Keep }
              ) }
  )


type ShowCountProps = { rate: int }

let ShowCount : ShowCountProps -> ReactElement =
  react {
    let! props = Props

    printf "Props changed rate: %d" props.rate

    if props.rate <= 0 then
      do! Render Html.div [ prop.text "Pausing Count" ]

    printf "Changing rate: %d" props.rate
    let! count = Counter props.rate
    // printf "Count: %d" count
    do! Render Html.div [ prop.text (sprintf "Current count: %d" count) ]
  }

let CountRate : unit -> ReactElement =
  react {
    let! (rate, setRate) = State 1000

    let! newRate =
      Render
        Html.div
        [ prop.children [ Html.div [ prop.text (sprintf "Current rate %d" rate) ]
                          Html.button [ prop.value "Stop"
                                        prop.text "Stop"
                                        prop.captureClick 0 ]
                          Html.button [ prop.value "100"
                                        prop.text "Rate: 100"
                                        prop.captureClick 100 ]
                          Html.button [ prop.value "1000"
                                        prop.text "Rate: 1000"
                                        prop.captureClick 1000 ]
                          ShowCount { rate = rate } ] ]

    do! setRate newRate
  }


let ShowCountDesugar : ShowCountProps -> ReactElement =
  react.Run(
    react.Delay
      (fun () ->
        react.Bind(
          Props,
          (fun props ->

            react.Combine(
              react.Delay
                (fun () ->
                  if props.rate <= 0 then
                    Render Html.div [ prop.text "Pausing Count" ]
                  else
                    react.Zero()),
              react.Delay
                (fun () ->
                  printf "Changing rate: %d" props.rate

                  react.Bind(
                    Counter props.rate,
                    fun count ->
                      printf "Count: %d" count

                      react.Bind(
                        Render Html.div [ prop.text (sprintf "Current count: %d" count) ],
                        (fun () -> react.Zero())
                      )
                  ))
            ))
        ))
  )

let CountRateDesugar : unit -> ReactElement =
  react.Run(
    react.Delay
      (fun () ->
        react.Bind(
          State 1000,
          fun (rate, setRate) ->
            react.Bind(
              Render
                Html.div
                [ prop.children [ Html.div [ prop.text (sprintf "Current rate %d" rate) ]
                                  Html.button [ prop.value "Stop"
                                                prop.captureClick 0 ]
                                  Html.button [ prop.value "100"
                                                prop.captureClick 100 ]
                                  Html.button [ prop.value "1000"
                                                prop.captureClick 1000 ]
                                  ShowCount { rate = rate } ] ],
              fun newRate -> react.Bind(setRate newRate, (fun () -> react.Zero()))
            )
        ))
  )
