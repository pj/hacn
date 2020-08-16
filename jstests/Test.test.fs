module Test

open Fable.React
open Hacn
open Browser.Dom
open Fable.ReactTestingLibrary
open Fable.Jester

type TestProps =
  { Hello: string}

type ContextResponse =
  { Everyone: string}

let useFakeRef initialValue =
  let mutable refValue = initialValue
  console.log "here"
  { new IRefValue<_> with
      member this.current with get() = refValue and set value = refValue <- value }

let useFakeContext (context) =
  { Everyone = "Everyone"}

// // let TestContext = ContextNonPartial useFakeContext

type TestHacnBuilder<'props>(useRef: RefState<'props> -> IRefValue<RefState<'props>>) = 
  member this.Bind(operation, f) = bind operation f
  member this.Zero() = zero()
  member this.Delay(f) = f
  member this.Run(f) =
    fun props -> render useRef f props

let testHacn = TestHacnBuilder(useFakeRef)

Jest.describe("my tests", fun () ->
  Jest.test("water is wet", fun () ->
    // let context = createContext({Everyone = "Everyone"})
    // let x = Hooks.useContext

    let element = testHacn {
      let! props = Props()
      do! Render (div [] [ str props.Hello; str "World"])
    }

    let x = element {Hello = "Hello"}
    RTL.render(x) |> ignore
  )
)
