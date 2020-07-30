module Hacn
open Fable.React.HookBindings
open Fable.React

// type HacnOperations = 
//     | Render of string
//     | Props

// type HacnControl =
//     | Continue of HacnOperations
//     | Suspend of HacnOperations
//     | Error

// let getProps = Suspend Props

// type HacnBuilder(useRef) =
//     let mutable prevprops = 1
//     member this.Bind(x, f) =
//         printfn "this.Bind: %A" x
//         match x with
//         | Suspend -> f(())
//         | Continue c -> f(())
//         | Error -> f(())
//     member this.Delay(f) = f()
//     member this.Return(x) = failwith "Unimplemented"
//     member this.Zero(x) = failwith "Unimplemented"

// let hacn = HacnBuilder(Hooks.useRef)

// let HacnFunction builder props = 
//     let refState = Hooks.useRef None
//     div [] [
//         str "Hello "
//         str "World"
//     ]

// let ofHacn props builder = 
//     ofFunction (HacnFunction builder) props [] 

// type QueryProps = {name: string; age: int}

// let qwer name age = 
//     ofHacn {name = name; age = age} (
//             hacn {
//                 let! props = Props
//                 return "asdf"
//             }
//         )