module Hacn.Utils
#if FABLE_COMPILER
open Fable.Core.JsInterop
#else
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Dyn
#endif

#if FABLE_COMPILER
let castObj x =
  !!x
#else
let castObj x =
  explicitConvert x
#endif