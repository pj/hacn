#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.JavaScript
open Fake.Tools
open System

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "Hacn"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "DSL for creating React components using Fable and F# computation expressions"

// Author(s) of the project
let author = "Paul Johnson"

// Github repo
let repo = "https://github.com/pj/hacn"

// Read additional information from the release notes document
let release = ReleaseNotes.load (__SOURCE_DIRECTORY__ @@ "RELEASE_NOTES.md")

let bin        = __SOURCE_DIRECTORY__ @@ "bin"
let docs       = __SOURCE_DIRECTORY__ @@ "docs"
let temp       = __SOURCE_DIRECTORY__ @@ "temp"
let objFolder  = __SOURCE_DIRECTORY__ @@ "obj"
let dist       = __SOURCE_DIRECTORY__ @@ "dist"
let src       = __SOURCE_DIRECTORY__ @@ "src"

// let install = lazy Fake.DotNet.install Fake.DotNet.Versions.FromGlobalJson

let configuration() =
    FakeVar.getOrDefault "configuration" "Release"

let getEnvFromAllOrNone (s: string) =
    let envOpt (envVar: string) =
        if String.isNullOrEmpty envVar then None
        else Some(envVar)

    let procVar = Environment.GetEnvironmentVariable(s) |> envOpt
    let userVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.User) |> envOpt
    let machVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.Machine) |> envOpt

    match procVar,userVar,machVar with
    | Some(v), _, _
    | _, Some(v), _
    | _, _, Some(v)
        -> Some(v)
    | _ -> None

Target.initEnvironment ()

// Set default
FakeVar.set "configuration" "Release"

Target.create "ConfigDebug" <| fun _ ->
    FakeVar.set "configuration" "Debug"

Target.create "ConfigRelease" <| fun _ ->
    FakeVar.set "configuration" "Release"

Target.create "NuGet" <| fun _ ->
    Paket.pack(fun p ->
        { p with
            ToolType = ToolType.CreateLocalTool()
            Version = release.NugetVersion
            ReleaseNotes = Fake.Core.String.toLines release.Notes
            ProjectUrl = repo
            MinimumFromLockFile = true
            IncludeReferencedProjects = true 
            WorkingDir = src
          })

Target.create "NuGetPublish" <| fun _ ->
    Paket.push(fun p ->
        { p with
            ApiKey = 
                match getEnvFromAllOrNone "NUGET_KEY" with
                | Some key -> key
                | None -> failwith "The NuGet API key must be set in a NUGET_KEY environment variable"
            WorkingDir = src
            ToolType = ToolType.CreateLocalTool()
        })

let gitPush msg =
    Git.Staging.stageAll ""
    Git.Commit.exec "" msg
    Git.Branches.push ""

Target.create "GitPush" <| fun p ->
    p.Context.Arguments
    |> List.choose (fun s ->
        match s.StartsWith("--Msg=") with
        | true -> Some(s.Substring 6)
        | false -> None)
    |> List.tryHead
    |> function
    | Some(s) -> s
    | None -> (sprintf "Bump version to %s" release.NugetVersion)
    |> gitPush

Target.create "GitTag" <| fun _ ->
    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" "origin" release.NugetVersion

// Target.create "PackageJson" <| fun _ ->
//     let setValues (current: Json.JsonPackage) =
//         { current with
//             Name = Str.toKebabCase project |> Some
//             Version = release.NugetVersion |> Some
//             Description = summary |> Some
//             Homepage = repo |> Some
//             Repository = 
//                 { Json.RepositoryValue.Type = "git" |> Some
//                   Json.RepositoryValue.Url = repo |> Some
//                   Json.RepositoryValue.Directory = None }
//                 |> Some
//             Bugs = 
//                 { Json.BugsValue.Url = 
//                     @"https://github.com/pj/hacn/issues/new/choose" |> Some } |> Some
//             License = "MIT" |> Some
//             Author = author |> Some
//             Private = true |> Some }
                
//     Json.setJsonPkg setValues

Target.create "Test" (fun _ -> 
  Npm.run "test" id
)

Target.create "All" ignore

Target.create "Release" ignore
Target.create "Publish" ignore

"Release" <== [
  "All"
  "NuGet"
  "ConfigRelease"
]

"Publish" <== [
  "Release"
  "ConfigRelease"
  "NuGetPublish" 
  "GitTag"
  "GitPush" 
]

Target.runOrDefault "All"