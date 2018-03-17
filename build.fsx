#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.MsBuild
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.DotNet

let buildDir = "./build/"
let testDir = "./test/"

// Targets
Target.Create "Clean" (fun _ ->
    Shell.CleanDir buildDir
)

Target.Create "BuildApp" (fun _ ->
    !! "src/**/*.fsproj"
    |> MsBuild.RunRelease buildDir "Build"
    |> Trace.Log "AppBuild-Output: "
)

Target.Create "BuildTest" (fun _ ->
    !! "src/**/*.test.fsproj"
      |> MsBuild.RunDebug testDir "Build"
      |> Trace.Log "TestBuild-Output: "
)

Target.Create "Test" (fun _ ->
    Trace.Log "AppTest-Output: " []
            //    !! (buildDir + "lsharp.test.exe")
            //    |> Fuchu
)

// Target.Create "Test" (fun _ ->
//     !! (testDir + "/*.test.exe")
//       |> NUnit3.NUnit3 (fun p ->
//           {p with
//                 ShadowCopy = false })
// )

// Default target
Target.Create "Default" (fun _ ->
  Trace.trace "Hello World from FAKE"
)

open Fake.Core.TargetOperators

"Clean"
    ==> "BuildApp"
    ==> "BuildTest"
    // ==> "Test"
    ==> "Default"

"BuildTest"
    ==> "Test"

// start build
Target.RunOrDefault "Default"