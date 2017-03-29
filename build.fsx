// include Fake lib
#r @"packages\FAKE\tools\FakeLib.dll"

open Fake
open Fake.Testing
open Fake.FuchuHelper

// Directories
let buildDir  = @".\build\"
let testDir   = @".\test\"
let deployDir = @".\deploy\"
let packagesDir = @".\packages"

// Default target
Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)

Target "Compile" (fun _ ->
    !! @"lsharp.sln"
        |> MSBuildRelease buildDir "Build"
        |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
               !! (buildDir + "lsharp.test.exe")
               |> Fuchu
)


"Compile"
==> "Default"

"Compile"
==> "Test"

// start build
RunTargetOrDefault "Default"
