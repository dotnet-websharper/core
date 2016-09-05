(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)
[<AutoOpen>]
module StartupScript

#I __SOURCE_DIRECTORY__ //learned from http://stackoverflow.com/questions/4860991/f-for-scripting-location-of-script-file

//should do dynamic action while building to toggle between this local path and the relative path
#r "FsEye.dll" //release deployment expects this file next to the dll
//#r "bin/Release/FsEye.dll"


open Swensen.FsEye.Fsi
let eye = eye
fsi.AddPrintTransformer eye.Listener //attached the listener
