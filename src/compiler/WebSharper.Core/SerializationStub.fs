// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

/// Compiled instead of Serialization.g.fs with -p:WebSharperSkipGeneratedSerializer=true,
/// so WebSharper.Core builds when changed metadata types break the generated file.
/// Only for running genSerializer.fsx: this build cannot read or write metadata.
module internal WebSharper.Core.SerializationGenerated

open WebSharper.Core.Serialization

let [<Literal>] HashedKinds = 0

let private skipped () =
    failwith "WebSharper.Core was built with WebSharperSkipGeneratedSerializer, regenerate Serialization.g.fs and rebuild"

let readTables (r: MetaReader) : unit = skipped ()

let writeInfo (w: MetaWriter) (x: WebSharper.Core.Metadata.Info) : unit = skipped ()

let readInfo (r: MetaReader) : WebSharper.Core.Metadata.Info = skipped ()
