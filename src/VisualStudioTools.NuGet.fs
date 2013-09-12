// Copyright 2013 IntelliFactory
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

namespace IntelliFactory.VisualStudioTools

module NuGet =
    open System
    type Content = IntelliFactory.VisualStudioTools.Utils.Content

    type Package =
        {
            PContent : Content
            PId : string
            PVersion : string
        }

        member p.Content = p.PContent
        member p.Id = p.PId
        member p.Version = p.PVersion

        static member Create(id: string, version: string, c: Content) =
            {
                PContent = c
                PId = id
                PVersion = version
            }
