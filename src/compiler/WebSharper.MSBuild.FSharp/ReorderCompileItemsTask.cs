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

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace WebSharper.MSBuild.FSharp
{
    public sealed class ReorderCompileItemsTask : Task
    {
        public ITaskItem[] ExistingCompileItems { get; set; } = new ITaskItem[] { };

        [Output]
        public ITaskItem[] ReorderedCompileItems { get; set; }

        public override bool Execute()
        {
            var delayed = new Dictionary<string, List<ITaskItem>>();
            var reordered = new List<ITaskItem>();
            foreach (var item in ExistingCompileItems)
            {
                if (item.GetMetadata("WebSharperGenerated") == "true")
                {
                    var dependentUpon = item.GetMetadata("DependentUpon");
                    if (delayed.TryGetValue(dependentUpon, out var list)) 
                    {
                        list.Add(item);
                    }
                    else
                    {
                        list = new List<ITaskItem> { item };
                        delayed[dependentUpon] = list;
                    }
                }
                else
                {
                    reordered.Add(item);
                    if (delayed.TryGetValue(item.ItemSpec, out var list))
                    {
                        reordered.AddRange(list);
                        delayed.Remove(item.ItemSpec);
                    }
                }
            }
            ReorderedCompileItems = reordered.ToArray();
            return true;
        }
    }
}
