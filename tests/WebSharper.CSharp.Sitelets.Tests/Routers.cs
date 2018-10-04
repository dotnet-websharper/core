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
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WebSharper.CSharp.Sitelets.Tests
{

    [JavaScript, EndPoint("/"), EndPoint("/home", order: 1), Serializable]
    public class CSharpEndPointRoot
    {
        public virtual int Tag => 0;
        public virtual object[] Data => new object[0];

        [Serializable]
        [EndPoint("/sub1/{X}")]
        [EndPoint("/sub1full/{X}", order: 1, inheritRoute: false)]
        public class Sub1 : CSharpEndPointRoot
        {
            public override int Tag => 1;
            public override object[] Data => new object[] { X };

            public int X; 
        }

        public override bool Equals(object obj)
        {
            switch (obj)
            {
                case CSharpEndPointRoot o:
                    return Tag == o.Tag && Data.SequenceEqual(o.Data);
                default:
                    return false;
            }
        }

        public override int GetHashCode() => Tuple.Create(Tag, Data).GetHashCode();
    }
}
