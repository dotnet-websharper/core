// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
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

namespace WebSharper.Comparers

open WebSharper

[<JavaScript>]
type private EquatableEqualityComparer<'T when 'T :> System.IEquatable<'T>>() =
    inherit System.Collections.Generic.EqualityComparer<'T>()
    override this.Equals(x, y) = (x :> System.IEquatable<_>).Equals(y)
    override this.GetHashCode(x) = (box x).GetHashCode()

[<JavaScript>]
type private BaseEqualityComparer<'T>() =
    inherit System.Collections.Generic.EqualityComparer<'T>()
    override this.Equals(x, y) = obj.Equals(box x, box y)
    override this.GetHashCode(x) = (box x).GetHashCode()

[<JavaScript>]
type private ComparableComparer<'T when 'T :> System.IComparable<'T>>() =
    inherit System.Collections.Generic.Comparer<'T>()
    override this.Compare(x, y) = (x :> System.IComparable<'T>).CompareTo(y)

[<JavaScript>]
type private BaseComparer<'T when 'T : comparison>() =
    inherit System.Collections.Generic.Comparer<'T>()
    override this.Compare(x, y) = compare x y

