// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.Core

/// Helper delegate classes.
module Functions =

    /// A helper delegate class.
    type Func<'T1,'T2> =
        delegate of 'T1 -> 'T2

    /// A helper delegate class.
    type Func<'T1,'T2,'T3> =
        delegate of 'T1 * 'T2 -> 'T3

    /// A helper delegate class.
    type Func<'T1,'T2,'T3,'T4> =
        delegate of 'T1 * 'T2 * 'T3 -> 'T4

    /// A helper delegate class.
    type Func<'T1,'T2,'T3,'T4,'T5> =
        delegate of 'T1 * 'T2 * 'T3 * 'T4 -> 'T5

    /// A helper delegate class.
    type Func<'T1,'T2,'T3,'T4,'T5,'T6> =
        delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> 'T6

    /// A helper delegate class.
    type Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7> =
        delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> 'T7

    /// A helper delegate class.
    type Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8> =
        delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> 'T8

    /// A helper delegate class.
    type Func<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'T8,'T9> =
        delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8 -> 'T9
