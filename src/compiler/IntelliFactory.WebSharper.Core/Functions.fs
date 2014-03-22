// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
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
