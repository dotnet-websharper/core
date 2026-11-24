// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2026 IntelliFactory
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
using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using WebSharper.Testing;
using System.Collections;

namespace WebSharper.CSharp.Tests
{
    [JavaScript, Test("C# records")]
    class Records : TestCategory
    {
        [Test("C# record equality")]
        public void Equality()
        {
            var person = new Person("Bill", "Wagner");
            var person2 = new Person("Bill", "Wagner");
            var teacher = new Teacher("Bill", "Wagner", "English");
            IsTrue(person == person2);
            IsFalse(person == teacher);
            IsFalse(person == new Person("Bill", "Gates"));
            IsTrue(new PersonP("Bill", "Wagner") == new PersonP("Bill", "Wagner"));
            IsFalse(new PersonP("Bill", "Wagner") == new PersonP("Bill", "Gates"));
        }

        [Test("C# record GetHashCode")]
        public void HashCode()
        {
            // GetHashCode used to call Unchecked.Hash(this), which calls GetHashCode
            // again: a stack overflow in any hashing collection (dotnet-websharper/ui#314).
            Equal(new PersonP("Bill", "Wagner").GetHashCode(), new PersonP("Bill", "Wagner").GetHashCode());
            NotEqual(new PersonP("Bill", "Wagner").GetHashCode(), new PersonP("Bill", "Gates").GetHashCode());
            Equal(new Person("Bill", "Wagner").GetHashCode(), new Person("Bill", "Wagner").GetHashCode());
            Equal(new Teacher("Bill", "Wagner", "English").GetHashCode(), new Teacher("Bill", "Wagner", "English").GetHashCode());
            Equal(new PersonStructPos("Bill", "Wagner").GetHashCode(), new PersonStructPos("Bill", "Wagner").GetHashCode());
            var set = new HashSet<PersonP> { new("Bill", "Wagner"), new("Bill", "Wagner"), new("Bill", "Gates") };
            Equal(set.Count, 2);
            var dict = new Dictionary<PersonP, int> { [new PersonP("Bill", "Wagner")] = 1 };
            Equal(dict[new PersonP("Bill", "Wagner")], 1);
        }

        [Test("C# record EqualityContract")]
        public void EqualityContract()
        {
            // The EqualityContract getter used to read a field of its own name,
            // which called the getter again and overflowed the stack.
            var bill = new ContractP("Bill");
            IsTrue(bill.SameContract(new ContractP("Ann")));
            IsFalse(bill.SameContract(new ContractDerived("Bill", 1)));
            IsTrue(new ContractDerived("Bill", 1).SameContract(new ContractDerived("Ann", 2)));
            // a user-defined Equals that relies on EqualityContract
            IsTrue(new CaseInsensitive("Bill").Equals(new CaseInsensitive("BILL")));
            IsFalse(new CaseInsensitive("Bill").Equals(new CaseInsensitive("Ann")));
        }

        [Test("C# record ToString")]
        public void ToStringTest()
        {
            var person = new Person("Bill", "Wagner");
            Equal(person.ToString(), "Person { LastName = Wagner, FirstName = Bill }");
            var teacher = new Teacher("Bill", "Wagner", "English");
            Equal(teacher.ToString(), "Teacher { LastName = Wagner, FirstName = Bill, Subject = English }");
        }

        [Test("C# record struct")]
        public void RecordStruct()
        {
            var person = new PersonStruct("Bill", "Wagner");
            var person2 = new PersonStruct("Bill", "Wagner");
            IsTrue(person == person2);
            IsTrue(person.Equals(person2));
            Equal(person.ToString(), "PersonStruct { LastName = Wagner, FirstName = Bill }");
            var pos = new PersonStructPos("Bill", "Wagner");
            var copy = pos;
            copy.FirstName = "William";
            Equal(pos.FirstName, "Bill");
            Equal(copy.FirstName, "William");
            IsFalse(pos == copy);
            var changed = pos with { LastName = "W" };
            Equal(pos.LastName, "Wagner");
            Equal(changed.LastName, "W");
        }

        [Test("C# record inheritance")]
        public void Inheritance()
        {
            var teacher = new Teacher("Bill", "Wagner", "English");
            Equal(teacher.FirstName, "Bill");
            Equal(teacher.LastName, "Wagner");
            Equal(teacher.Subject, "English");
            var teacher2 = new Teacher("Bill", "Wagner");
            Equal(teacher2.Subject, "Math");
        }

        [Test("C# record pattern matching")]
        public void PatternMatch()
        {
            Person person = new ("Bill", "Wagner");
            if (person is { FirstName: var firstName, LastName: var lastName })
            {
                Equal(firstName, "Bill");
                Equal(lastName, "Wagner");
            }
        }

        [Test("C# positional record equality")]
        public void PosEquality()
        {
            var person = new PersonP("Bill", "Wagner");
            var person2 = new PersonP("Bill", "Wagner");
            var teacher = new TeacherP("Bill", "Wagner", "English");
            IsTrue(person == person2);
            IsFalse(person == teacher);
        }

        [Test("C# positional record ToString overridden sealed")]
        public void PosToStringTest()
        {
            var person = new PersonP("Bill", "Wagner");
            Equal(person.ToString(), "Bill Wagner");
            var teacher = new TeacherP("Bill", "Wagner", "English");
            Equal(teacher.ToString(), "Bill Wagner");
        }

        [Test("C# positional record with")]
        public void PosWithExpression()
        {
            var person = new PersonP("Bill", "Wagner");
            var person2 = person with { FirstName = "Thomas" };
            Equal(person2.FirstName, "Thomas");
            Equal(person2.LastName, "Wagner");
            var personClone = person with { };
            IsTrue(person == personClone);
            NotStrictEqual(person, personClone);
        }

        [Test("C# positional record deconstruction")]
        public void PosDeconstruct()
        {
            var person = new PersonP("Bill", "Wagner");
            var (lastName, firstName) = person;
            Equal(lastName, "Bill");
            Equal(firstName, "Wagner");
        }

        [Test("C# positional record inheritance")]
        public void PosInheritance()
        {
            var teacher = new TeacherP("Bill", "Wagner", "English");
            Equal(teacher.FirstName, "Bill");
            Equal(teacher.TFirstName, "Bill");
            Equal(teacher.LastName, "Wagner");
            Equal(teacher.Subject, "English");
            var teacher2 = new TeacherP("Bill", "Wagner");
            Equal(teacher2.Subject, "Math");
        }

        [Test("C# positional record pattern matching")]
        public void PosPatternMatch()
        {
            var person = new PersonP("Bill", "Wagner");
            if (person is { FirstName: var firstName, LastName: var lastName })
            {
                Equal(firstName, "Bill");
                Equal(lastName, "Wagner");
            }
        }

        [Test("C# positional record pattern matching with Deconstruct")]
        public void PosPatternMatchDeconstruct()
        {
            var person = new PersonP("Bill", "Wagner");
            var matched = false;
            if (person is var (firstName, lastName))
            {
                matched = true;
                Equal(firstName, "Bill");
                Equal(lastName, "Wagner");
            }
            IsTrue(matched);
            PersonP nobody = null;
            IsFalse(nobody is var (_, _));
        }

        [Test("C# init only setter")]
        public void InitOnlySetter()
        {
            var person = new InitOnlyTest { Name = "Bill Wagner" };
            Equal(person.Name, "Bill Wagner");
        }

        [Test("C# records: null, init-only initializers, default, ToString (#1646)")]
        public void RecordNullAndInit()
        {
            PersonP nobody = null;
            IsTrue(nobody == null);
            IsFalse(nobody != null);
            IsFalse(new PersonP("A", "B") == null);
            IsFalse(new PersonP("A", "B").Equals(null));
            object o = "x";
            IsTrue((o as PersonP) == null);

            var w = new RecordWithBody { A = "a" };
            Equal(w.A, "a");
            Equal(w.B, 0);
            Equal(new RecordWithBody { A = "x", B = 2 }.ToString(), "RecordWithBody { A = x, B = 2 }");
            Equal(default(RecordStructPos).ToString(), "RecordStructPos { X = 0, Y = 0 }");
            Equal(new RecordComputed("Ann").ToString(), "RecordComputed { Name = Ann, Greeting = Hi Ann }");

            var tags = new[] { 1 };
            IsTrue(new RecordWithArray(tags) == new RecordWithArray(tags));
            IsFalse(new RecordWithArray(new[] { 1 }) == new RecordWithArray(new[] { 1 }));
        }
    }

    [JavaScript]
    public record RecordWithBody { public string A { get; init; } public int B { get; init; } }

    [JavaScript]
    public record struct RecordStructPos(int X, int Y);

    [JavaScript]
    public record RecordComputed(string Name) { public string Greeting => $"Hi {Name}"; }

    [JavaScript]
    public record RecordWithArray(int[] Tags);

    [JavaScript]
    public record Person
    {
        public string LastName { get; }
        public string FirstName { get; }

        public Person(string first, string last) => (FirstName, LastName) = (first, last);
    }

    [JavaScript]
    public record Teacher : Person
    {
        public string Subject { get; }

        public Teacher(string first, string last, string sub = "Math")
            : base(first, last) => Subject = sub;
    }

    [JavaScript]
    public record PersonP(string FirstName, string LastName) // positional record
    {
        public override sealed string ToString()
        {
            return $"{FirstName} {LastName}";
        }
    }

    [JavaScript]
    public record TeacherP(string TFirstName, string LastName, string Subject = "Math") : PersonP(TFirstName, LastName);

    [JavaScript]
    public record PersonC(string FirstName)
    {
        private string middleName;

        public string MiddleName { get => middleName; set => middleName = value; }

        public string LastName { get; set; }
    }

    [JavaScript]
    public record ContractP(string Name)
    {
        public bool SameContract(ContractP other) => ReferenceEquals(EqualityContract, other.EqualityContract);
    }

    [JavaScript]
    public record ContractDerived(string Name, int Year) : ContractP(Name);

    [JavaScript]
    public record CaseInsensitive(string Name)
    {
        public virtual bool Equals(CaseInsensitive other) =>
            other is not null && ReferenceEquals(EqualityContract, other.EqualityContract) && Name.ToLower() == other.Name.ToLower();

        public override int GetHashCode() => Name.ToLower().GetHashCode();
    }

    [JavaScript]
    public record struct PersonStruct
    {
        public readonly string LastName;
        public readonly string FirstName;

        public PersonStruct(string first, string last) => (FirstName, LastName) = (first, last);
    }

    [JavaScript]
    public record struct PersonStructPos(string FirstName, string LastName);

    [JavaScript]
    public class InitOnlyTest
    {
        public string Name { get; init; }
    }
}
