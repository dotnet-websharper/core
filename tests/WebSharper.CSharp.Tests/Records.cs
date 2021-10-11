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
            var student = new Teacher("Bill", "Wagner", "English");
            IsTrue(person == person2);
            IsFalse(person == student);
        }

        [Test("C# record ToString")]
        public void ToStringTest()
        {
            var person = new Person("Bill", "Wagner");
            Equal(person.ToString(), "Person { LastName = Wagner, FirstName = Bill }");
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

        [Test("C# positional record equality")]
        public void PosEquality()
        {
            var person = new PersonP("Bill", "Wagner");
            var person2 = new PersonP("Bill", "Wagner");
            var student = new TeacherP("Bill", "Wagner", "English");
            IsTrue(person == person2);
            IsFalse(person == student);
        }

        [Test("C# positional record ToString")]
        public void PosToStringTest()
        {
            var person = new PersonP("Bill", "Wagner");
            Equal(person.ToString(), "PersonP { FirstName = Bill, LastName = Wagner }");
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

        [Test("C# init only setter")]
        public void InitOnlySetter()
        {
            var person = new InitOnlyTest { Name = "Bill Wagner" };
            Equal(person.Name, "Bill Wagner");
        }
    }

    [JavaScript]
    public record Person
    {
        public string LastName { get; }
        public string FirstName { get; }

        public Person(string first, string last) // => (FirstName, LastName) = (first, last);
        {
            this.FirstName = first;
            this.LastName = last;
        }
    }

    [JavaScript]
    public record Teacher : Person
    {
        public string Subject { get; }

        public Teacher(string first, string last, string sub = "Math")
            : base(first, last) => Subject = sub;
    }

    [JavaScript]
    public record PersonP(string FirstName, string LastName); // positional record

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
    public class InitOnlyTest
    {
        public string Name { get; init; }
    }
}
