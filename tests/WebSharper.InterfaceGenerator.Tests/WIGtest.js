var WIGtest = {
    ArgsFuncIn: function (f) {
        return f(1, 2)
    },

    ArgsFuncInStrings: function (f) {
        return f("Hello", "world")
    },

    ArgsFuncOut: function () {
        return function (a, b) { return a + b; }
    },

    GetThis: function () {
        return this;
    },

    GetGetThis: function () {
        return WIGtest.GetThis;
    },

    FuncInWithThis: function (f) {
        return f.call({ x: 0 });
    },

    ArgFuncInWithThis: function (f) {
        return f.call({ x: 0 }, 1);
    },

    ArgsFuncInWithThis: function (f) {
        return f.call({ x: 0 }, 1, 2);
    },

    TupledFuncInWithThis: function (f) {
        return f.call({ x: 0 }, [1, 2]);
    },

    Sum: function () {
        var res = 0;
        for (var i = 0; i < arguments.length; i++) {
            res += arguments[i];
        }
        return res;
    },

    SumBy: function (f) {
        var res = 0;
        for (var i = 1; i < arguments.length; i++) {
            res += f(arguments[i]);
        }
        return res;
    },

    SumByThenMap: function (f, g) {
        var res = 0;
        for (var i = 2; i < arguments.length; i++) {
            res += f(arguments[i]);
        }
        return g(res);
    },

    GetSum: function () {
        return WIGtest.Sum;
    },

    GetSumBy: function () {
        return WIGtest.SumBy;
    },

    GetSumByThenMap: function () {
        return WIGtest.SumByThenMap;
    },

    CallWith1: function(f) {
        return f(1);
    },

    CallWith2: function (f) {
        return f(1, 2);
    },

    CallWith10: function (f) {
        return f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    },

    Instance: {
        x: 0,
        add:
            function (a, b) {
                return a + b;
            },
        adderFunc:
            function (a, b) {
                return a + b;
            },
        adderFuncWithThis:
            function (a, b) {
                return this.x + a + b;
            },
        stringOrInt: 0,
        jsStringOrNum: 0,
        callMixin:
            function (a) {
                if (a = 'called through mixin')
                    return 'ok';
            }
    }
}

var StubTest = {
    Class: function () {
        this.X = 3;
        this.GetX = function () { return 3; };
        this.GetY = function () { return 3; };
    }
};

StubTest.Class.Static = function () { return 4; };

var StubTestClass = StubTest.Class;

var StubModule = {
  Function_add1: function(x) { return x + 1; },
  UnitFunction_returns2: function() { return 2; },
  Value_equals15: 15,
  Nested: {
    Function_add20: function(x) { return x + 20; },
    UnitFunction_returns54: function() { return 54; },
    Value_equals28: 28
  }
};

var AbsCls = function(arg) { this.arg = arg; }
AbsCls.prototype.virtMeth = function () { return "base virtual method from " + this.arg; }
AbsCls.prototype.concMeth = function () { return "concrete method from " + this.arg; }

var OverridingCls = function() { AbsCls.call(this, "OverridingCls"); }
OverridingCls.prototype = Object.create(AbsCls.prototype);
OverridingCls.prototype.absMeth = function () { return "overridden abstract method from OverridingCls"; }
OverridingCls.prototype.virtMeth = function () { return "overridden virtual method from OverridingCls"; }

var NonOverridingCls = function() { AbsCls.call(this, "NonOverridingCls"); }
NonOverridingCls.prototype = Object.create(AbsCls.prototype);
NonOverridingCls.prototype.absMeth = function () { return "overridden abstract method from NonOverridingCls"; }

var ConcCls = function(arg) { this.arg = arg; }
ConcCls.prototype.virtMeth = function () { return "base virtual method from " + this.arg; }
ConcCls.prototype.concMeth = function () { return "concrete method from " + this.arg; }

var Regression1010 = {
  A: function() { this.a = 42; },
  B: function() { Regression1010.A.call(this); }
};

Regression1010.A.prototype.m = function() { return this.a; }
Regression1010.B.prototype = Object.create(Regression1010.A.prototype);
