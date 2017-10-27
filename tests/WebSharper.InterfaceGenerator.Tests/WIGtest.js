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
                if (a == 'called through mixin')
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