var WIGtest = {
    ArgsFuncIn: function (f) {
        return f(1, 2)
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
    }
}