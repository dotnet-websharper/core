var WIGtest = {
    TupledFuncIn: function (f) {
        return f(1, 2)
    },

    TupledFuncOut: function() {
        return function(a, b) { return a + b; }
    }
}