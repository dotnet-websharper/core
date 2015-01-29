var WIGtest = {
    TupledFuncIn: function (f) {
        return f(1, 2)
    },

    TupledFuncOut: function() {
        return function(a, b) { return a + b; }
    },

    GetThis: function() {
        return this;
    },

    GetGetThis: function() {
        return WIGtest.GetThis;
    },

    TupledFuncInWithThis: function(f) {
       return f(this.x, a, b)
    }

}