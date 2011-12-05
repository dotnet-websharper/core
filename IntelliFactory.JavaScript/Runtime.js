// $begin{copyright}
//
// This file is confidential and proprietary.
//
// Copyright (c) IntelliFactory, 2004-2011.
//
// All rights reserved.  Reproduction or use in whole or in part is
// prohibited without the written consent of the copyright holder.
//-----------------------------------------------------------------
// $end{copyright}

try {
    Object.defineProperty(Error.prototype, 'message', { enumerable: true });
} catch (e) { }

var IntelliFactory =
{
    Runtime:
    {

        Class:
            function (p, s) {
                function r() { }
                r.prototype = p;
                for (var f in s) { r[f] = s[f]; }
                return r;
            },

        Define:
            function (a, b) {
                function define(a, b) {
                    for (var k in b) {
                        var t1 = typeof a[k];
                        var t2 = typeof b[k];
                        if (t1 == "undefined") {
                            a[k] = b[k];
                        } else if (t1 == "object" && t2 == "object") {
                            define(a[k], b[k]);
                        } else {
                            throw new Error("Name conflict: " + k);
                        }
                    }
                }
                define(a, b);
            },

        Field:
            function (f) {
                var value, ready = false;
                return function () {
                    if (!ready) { ready = true; value = f(); }
                    return value;
                }
            },

        For:
            function (lowerBound, upperBound, body) {
                for (var i = lowerBound; i <= upperBound; i++) {
                    body(i);
                }
            },

        ForEach:
            function (obj, body) {
                for (var f in obj) {
                    body(f);
                }
            },

        New:
            function (ctor, fields) {
                var r = new ctor();
                for (var f in fields) {
                    r[f] = fields[f];
                }
                return r
            },

        OnInit:
            function (f) {
                if (!("init" in this)) {
                    this.init = [];
                }
                this.init.push(f);
            },

        OnLoad:
            function (f) {
                if (!("load" in this)) {
                    this.load = [];
                }
                this.load.push(f);
            },

        Inherit:
            function (a, b) {
                var p = a.prototype;
                a.prototype = new b();
                for (var f in p) {
                    a.prototype[f] = p[f];
                }
            },

        Safe:
            function (x) {
                if (x === undefined) return {};
                return x;
            },

        Start:
            function () {
                function run(c) {
                    for (var i = 0; i < c.length; i++) {
                        c[i]();
                    }
                }
                if ("init" in this) {
                    run(this.init);
                }
                if ("load" in this) {
                    run(this.load);
                }
            },

        Throw:
            function (e) {
                throw e;
            },

        Tupled:
            function (f) {
                return function (x) {
                    if (arguments.length > 1) {
                        return f(arguments);
                    } else {
                        return f(x);
                    }
                }
            },

        Try:
            function (block, handler) {
                try {
                    return block();
                } catch (e) {
                    return handler(e);
                }
            },

        TryFinally:
            function (block, handler) {
                try {
                    return block();
                } finally {
                    handler();
                }
            },

        While:
            function (guard, body) {
                while (guard()) {
                    body();
                }
            }
    }
};
