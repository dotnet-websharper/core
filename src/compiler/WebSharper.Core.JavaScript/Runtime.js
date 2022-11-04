
//export function Create(ctor, copyFrom) {
//  var obj = new ctor();
//  //Object.setPrototypeOf(obj, ctor.prototype);
//  Object.assign(obj, copyFrom);
//  return obj;
//}

export function Clone(obj) {
  var res = { ...obj };
  Object.setPrototypeOf(res, Object.getPrototypeOf(obj));
  return res;
}

//NewObject:
//  function (kv) {
//    var o = {};
//    for (var i = 0; i < kv.length; i++) {
//      o[kv[i][0]] = kv[i][1];
//    }
//    return o;
//  }

export function PrintObject(obj) {
  let res = "{ ";
  let empty = true;
  for (var field of Object.getOwnPropertyNames(obj)) {
    if (empty) {
      empty = false;
    } else {
      res += ", ";
    }
    res += field + " = " + obj[field];
  }
  if (empty) {
    res += "}";
  } else {
    res += " }";
  }
  return res;
}

export function DeleteEmptyFields(obj, fields) {
  for (var i = 0; i < fields.length; i++) {
    var f = fields[i];
    if (obj[f] === void (0)) { delete obj[f]; }
  }
  return obj;
}

export function GetOptional(value) {
  return (value === void (0)) ? null : { $: 1, $0: value };
}

export function SetOptional(obj, field, value) {
  if (value) {
    obj[field] = value.$0;
  } else {
    delete obj[field];
  }
}

export function SetOrDelete(obj, field, value) {
  if (value === void (0)) {
    delete obj[field];
  } else {
    obj[field] = value;
  }
}

export function Apply(f, obj, args) {
  return f.apply(obj, args);
}

export function Bind(f, obj) {
  return function () { return f.apply(obj, arguments) };
}

export function CreateFuncWithArgs(f) {
  return function () { return f(Array.prototype.slice.call(arguments)) };
}

export function CreateFuncWithOnlyThis(f) {
  return function () { return f(this) };
}

export function CreateFuncWithThis(f) {
  return function () { return f(this).apply(null, arguments) };
}

export function CreateFuncWithThisArgs(f) {
  return function () { return f(this)(Array.prototype.slice.call(arguments)) };
}

export function CreateFuncWithRest(length, f) {
  return function () { return f(Array.prototype.slice.call(arguments, 0, length).concat([Array.prototype.slice.call(arguments, length)])) };
}

export function CreateFuncWithArgsRest(length, f) {
  return function () { return f([Array.prototype.slice.call(arguments, 0, length), Array.prototype.slice.call(arguments, length)]) };
}

export function BindDelegate(func, obj) {
  var res = func.bind(obj);
  res.$Func = func;
  res.$Target = obj;
  return res;
}

export function CreateDelegate(invokes) {
  if (invokes.length == 0) return null;
  if (invokes.length == 1) return invokes[0];
  var del = function () {
    var res;
    for (var i = 0; i < invokes.length; i++) {
      res = invokes[i].apply(null, arguments);
    }
    return res;
  };
  del.$Invokes = invokes;
  return del;
}

export function CombineDelegates(dels) {
  var invokes = [];
  for (var i = 0; i < dels.length; i++) {
    var del = dels[i];
    if (del) {
      if ("$Invokes" in del)
        invokes = invokes.concat(del.$Invokes);
      else
        invokes.push(del);
    }
  }
  return CreateDelegate(invokes);
}

export function DelegateEqual(d1, d2) {
  if (d1 === d2) return true;
  if (d1 == null || d2 == null) return false;
  var i1 = d1.$Invokes || [d1];
  var i2 = d2.$Invokes || [d2];
  if (i1.length != i2.length) return false;
  for (var i = 0; i < i1.length; i++) {
    var e1 = i1[i];
    var e2 = i2[i];
    if (!(e1 === e2 || ("$Func" in e1 && "$Func" in e2 && e1.$Func === e2.$Func && e1.$Target == e2.$Target)))
      return false;
  }
  return true;
}

export function ThisFunc(d) {
  return function () {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(this);
    return d.apply(null, args);
  };
}

export function ThisFuncOut(f) {
  return function () {
    var args = Array.prototype.slice.call(arguments);
    return f.apply(args.shift(), args);
  };
}

export function ParamsFunc(length, d) {
  return function () {
    var args = Array.prototype.slice.call(arguments);
    return d.apply(null, args.slice(0, length).concat([args.slice(length)]));
  };
}

export function ParamsFuncOut(length, f) {
  return function () {
    var args = Array.prototype.slice.call(arguments);
    return f.apply(null, args.slice(0, length).concat(args[length]));
  };
}

export function ThisParamsFunc(length, d) {
  return function () {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(this);
    return d.apply(null, args.slice(0, length + 1).concat([args.slice(length + 1)]));
  };
}

export function ThisParamsFuncOut(length, f) {
  return function () {
    var args = Array.prototype.slice.call(arguments);
    return f.apply(args.shift(), args.slice(0, length).concat(args[length]));
  };
}

export function Curried(f, n, args) {
  args = args || [];
  return function (a) {
    var allArgs = args.concat([a === void (0) ? null : a]);
    if (n == 1)
      return f.apply(null, allArgs);
    if (n == 2)
      return function (a) { return f.apply(null, allArgs.concat([a === void (0) ? null : a])); }
    return Curried(f, n - 1, allArgs);
  }
}

export function Curried2(f) {
  return function (a) { return function (b) { return f(a, b); } }
}

export function Curried3(f) {
  return function (a) { return function (b) { return function (c) { return f(a, b, c); } } }
}

export function UnionByType(types, value, optional) {
  var vt = typeof value;
  for (var i = 0; i < types.length; i++) {
    var t = types[i];
    if (typeof t == "number") {
      if (Array.isArray(value) && (t == 0 || value.length == t)) {
        return { $: i, $0: value };
      }
    } else {
      if (t == vt) {
        return { $: i, $0: value };
      }
    }
  }
  if (!optional) {
    throw new Error("Type not expected for creating Choice value.");
  }
}

export function MarkResizable(arr) {
  Object.defineProperty(arr, "resizable", { enumerable: false, writable: false, configurable: false, value: true });
  return arr;
}

export function MarkReadOnly(arr) {
  Object.defineProperty(arr, "readonly", { enumerable: false, writable: false, configurable: false, value: true });
  return arr;
}

export var ScriptBasePath = "./";

export function ScriptPath(a, f) {
  return this.ScriptBasePath + (this.ScriptSkipAssemblyDir ? "" : a + "/") + f;
}

export function OnLoad(f) {
  if (!("load" in this)) {
    this.load = [];
  }
  this.load.push(f);
}

export function Start() {
  function run(c) {
    for (var i = 0; i < c.length; i++) {
      c[i]();
    }
  }
  if ("load" in this) {
    run(this.load);
    this.load = [];
  }
}

OnLoad(async () => {
  if (document && document.getElementById("websharper-data")) {
    (await import("./WebSharper.Activator.js")).default.Activate();
  }
});

export var ignore = () => { };
export var id = (x) => x;
export var fst = (x) => x[0];
export var snd = (x) => x[1];
export var trd = (x) => x[2];

//  if (!Global.console) {
//    Global.console = {
//      count: ignore,
//      dir: ignore,
//      error: ignore,
//      group: ignore,
//      groupEnd: ignore,
//      info: ignore,
//      log: ignore,
//      profile: ignore,
//      profileEnd: ignore,
//      time: ignore,
//      timeEnd: ignore,
//      trace: ignore,
//      warn: ignore
//    }
//  }
//}(self));