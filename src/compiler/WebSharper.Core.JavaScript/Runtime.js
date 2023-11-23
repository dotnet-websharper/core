
export function Create(ctor, copyFrom) {
  return Object.assign(Object.create(ctor.prototype), copyFrom);
}

export function Clone(obj) {
  return Object.assign(Object.create(Object.getPrototypeOf(obj)), obj);
}

export function Ctor(ctor, typeFunction) {
  ctor.prototype = typeFunction.prototype;
  return ctor;
}

export function Base(obj, base, ...args) {
  return Object.assign(obj, Reflect.construct(base, args, obj.constructor));
}

const forceSymbol = Symbol("force")
export function Force(obj) { obj[forceSymbol] }

export function Lazy(factory) {
  var instance;
  function getInstance() {
    if (!instance) {
      instance = factory(i => instance = i);
    }
    return instance;
  }
  let res = new Proxy(Function(), {
    get(_, key) {
      if (key == forceSymbol) {
        getInstance();
      }
      return getInstance()[key];
    },
    set(_, key, value) {
      getInstance()[key] = value;
      return true;
    },
    construct(_, args, newTarget) {
      return Reflect.construct(getInstance(), args, newTarget);
    }
  });
  return res;
}

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
    if (obj[f] === void 0) { delete obj[f]; }
  }
  return obj;
}

export function GetOptional(value) {
  return (value === void 0) ? null : { $: 1, $0: value };
}

export function SetOptional(obj, field, value) {
  if (value) {
    obj[field] = value.$0;
  } else {
    delete obj[field];
  }
}

export function SetOrDelete(obj, field, value) {
  if (value === void 0) {
    delete obj[field];
  } else {
    obj[field] = value;
  }
}

export function Apply(f, obj, args) {
  return f.apply(obj, args);
}

export function Bind(f, obj) {
  return function (...args) { return f.apply(obj, args) };
}

export function CreateFuncWithArgs(f) {
  return function (...args) { return f(args) };
}

export function CreateFuncWithOnlyThis(f) {
  return function () { return f(this) };
}

export function CreateFuncWithThis(f) {
  return function (...args) { return f(this)(...args) };
}

export function CreateFuncWithThisArgs(f) {
  return function (...args) { return f(this)(args) };
}

export function CreateFuncWithRest(length, f) {
  return function (...args) { return f([...(args.slice(0, length)), args.slice(length)]) };
}

export function CreateFuncWithArgsRest(length, f) {
  return function (...args) { return f([args.slice(0, length), args.slice(length)]) };
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
  return function (...args) {
    args.unshift(this);
    return d.apply(null, args);
  };
}

export function ThisFuncOut(f) {
  return function (...args) {
    return f.apply(args.shift(), args);
  };
}

export function ParamsFunc(length, d) {
  return function (...args) {
    return d.apply(null, args.slice(0, length).concat([args.slice(length)]));
  };
}

export function ParamsFuncOut(length, f) {
  return function (...args) {
    return f.apply(null, args.slice(0, length).concat(args[length]));
  };
}

export function ThisParamsFunc(length, d) {
  return function (...args) {
    args.unshift(this);
    return d.apply(null, args.slice(0, length + 1).concat([args.slice(length + 1)]));
  };
}

export function ThisParamsFuncOut(length, f) {
  return function (...args) {
    return f.apply(args.shift(), args.slice(0, length).concat(args[length]));
  };
}

export function Curried(f, n, args) {
  args = args || [];
  return (a) => {
    var allArgs = args.concat([a === void 0 ? null : a]);
    if (n == 1)
      return f(...allArgs);
    if (n == 2)
      return (a) => f(...allArgs, a === void 0 ? null : a);
    return Curried(f, n - 1, allArgs);
  }
}

export function Curried2(f) {
  return (a) => (b) => f(a, b);
}

export function Curried3(f) {
  return (a) => (b) => (c) => f(a, b, c);
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

const Runtime = {
  ScriptBasePath: "./",
  ScriptSkipAssemblyDir: false
}

export default Runtime;

export function ScriptPath(a, f) {
  return Runtime.ScriptBasePath + (Runtime.ScriptSkipAssemblyDir ? "" : a + "/") + f;
}

export function GetterOf(o, n) {
  return Object.getOwnPropertyDescriptor(o, n).get;
}

export function SetterOf(o, n) {
  return Object.getOwnPropertyDescriptor(o, n).set;
}

let scriptsLoaded = [];

export function LoadScript(u) {
  if (!scriptsLoaded.some(s => s == u.toLowerCase())) {
    if (!u.startsWith("http")) {
      u = Runtime.ScriptBasePath + u;
    }
    let xhr = new XMLHttpRequest();
    xhr.open("GET", u, false);
    xhr.send(null);
    scriptsLoaded.push(u.toLowerCase());
    globalThis.eval(xhr.responseText);
  }
}

let load = [];

export function OnLoad(f) {
  load.push(f);
}

export function Start() {
  for (var i = 0; i < load.length; i++) {
    load[i]();
  }
  load = [];
}

export function ignore() { }
export function id(x) { return x }
export function fst(x) { return x[0] }
export function snd(x) { return x[1] }
export function trd(x) { return x[2] }
