export function sayHi(user) {
  return `Hello, ${user}!`;
}

export default class MyClass { 
  sayHiInst(user) {
    return `Hello, ${user}!`;
  }

  static sayHiStatic(user) { 
    return `Hello, ${user}!`;
  }
}