export function sayHi(user) {
  return `Hello, ${user}!`;
}

export default class WIGtest4 {
  sayHiInst(user) {
    return `Hello, ${user}!`;
  }

  static sayHiStatic(user) {
    return `Hello, ${user}!`;
  }
}