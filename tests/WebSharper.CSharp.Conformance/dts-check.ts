// Type-checked by check.mjs with `strict` and without skipLibCheck: the TypeScript declarations WebSharper
// writes for the cases (and, through their imports, for WebSharper.StdLib) must be usable from strict code.
import Point from "./Js/build/Scripts/WebSharper/Conformance/Conformance.Point.js";
import Person from "./Js/build/Scripts/WebSharper/Conformance/Conformance.Person.js";
import { Run } from "./Js/build/Scripts/WebSharper/Conformance/Conformance.Runner.js";

declare const p: Point;
const x: number = p.X;
const same: boolean = Point.op_Equality(p, p) && p.Equals_1(p);
declare const ann: Person;
const greeting: string = ann.Greeting;
const lines: string[] = Run();
export { x, same, greeting, lines };
