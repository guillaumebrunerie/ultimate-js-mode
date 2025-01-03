type T = {
//   ^ type.parameter
  x ?: number
//^ property.definition
// no highlighting on ?:
}

interface X {
//        ^ type.parameter
    a: (v: number) => number,
//  ^ property.definition
    b(v: number): number,
//  ^ property.definition
}

const x = {a: 2} as const;
//               ^ keyword
//                  ^ keyword

function isNum(v: unknown): v is number {
//                            ^ keyword
    return;
};

function assertNum(v: unknown): asserts v is number {
//                              ^ keyword
//                                        ^ keyword
    return;
};

type U = (v: unknown) => asserts v is number;
//                       ^ keyword
//                                 ^ keyword

type V = (v: unknown) => v is number;
//                         ^ keyword

type TT<X> = X extends (infer K)[] ? K : never;
//   ^ type.parameter
//      ^ type.parameter
//           ^ type
//                            ^ type.parameter
//                                   ^ type

import {a, type b} from "x";
//      ^ variable
//              ^ type.parameter

import type {a} from "x";
//           ^ type.parameter

const handleChange = (event: React.ChangeEvent) => {};
//                           ^ constructor

const handleChange = (event: React.ChangeEvent<"a">) => {};
//                           ^ constructor

export class MyClass {
  value: number,
//^ property.definition
  constructor();
//^ keyword
}

declare global {};
//      ^ keyword
declare module a {};
//      ^ keyword

export type {A} from "a";
//           ^ type

export {a, type C} from "a";
//              ^ type

const f: (abc: number) => void = (abc) => void abc;
//                        ^ type.builtin  ^ keyword

type StringToNumber<T> = T extends `${infer N extends number}`? N: never;
//                                    ^ keyword
//                                            ^ keyword

const f = <const T extends string>(x: T) => x;
//         ^ keyword
//                 ^ keyword

export function f();
//              ^ function

export function F();
//              ^ constructor

type S<T> = T extends ((v: infer N extends U) => void) ? N : never;
//                                         ^ type

type a = `aeb${number}e`;
const a = `aeb${number}e`;

// Delimiters
const a = f<number>(1 < 2)
type T = Foo<Bar<Baz>>;
const t = Foo(Bar(Baz));
const x = a < b && b > c;
