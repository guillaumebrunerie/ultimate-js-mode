#!/test/hashline
// ^ comment

const f1 = x => x;
//         ^ variable

const f2 = (x) => x;
//          ^ variable

const f3 = ({x}) => x;
//           ^ variable

const f4 = ({y: x, z}) => x + z;
//           ^ property.definition
//              ^ variable
//                 ^ variable

const f5 = (x = 3, {z = 2}) => x + z;
//          ^ variable
//                  ^ variable

const f6 = ([x, y]) => x + y;
//           ^ variable
//              ^ variable

const f7 = (...args) => args;
//              ^ variable

const x = {
  a: 2,
//^ property.definition
  A: B.C.d,
//^ property.definition
//   ^ constructor
//     ^ constructor
//       ^ property
  v
}

import * as React from "react";
//          ^ constructor
import {a, B} from "x";
//      ^ variable
//         ^ constructor

class MyClass {
//    ^ constructor
    constructor(){}
//  ^ keyword
    method(){}
//  ^ property.definition
}

// The testing syntax is not good enough to assert interesting stuff about the
// code below, check by hand (the inner template string is not currently working)
const a = "x" + `before${b => b + "a" + `inner${b}`}after`;

import * as styled from "styled";
//          ^ variable

const SBlock = styled.div`display: block;`
//    ^ constructor

for (const a of b) {}
//         ^ variable

export {a} from "b";

const a = void 3;
//        ^ keyword
