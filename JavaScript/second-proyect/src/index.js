const Punto = require("./js/punto");
const distance = require("./js/distance");
const range = require("./js/range");
const { asyncTryMany, asyncGoUntil } = require("./js/async");
const { delayValue } = require("./js/delay");

//const p1 = new Punto(1, 2);
//const p2 = new Punto(4, 5);
//
//console.log(distance(p1, p2));
//
//const p3 = new Punto(1, 2, 3);
//const p4 = new Punto(4, 5, 6);
//
//console.log(distance(p3, p4));
//
//const p5 = new Punto(1, 2, 3, 10);
//const p6 = new Punto(4, 5, 6, 7);
//
//console.log(distance(p5, p6));
//
//
//const resultado = range(0, 3);
//console.log([...resultado]);
//
//
//const resultado2 = range(3, 10);
//console.log([...resultado2]);
//
//
//const resultado3 = range(0, 10, 3);
//console.log([...resultado3]);
//
//
//const resultado4 = range(1, 1);
//console.log([...resultado4]);
//
//
//const resultado5 = range(10, 1, -1);
//console.log([...resultado5]);
//
////asyncTryMany([1,2,3], (v) => delayValue(v));
//


asyncGoUntil([1,2,3,4,5], (v) => 
        new Promise((resolve, reject) =>{
            if (v % 2 == 0) {
                reject(new Error('Es multiplo de 2'));
            } else {
                resolve(v);
            }
        })
    , 3)
    .then((result) => console.log([...result]))





//let a = {x:1, y:3}
//let b = {x:3, y:9}
//let c = {a:3, b:9}
//console.log(distance(c, b))