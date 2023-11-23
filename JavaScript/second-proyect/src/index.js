const Punto = require("./js/punto");
const distance = require("./js/operations");
const range = require("./js/range");

const p1 = new Punto(1, 2); // Asigna coordenadas diferentes
const p2 = new Punto(4, 5); // Asigna coordenadas diferentes

console.log(distance(p1, p2)); // Deberías ver la distancia entre p1 y p2

const p3 = new Punto(1, 2, 3); // Asigna coordenadas diferentes
const p4 = new Punto(4, 5, 6); // Asigna coordenadas diferentes

console.log(distance(p3, p4)); // Deberías ver la distancia entre p1 y p2

const p5 = new Punto(1, 2, 3, 10); // Asigna coordenadas diferentes
const p6 = new Punto(4, 5, 6, 7); // Asigna coordenadas diferentes

console.log(distance(p5, p6)); // Deberías ver la distancia entre p1 y p2


const resultado = range(0, 3);
console.log([...resultado]);


const resultado2 = range(3, 10);
console.log([...resultado2]);


const resultado3 = range(0, 10, 3);
console.log([...resultado3]);


const resultado4 = range(1, 1);
console.log([...resultado4]);


const resultado5 = range(10, 1, -1);
console.log([...resultado5]);