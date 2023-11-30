function distance(punto1, punto2) {
  if (punto1.coordenadas.length !== punto2.coordenadas.length) {
    throw new Error("Los puntos deben tener la misma cantidad de dimensiones");
  }

  let sum = 0;
  for (let i = 0; i < punto1.coordenadas.length; i++) {
    sum += Math.pow(punto1.coordenadas[i] - punto2.coordenadas[i], 2);
  }

  return Math.sqrt(sum);
}


function distanceXY(p1, p2) {
  if (!('x' in p1) || !('y' in p1) || !('x' in p2) || !('y' in p2)) {
      throw new Error('Los puntos deben tener las propiedades "x" e "y".');
  }

  return Math.sqrt(Math.pow(p2.x - p1.x, 2)+ Math.pow(p2.y - p1.y, 2));
}

module.exports = {distance, distanceXY};