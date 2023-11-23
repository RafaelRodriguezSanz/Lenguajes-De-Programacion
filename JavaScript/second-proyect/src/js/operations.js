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
module.exports = distance;