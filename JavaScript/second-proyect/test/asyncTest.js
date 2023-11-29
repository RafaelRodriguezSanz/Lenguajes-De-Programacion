var assert = require('assert');
const { asyncTryMany, asyncGoUntil } = require('../src/js/async');

describe('asyncTryMany', () => {
    it('debería filtrar y mapear correctamente', async () => {
        const array = [1, 2, 3];
        const f = async (x) => x * 2;
        const resultado = await asyncTryMany(array, f);
        assert.deepEqual(resultado, [2, 4, 6]);
    });
});

describe('asyncGoUntil', () => {
    it('debería ejecutar funciones hasta que se alcance el tiempo máximo', async () => {
        const iterable = [1, 2, 3];
        const f = async (x) => x * 2;
        const ms = 500;
        const resultado = await asyncGoUntil(iterable, f, ms);
    });
});
