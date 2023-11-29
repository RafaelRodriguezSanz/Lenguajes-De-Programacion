const { delayValue} = require("./delay");

async function asyncTryMany(array, f) {
    return (await Promise.allSettled(array.map(e => f(e))))
      .filter(result => result.status === 'fulfilled')
      .map(result => result.value);
}

async function asyncGoUntil(iterable, f, ms) {
    let res = [];
    return new Promise((resolve) => {
        resolve(delayValue(ms, res));
        Array.from(iterable).forEach(async element =>  {
            res.push(await f(element));
        });
    })
}

module.exports = {asyncTryMany, asyncGoUntil};