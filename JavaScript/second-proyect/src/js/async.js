const { delayValue} = require("./delay");
const {SettledResult} = require("./settledResult");
const {StatusEnum} = require("./settledEnum");

async function asyncTryMany(array, f) {
    return (await Promise.allSettled(array.map(e => f(e))))
      .filter(result => result.status === 'fulfilled')
      .map(result => result.value);
}

async function asyncGoUntil(iterable, f, ms) {
    let res = [];
    return new Promise((resolve) => {
        resolve(delayValue(ms, res));
        Array.from(iterable).forEach(async element =>
            await f(element)
                .then((result) => res.push(new SettledResult(StatusEnum.FULFILLED, result, null)))
                .catch(error => res.push(new SettledResult(StatusEnum.REJECTED, error, error)))
        );
    })
}

module.exports = {asyncTryMany, asyncGoUntil};