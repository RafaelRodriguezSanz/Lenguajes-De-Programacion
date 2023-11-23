function delayFunction(ms, callback = () => console.log(Date.now())) {
  setTimeout(() => {
    callback();
  }, ms);
}

function delayFunction(ms, callback = () => console.log(Date.now())) {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve(callback());
    }, ms);
  })
}

function delayValue(ms, value = Date.now()) {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve(value);
      console.log(value);
    }, ms);
  })
}

async function asyncTryMany(array, f) {
  return (await Promise.allSettled(array.map(e => f(e))))
    .filter(result => result.status === 'fulfilled')
    .map(result => result.value);
}

asyncTryMany([1,2,3], (v) => delayValue(v));
