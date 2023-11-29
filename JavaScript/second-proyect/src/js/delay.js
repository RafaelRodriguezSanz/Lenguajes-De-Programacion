//function delayFunction(ms, callback = () => console.log(Date.now())) {
//  setTimeout(() => {
//    callback();
//  }, ms);
//}

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
    }, ms);
  })
}

module.exports = {delayValue, delayFunction, delayFunction};
