function* range(begin, end, step = 1) {
  for (let i = begin; i < end; i += step) {
    yield i;
  }
}
module.exports = range;
