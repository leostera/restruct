let default_announcer = (~size, ~name, _e) =>
  Js.log({j|Benchmark: $name (size: $size)|j});

let default_printer = e => {
  let t = ReBench.targetGet(e);
  let name = ReBench.nameGet(t);
  let count = ReBench.countGet(t);
  Js.log({j| => $name - $count ops|j});
};
