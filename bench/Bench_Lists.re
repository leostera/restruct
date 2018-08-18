open Bench_Utils;
open ReStruct;

let make_lazy_list = x => {
  let rec __make =
    fun
    | (0, acc) => acc
    | (n, acc) => __make((n - 1, Lazy.List.Cons(0, lazy acc)));
  __make((x, Lazy.List.Nil));
};

let make_strict_list = x => {
  let rec __make =
    fun
    | (0, acc) => acc
    | (n, acc) => __make((n - 1, [0, ...acc]));
  __make((x, []));
};

let make_array = x => Array.make(x, 0);

module Build = {
  let make_lazy_list = (x, ()) => {
    let _ = make_lazy_list(x);
    ();
  };

  let make_strict_list = (x, ()) => {
    let _ = make_strict_list(x);
    ();
  };

  let make_array = (x, ()) => {
    let _ = make_array(x);
    ();
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Lazy.List Build", make_lazy_list(size))
      |> add("Built-in Array Build", make_array(size))
      |> add("Built-in List Build", make_strict_list(size))
      |> on(Start, _e => Js.log("Begin Build Benchmark"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log("Complete!"))
      |> run(run_opts(~async=true))
    );
};

module Concat = {
  let consume_lazy_list = x => {
    let l = make_lazy_list(x);
    let l' = make_lazy_list(x);
    () => {
      let _ = Lazy.List.concat(l, l');
      ();
    };
  };

  let consume_strict_list = (x, ()) => {
    let _ = make_strict_list(x);
    ();
  };

  let consume_array = (x, ()) => {
    let _ = make_array(x);
    ();
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Lazy.List Build", make_lazy_list(size))
      |> add("Built-in Array Build", make_array(size))
      |> add("Built-in List Build", make_strict_list(size))
      |> on(Start, _e => Js.log("Begin Build Benchmark"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log("Complete!"))
      |> run(run_opts(~async=true))
    );
};
