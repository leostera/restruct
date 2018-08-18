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
      |> add("Lazy.List", make_lazy_list(size))
      |> add("Array.make", make_array(size))
      |> add("List", make_strict_list(size))
      |> on(Start, _e => Js.log("Begin Build Benchmark"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log("Complete!"))
      |> run(run_opts(~async=true))
    );
};

module Append = {
  let append_lazy_list = x => {
    let l = make_lazy_list(x);
    let l' = make_lazy_list(x);
    () => {
      let _ = Lazy.List.append(lazy l, lazy l');
      ();
    };
  };

  let append_strict_list = x => {
    let l = make_strict_list(x);
    let l' = make_strict_list(x);
    () => {
      let _ = List.append(l, l');
      ();
    };
  };

  let append_array = x => {
    let l = make_array(x);
    let l' = make_array(x);
    () => {
      let _ = Array.append(l, l');
      ();
    };
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Lazy.List.append", append_lazy_list(size))
      |> add("Array.append", append_array(size))
      |> add("List.append", append_strict_list(size))
      |> on(Start, _e => Js.log("Begin Append Benchmark"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log("Complete!"))
      |> run(run_opts(~async=true))
    );
};
