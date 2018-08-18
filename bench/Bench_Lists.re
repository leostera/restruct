open Bench_Utils;
open ReStruct;

let make_lazy_list = x => {
  let rec __make =
    fun
    | (0, acc) => acc
    | (n, acc) => __make((n - 1, lazy (Lazy.List.Cons(0, acc))));
  __make((x, lazy Lazy.List.Nil));
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
      |> add("Lazy.List (inductive)", make_lazy_list(size))
      |> add("List (inductive)", make_strict_list(size))
      |> add("Array.make", make_array(size))
      |> on(Start, default_announcer(~size, ~name="List.Build"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run(run_opts(~async=false))
    );
};

module Append = {
  let append_lazy_list = x => {
    let l = make_lazy_list(x);
    let l' = make_lazy_list(x);
    () => {
      let _ = Lazy.List.append(l, l');
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
      |> on(Start, default_announcer(~size, ~name="List.Append"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run(run_opts(~async=false))
    );
};
