module LazyList = {
  open Lazy;

  type t('a) =
    | Nil
    | Cons('a, lazy_t(t('a)));

  let rec concat = (a, b) =>
    switch (force(a)) {
    | Nil => b
    | Cons(a', aa') => lazy (Cons(a', concat(aa', b)))
    };

  let head =
    fun
    | Nil => None
    | Cons(a, _) => Some(a);

  let tail =
    fun
    | Nil => None
    | Cons(_, aa) => Some(aa);
};

module Bench = {
  open ReBench;

  let make_lazy_list: int => LazyList.t(int) =
    x => {
      let rec __make =
        fun
        | (0, acc) => acc
        | (n, acc) => __make((n - 1, LazyList.Cons(0, lazy acc)));
      __make((x, LazyList.Nil));
    };

  let make_strict_list: int => list(int) =
    x => {
      let rec __make =
        fun
        | (0, acc) => acc
        | (n, acc) => __make((n - 1, [0, ...acc]));
      __make((x, []));
    };

  let run = size =>
    make()
    |> add("LazyList Build", () => {
         let _ = make_lazy_list(size);
         ();
       })
    |> add("StrictList Build", () => {
         let _ = make_strict_list(size);
         ();
       })
    |> on(
         Cycle,
         e => {
           let t = ReBench.target(e);
           Js.log(ReBench.name(t));
           Js.log(ReBench.count(t));
         },
       )
    |> on(
         Complete,
         e => {
           Js.log("Complete!");
           Js.log(e);
         },
       )
    |> run(run_opts(~async=true));
};

Bench.run(10);
