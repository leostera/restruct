open Bench_Utils;
open ReStruct;

let make_lazy_list = x => Lazy.List.make(x, 0);

let make_strict_list = x => {
  let rec __make =
    fun
    | (0, acc) => acc
    | (n, acc) => __make((n - 1, [0, ...acc]));
  __make((x, []));
};

let make_belt_list = x => Belt.List.make(x, 0);

let make_belt_array = x => Belt.Array.make(x, 0);

let make_array = x => Array.make(x, 0);

module Make = {
  let make_lazy_list = (x, ()) => {
    let _ = make_lazy_list(x);
    ();
  };

  let make_strict_list = (x, ()) => {
    let _ = make_strict_list(x);
    ();
  };

  let make_strict_belt_list = (x, ()) => {
    let _ = make_belt_list(x);
    ();
  };

  let make_array = (x, ()) => {
    let _ = make_array(x);
    ();
  };

  let make_belt_array = (x, ()) => {
    let _ = make_belt_array(x);
    ();
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Array.make", make_array(size))
      |> add("Belt.Array.make", make_belt_array(size))
      |> add("Belt.List.make", make_strict_belt_list(size))
      |> add("ReStruct.Lazy.List.make", make_lazy_list(size))
      |> add("List (inductive)", make_strict_list(size))
      |> on(Start, default_announcer(~size, ~name="List.Make"))
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

  let append_strict_belt_list = x => {
    let l = make_strict_list(x);
    let l' = make_strict_list(x);
    () => {
      let _ = Belt.List.concat(l, l');
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

  let append_belt_array = x => {
    let l = make_belt_array(x);
    let l' = make_belt_array(x);
    () => {
      let _ = Belt.Array.concat(l, l');
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
      |> add("Array.append", append_array(size))
      |> add("Belt.Array.concat", append_belt_array(size))
      |> add("Belt.List.concat", append_strict_belt_list(size))
      |> add("ReStruct.Lazy.List.append", append_lazy_list(size))
      |> add("List.append", append_strict_list(size))
      |> on(Start, default_announcer(~size, ~name="List.Append"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run(run_opts(~async=false))
    );
};

module Take = {
  let take_lazy_list = x => {
    let l = make_lazy_list(x);
    () => {
      let _ = Lazy.List.take(x, l);
      ();
    };
  };

  let take_strict_list = x => {
    let l = make_strict_list(x);
    () => {
      let _ = Belt.List.take(l, x);
      ();
    };
  };

  let take_array = x => {
    let l = make_array(x);
    () => {
      let _ = Belt.Array.slice(l, ~offset=0, ~len=x);
      ();
    };
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Belt.Array.slice", take_array(size))
      |> add("Belt.List.take", take_strict_list(size))
      |> add("ReStruct.Lazy.List.take", take_lazy_list(size))
      |> on(Start, default_announcer(~size, ~name="List.take"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run(run_opts(~async=false))
    );
};

module Drop = {
  let drop_lazy_list = x => {
    let l = make_lazy_list(x);
    () => {
      let _ = Lazy.List.drop(x, l);
      ();
    };
  };

  let drop_strict_list = x => {
    let l = make_strict_list(x);
    () => {
      let _ = Belt.List.drop(l, x);
      ();
    };
  };

  let drop_array = x => {
    let l = make_array(x);
    () => {
      let _ = Belt.Array.sliceToEnd(l, x);
      ();
    };
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Belt.Array.sliceToEnd", drop_array(size))
      |> add("Belt.List.drop", drop_strict_list(size))
      |> add("ReStruct.Lazy.List.drop", drop_lazy_list(size))
      |> on(Start, default_announcer(~size, ~name="List.drop"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run(run_opts(~async=false))
    );
};

module Reverse = {
  let reverse_lazy_list = x => {
    let l = make_lazy_list(x);
    () => {
      let _ = Lazy.List.reverse(l);
      ();
    };
  };

  let reverse_strict_list = x => {
    let l = make_strict_list(x);
    () => {
      let _ = Belt.List.reverse(l);
      ();
    };
  };

  let reverse_array = x => {
    let l = make_array(x);
    () => {
      let _ = Belt.Array.reverse(l);
      ();
    };
  };

  let run = size =>
    ReBench.(
      make()
      |> add("Belt.Array.reverse", reverse_array(size))
      |> add("Belt.List.reverse", reverse_strict_list(size))
      |> add("ReStruct.Lazy.List.reverse", reverse_lazy_list(size))
      |> on(Start, default_announcer(~size, ~name="List.reverse"))
      |> on(Cycle, default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run(run_opts(~async=false))
    );
};
