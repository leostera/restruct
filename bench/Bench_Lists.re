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
      |> add({desc: "Array.make", bench: make_array(size)})
      |> add({desc: "Belt.Array.make", bench: make_belt_array(size)})
      |> add({desc: "Belt.List.make", bench: make_strict_belt_list(size)})
      |> add({desc: "ReStruct.Lazy.List.make", bench: make_lazy_list(size)})
      |> add({desc: "List (inductive)", bench: make_strict_list(size)})
      |> on(Start, Utils.default_announcer(~size, ~name="List.Make"))
      |> on(Cycle, Utils.default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run({async: false})
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
      |> add({desc: "Array.append", bench: append_array(size)})
      |> add({desc: "Belt.Array.concat", bench: append_belt_array(size)})
      |> add({
           desc: "Belt.List.concat",
           bench: append_strict_belt_list(size),
         })
      |> add({
           desc: "ReStruct.Lazy.List.append",
           bench: append_lazy_list(size),
         })
      |> add({desc: "List.append", bench: append_strict_list(size)})
      |> on(Start, Utils.default_announcer(~size, ~name="List.Append"))
      |> on(Cycle, Utils.default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run({async: false})
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
      |> add({desc: "Belt.Array.slice", bench: take_array(size)})
      |> add({desc: "Belt.List.take", bench: take_strict_list(size)})
      |> add({desc: "ReStruct.Lazy.List.take", bench: take_lazy_list(size)})
      |> on(Start, Utils.default_announcer(~size, ~name="List.take"))
      |> on(Cycle, Utils.default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run({async: false})
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
      |> add({desc: "Belt.Array.sliceToEnd", bench: drop_array(size)})
      |> add({desc: "Belt.List.drop", bench: drop_strict_list(size)})
      |> add({desc: "ReStruct.Lazy.List.drop", bench: drop_lazy_list(size)})
      |> on(Start, Utils.default_announcer(~size, ~name="List.drop"))
      |> on(Cycle, Utils.default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run({async: false})
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
      |> add({desc: "Belt.Array.reverse", bench: reverse_array(size)})
      |> add({desc: "Belt.List.reverse", bench: reverse_strict_list(size)})
      |> add({
           desc: "ReStruct.Lazy.List.reverse",
           bench: reverse_lazy_list(size),
         })
      |> on(Start, Utils.default_announcer(~size, ~name="List.reverse"))
      |> on(Cycle, Utils.default_printer)
      |> on(Complete, _e => Js.log(""))
      |> run({async: false})
    );
};
