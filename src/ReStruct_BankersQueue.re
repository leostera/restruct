open ReStruct.Lazy;

module M: ReStruct_Queue.S = {
  type t('a) = {
    f_size: int,
    f: List.t('a),
    r_size: int,
    r: List.t('a),
  };

  let invariant = q => {
    let {f_size, f, r_size, r} = q;
    r_size <= f_size ?
      q :
      {
        f_size: f_size + r_size,
        f: List.(append(f, reverse(r))),
        r_size: 0,
        r: lazy Nil,
      };
  };

  let empty = {f_size: 0, f: lazy Nil, r_size: 0, r: lazy Nil};

  let isEmpty = ({f_size}) => f_size == 0;

  let push = ({f_size, f, r_size, r}, e) =>
    invariant({f_size, f, r_size: r_size + 1, r: lazy (List.Cons(e, r))});

  let head = ({f}) =>
    switch (Lazy.force(f)) {
    | List.Nil => None
    | List.Cons(x, _) => Some(x)
    };

  let tail = ({f_size, f, r_size, r}) =>
    switch (Lazy.force(f)) {
    | List.Nil => None
    | List.Cons(_, xs) => Some({f_size: f_size - 1, f: xs, r_size, r})
    };
};
