open Lazy;
open ReStruct_Lazy_List;

module M: ReStruct_Queue.S = {
  type t('a) = {
    front: ReStruct_Lazy_List.t('a),
    rear: list('a),
    schedule: ReStruct_Lazy_List.t('a),
  };

  let empty = {front: lazy Nil, rear: [], schedule: lazy Nil};

  let isEmpty = q => isEmpty(q.front);

  let rec rotate = ({front, rear, schedule}) =>
    switch (force(front), rear) {
    | (Nil, [r, ..._]) => lazy (Cons(r, schedule))
    | (Cons(f, fs), [r, ...rs]) =>
      lazy (
        Cons(
          f,
          rotate({front: fs, rear: rs, schedule: lazy (Cons(r, schedule))}),
        )
      )
    | (_, []) => schedule
    };

  let exec = q =>
    switch (force(q.schedule)) {
    | Nil =>
      let f' = rotate({front: q.front, rear: q.rear, schedule: lazy Nil});
      {front: f', rear: [], schedule: f'};
    | Cons(_, s) => {front: q.front, rear: q.rear, schedule: s}
    };

  let push = ({front, rear, schedule}, e) =>
    exec({front, rear: [e, ...rear], schedule});

  let head = ({front}) =>
    switch (force(front)) {
    | Nil => None
    | Cons(x, _) => Some(x)
    };

  let tail = ({front, rear, schedule}) =>
    switch (Lazy.force(front)) {
    | Nil => None
    | Cons(_, xs) => Some(exec({front: xs, rear, schedule}))
    };
};

include M;
