open Lazy;
open ReStruct.Lazy;

module M: ReStruct_Queue.S = {
  type t('a) = {
    front: List.t('a),
    rear: list('a),
    schedule: List.t('a),
  };

  let empty = {front: lazy List.Nil, rear: [], schedule: lazy List.Nil};

  let isEmpty = q => List.isEmpty(q.front);

  let rec rotate = ({front, rear, schedule}) =>
    switch (force(front), rear) {
    | (List.Nil, [r, ..._rs]) => lazy (List.Cons(r, schedule))
    | (List.Cons(f, fs), [r, ...rs]) =>
      lazy (
        List.Cons(
          f,
          rotate({
            front: fs,
            rear: rs,
            schedule: lazy (List.Cons(r, schedule)),
          }),
        )
      )
    | (_, []) => front
    };

  let exec = q =>
    switch (force(q.schedule)) {
    | List.Nil =>
      let f' = rotate(q);
      {front: f', rear: [], schedule: f'};
    | _ => q
    };

  let push = ({front, rear, schedule}, e) =>
    exec({front, rear: [e, ...rear], schedule});

  let head = ({front}) =>
    switch (force(front)) {
    | List.Nil => None
    | List.Cons(x, _) => Some(x)
    };

  let tail = ({front, rear, schedule}) =>
    switch (Lazy.force(front)) {
    | List.Nil => None
    | List.Cons(_, xs) => Some(exec({front: xs, rear, schedule}))
    };
};
