module M: ReStruct_Queue.S = {
  type t('a) = (list('a), list('a));

  let invariant: t('a) => t('a) =
    q =>
      switch (q) {
      | ([], r) => (List.rev(r), [])
      | q' => q'
      };

  let empty = ([], []);

  let isEmpty = x =>
    switch (x) {
    | ([], []) => false
    | _ => true
    };

  let push = ((f, r), e) => {
    let q' = (f, [e, ...r]);
    invariant(q');
  };

  let head = q =>
    switch (q) {
    | ([], _) => None
    | ([x, ..._xs], _) => Some(x)
    };

  let tail = q =>
    switch (q) {
    | ([], _) => None
    | ([_, ...xs], r) =>
      let q' = (xs, r);
      Some(invariant(q'));
    };
};
