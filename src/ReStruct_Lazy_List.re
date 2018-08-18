open Lazy;

type cell('a) =
  | Nil
  | Cons('a, t('a))
and t('a) = lazy_t(cell('a));

let rec append = (a, b) =>
  switch (force(a)) {
  | Nil => b
  | Cons(a', aa') => lazy (Cons(a', append(aa', b)))
  };

let head =
  fun
  | Nil => None
  | Cons(a, _) => Some(a);

let tail =
  fun
  | Nil => None
  | Cons(_, aa) => Some(aa);
