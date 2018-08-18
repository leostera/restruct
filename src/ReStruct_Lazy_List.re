open Lazy;

type t('a) =
  | Nil
  | Cons('a, lazy_t(t('a)));

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
