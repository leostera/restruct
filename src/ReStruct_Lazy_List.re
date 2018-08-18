open Lazy;

type cell('a) =
  | Nil
  | Cons('a, t('a))
and t('a) = lazy_t(cell('a));

let make = (x, el) => {
  let rec __make =
    fun
    | (0, acc) => acc
    | (n, acc) => __make((n - 1, lazy (Cons(el, acc))));
  __make((x, lazy Nil));
};

let rec of_list =
  fun
  | [] => lazy Nil
  | [x, ...xs] => lazy (Cons(x, of_list(xs)));

let rec append = (a, b) =>
  switch (force(a)) {
  | Nil => b
  | Cons(a', aa') => lazy (Cons(a', append(aa', b)))
  };

let head = x =>
  switch (force(x)) {
  | Nil => None
  | Cons(a, _) => Some(a)
  };

let tail = x =>
  switch (force(x)) {
  | Nil => None
  | Cons(_, aa) => Some(aa)
  };

let rec take = (n, s) =>
  switch (n) {
  | 0 => lazy Nil
  | n =>
    switch (force(s)) {
    | Nil => lazy Nil
    | Cons(x, xs) => lazy (Cons(x, take(n - 1, xs)))
    }
  };

let rec drop = (n, s) =>
  switch (n) {
  | 0 => s
  | n =>
    switch (force(s)) {
    | Nil => lazy Nil
    | Cons(_, xs) => drop(n - 1, xs)
    }
  };

let reverse = s => {
  let rec rev = (a, b) =>
    switch (force(a)) {
    | Nil => b
    | Cons(x, xs) => rev(xs, lazy (Cons(x, b)))
    };
  rev(s, lazy Nil);
};
