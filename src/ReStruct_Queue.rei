module type S = {
  type t('a);

  let empty: t('a);

  let isEmpty: t('a) => bool;

  let push: (t('a), 'a) => t('a);

  let head: t('a) => option('a);

  let tail: t('a) => option(t('a));
};
