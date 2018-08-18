type test; /* abstract FFI type */

module FFI = {
  [@bs.module] external __unsafe_make: (string, test => unit) => unit = "tape";
};

let make = FFI.__unsafe_make;

let t = make("sample");
