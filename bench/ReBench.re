type t; /* abstract FFI type */

type noop = unit => unit;

type event_name =
  | Start
  | Cycle
  | Complete;

[@bs.deriving abstract]
type target = {
  name: string,
  count: int,
};

[@bs.deriving abstract]
type event = {
  timeStamp: int,
  target,
};

type handler = event => unit;

type case = (string, noop);

[@bs.deriving abstract]
type run_opts = {async: bool};

module FFI = {
  [@bs.module "benchmark"] [@bs.new]
  external __unsafe_make: unit => t = "Suite";

  [@bs.send] external __unsafe_add: (t, string, noop) => t = "add";

  [@bs.send] external __unsafe_on: (t, string, handler) => t = "on";

  [@bs.send] external __unsafe_run: (t, run_opts) => unit = "run";
};

module Utils = {
  let event_to_string: event_name => string =
    fun
    | Start => "start"
    | Cycle => "cycle"
    | Complete => "complete";
};

let make = FFI.__unsafe_make;

let on: (event_name, handler, t) => t =
  (n, h, s) => FFI.__unsafe_on(s, Utils.event_to_string(n), h);

let add: (string, noop, t) => t = (n, f, s) => FFI.__unsafe_add(s, n, f);

let run: (run_opts, t) => unit = (o, s) => FFI.__unsafe_run(s, o);
