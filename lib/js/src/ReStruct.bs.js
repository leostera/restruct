// Generated by BUCKLESCRIPT VERSION 4.0.5, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");
var CamlinternalLazy = require("bs-platform/lib/js/camlinternalLazy.js");
var ReBench$Restruct = require("./ReBench.bs.js");

function concat(a, b) {
  var tag = a.tag | 0;
  var match = tag === 250 ? a[0] : (
      tag === 246 ? CamlinternalLazy.force_lazy_block(a) : a
    );
  if (match) {
    var aa$prime = match[1];
    var a$prime = match[0];
    return Block.__(246, [(function () {
                  return /* Cons */[
                          a$prime,
                          concat(aa$prime, b)
                        ];
                })]);
  } else {
    return b;
  }
}

function head(param) {
  if (param) {
    return Js_primitive.some(param[0]);
  }
  
}

function tail(param) {
  if (param) {
    return param[1];
  }
  
}

var LazyList = /* module */[
  /* concat */concat,
  /* head */head,
  /* tail */tail
];

function make_lazy_list(x) {
  var _param = /* tuple */[
    x,
    /* Nil */0
  ];
  while(true) {
    var param = _param;
    var n = param[0];
    if (n !== 0) {
      _param = /* tuple */[
        n - 1 | 0,
        /* Cons */[
          0,
          Block.__(250, [param[1]])
        ]
      ];
      continue ;
    } else {
      return param[1];
    }
  };
}

function make_strict_list(x) {
  var _param = /* tuple */[
    x,
    /* [] */0
  ];
  while(true) {
    var param = _param;
    var n = param[0];
    if (n !== 0) {
      _param = /* tuple */[
        n - 1 | 0,
        /* :: */[
          0,
          param[1]
        ]
      ];
      continue ;
    } else {
      return param[1];
    }
  };
}

function run(size) {
  return ReBench$Restruct.run({
              async: true
            }, ReBench$Restruct.on(/* Complete */1, (function (e) {
                    console.log("Complete!");
                    console.log(e);
                    return /* () */0;
                  }), ReBench$Restruct.on(/* Cycle */0, (function (e) {
                        var t = e.target;
                        console.log(t.name);
                        console.log(t.count);
                        return /* () */0;
                      }), ReBench$Restruct.add("StrictList Build", (function () {
                            make_strict_list(size);
                            return /* () */0;
                          }), ReBench$Restruct.add("LazyList Build", (function () {
                                make_lazy_list(size);
                                return /* () */0;
                              }), ReBench$Restruct.make(/* () */0))))));
}

var Bench = /* module */[
  /* make_lazy_list */make_lazy_list,
  /* make_strict_list */make_strict_list,
  /* run */run
];

run(10);

exports.LazyList = LazyList;
exports.Bench = Bench;
/*  Not a pure module */