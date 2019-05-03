module Js = Js_of_ocaml.Js;
module Firebug = Js_of_ocaml.Firebug;
module Sys_js = Js_of_ocaml.Sys_js;

open Util;

Js_of_ocaml_toplevel.JsooTop.initialize();
Repl.SyntaxControl.re();

let completion = input => {
  let input = Js.to_string(input);

  let (startPos, completions) = Completion.complete(input);

  %js
  {
    val completions =
      completions |> Array.of_list |> Array.map(fst >> Js.string) |> Js.array;
    val startPos = startPos
  };
};

let execute = (js_send, input) => {
  print_endline("calling me");
  let input = Js.to_string(input);

  let send: Core.Evaluate.result => unit =
    content => {
      js_send(CoreTypes.Evaluate.result_to_js(content));
    };
  let complete = _ => {
    VendoredUTop.UTop_complete.reset();
  };

  module ReadStdout: Repl.ReadStdout.Sig = {
    type capture = Buffer.t;

    let bff = Buffer.create(256);
    Sys_js.set_channel_flusher(stdout, Buffer.add_string(bff));

    let start = () => {
      bff;
    };

    let stop = bff => {
      let r = bff |> Buffer.contents;
      bff |> Buffer.reset;
      r;
    };
  };

  Repl.Evaluate.eval(
    ~send,
    ~complete,
    ~readStdout=(module ReadStdout),
    input,
  );
};

let () =
  Js.export_all([%js {val completion = completion; val execute = execute}]);
