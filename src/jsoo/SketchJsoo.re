module Js = Js_of_ocaml.Js;

open Util;

Js_of_ocaml_toplevel.JsooTop.initialize();

let completion = input => {
  let input = Js.to_string(input);

  let (startPos, completions) = Completion.complete(input);

  %js
  {
    val completions =
      completions
      |> Array.of_list
      |> Array.map(fst >> Js.string)
      |> Js.array;
    val startPos = startPos
  };
};

let () = Js.export_all([%js {val completion = completion}]);
