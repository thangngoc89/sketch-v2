let buffer = Buffer.create(256);
let ppf = Format.formatter_of_buffer(buffer);

/** {2 Communication} */;

/** {2 Communication} */;

let protocol_success = message => Console.log("Success: " ++ message);

let protocol_error = message => Console.log("Error: " ++ message);

let protocol_interrupt = () => Console.log("interrupted");

/** {2 Execution} */;

/** {2 Execution} */

let eval_phrase = phrase => {
  Warnings.reset_fatal();
  Env.reset_cache_toplevel();
  let is_ok = Toploop.execute_phrase(true, ppf, phrase);
  let message = Buffer.contents(buffer);
  Buffer.clear(buffer);
  (is_ok, message);
};

[@deriving show]
type evalStatus =
  | Evstt_ok
  | Evstt_error
  | Evstt_abort;

let eval = (~send, code) => {
  let rec loop = status =>
    fun
    | [] => status
    | [phrase, ...tl] =>
      switch (eval_phrase(phrase)) {
      | (true, "") => loop(status, tl)
      | (true, msg) =>
        send(protocol_success(msg));
        loop(status, tl);
      | (false, msg) =>
        send(protocol_error(msg));
        Evstt_error;
      };

  try (
    {
      let filename = "//toplevel//";
      let lexbuf = Lexing.from_string(code ++ "\n");
      Location.init(lexbuf, filename);
      Location.input_name := filename;
      Location.input_lexbuf := Some(lexbuf);
      loop(Evstt_ok, Toploop.parse_use_file^(lexbuf));
    }
  ) {
  | Sys.Break =>
    send(protocol_interrupt());
    Evstt_abort;
  | exn =>
    let msg = Error.to_string_hum(~ctx_size=1, exn);
    /* send(Iopub.error(~value="compile_error", [msg])); */
    send(protocol_error(msg));
    Evstt_error;
  };
};
