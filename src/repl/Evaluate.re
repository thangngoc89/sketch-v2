open Util;
module Protocol = Core.Protocol;

let buffer = Buffer.create(256);
let ppf = Format.formatter_of_buffer(buffer);

/** {2 Communication} */;

/** {2 Communication} */;

let protocolSuccess = (~loc=?, ~msg) =>
  Protocol.Reply_ExecBlockContent({loc, result: Ok(msg |> String.trim)});

let protocolError = (~loc=?, ~error) =>
  Protocol.Reply_ExecBlockContent({loc, result: Error(error)});

let protocolInterrupt = () => Protocol.Reply_ExecInterupted;

/** {2 Execution} */;

/** {2 Execution} */

[@deriving show]
type evalStatus =
  | Evstt_ok
  | Evstt_error
  | Evstt_abort;

let rec last = (head, tail) =>
  switch (tail) {
  | [] => head
  | [head, ...tail] => last(head, tail)
  };

let locFromPhrase = {
  fun
  | Parsetree.Ptop_def([]) => None
  | Parsetree.Ptop_def([item, ...items]) => {
      let loc = {
        Location.loc_start: item.pstr_loc.Location.loc_start,
        Location.loc_end: last(item, items).pstr_loc.Location.loc_end,
        Location.loc_ghost: false,
      };
      Some(loc);
    }
  | Ptop_dir(_name, _argument) => None;
};

let eval_phrase = phrase => {
  Warnings.reset_fatal();
  Env.reset_cache_toplevel();
  let isOk = Toploop.execute_phrase(true, ppf, phrase);
  let message = Buffer.contents(buffer);
  Buffer.clear(buffer);
  (isOk, message);
};

let eval = (~send, code) => {
  let rec loop = status =>
    fun
    | [] => status
    | [phrase, ...tl] => {
        let loc: option(Core.Loc.t) =
          locFromPhrase(phrase) |> Option.flatMap(Core.Loc.toLocation);

        switch (eval_phrase(phrase)) {
        | (true, "") => loop(status, tl)
        | (true, msg) =>
          send(protocolSuccess(~loc?, ~msg));
          loop(status, tl);
        | (false, msg) =>
          send(protocolError(~loc?, ~error={loc, message: msg}));
          Evstt_error;
        };
      };

  try (
    {
      let filename = "//toplevel//";
      let lexbuf = Lexing.from_string(code);
      Location.init(lexbuf, filename);
      Location.input_name := filename;
      Location.input_lexbuf := Some(lexbuf);
      loop(Evstt_ok, Toploop.parse_use_file^(lexbuf));
    }
  ) {
  | Sys.Break =>
    send(protocolInterrupt());
    Evstt_abort;
  | exn =>
    let (loc, msg) = Error.extractInfo(exn);
    send(protocolError(~loc?, ~error={loc: loc, message: msg}));
    Evstt_error;
  };
};
