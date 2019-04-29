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
  try (
    {
      let isOk = Toploop.execute_phrase(true, ppf, phrase);
      let message = Buffer.contents(buffer);
      Buffer.clear(buffer);
      Ok((isOk, message));
    }
  ) {
  | exn => Error(exn)
  };
};

let eval =
    (~send: Protocol.reply => unit, ~complete=() => (), code: string): unit => {
  let rec loop =
    fun
    | [] => complete()
    | [phrase, ...tl] => {
        let loc: option(Core.Loc.t) =
          locFromPhrase(phrase) |> Option.flatMap(Core.Loc.toLocation);

        switch (eval_phrase(phrase)) {
        | Ok((true, "")) => loop(tl)
        | Ok((true, msg)) =>
          send(protocolSuccess(~loc?, ~msg));
          loop(tl);
        | Ok((false, msg)) =>
          /* No ideas when this happens */
          send(protocolError(~loc?, ~error={loc, message: msg}));
          complete();
        | Error(exn) =>
          let (loc', msg) = Error.extractInfo(exn);
          send(protocolError(~loc?, ~error={loc: loc', message: msg}));
        };
      };

  try (
    {
      let filename = "//toplevel//";
      let lexbuf = Lexing.from_string(code);
      Location.init(lexbuf, filename);
      Location.input_name := filename;
      Location.input_lexbuf := Some(lexbuf);
      loop(Toploop.parse_use_file^(lexbuf));
    }
  ) {
  | Sys.Break =>
    send(protocolInterrupt());
    complete();
  | exn =>
    let (loc, msg) = Error.extractInfo(exn);
    send(protocolError(~loc?, ~error={loc, message: msg}));
    complete();
  };
};
