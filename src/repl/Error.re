open Util;
let extractLocation =
  fun
  | Syntaxerr.Error(x) => Some(Syntaxerr.location_of_error(x))
  | Lexer.Error(_, loc)
  | Typecore.Error(loc, _, _)
  | Typetexp.Error(loc, _, _)
  | Typeclass.Error(loc, _, _)
  | Typemod.Error(loc, _, _)
  | Typedecl.Error(loc, _)
  | Translcore.Error(loc, _)
  | Translclass.Error(loc, _)
  | Translmod.Error(loc, _) => Some(loc)
  | Reason_syntax_util.Error(loc, _) => Some(loc)
  | Reason_lexer.Error(_err, loc) => Some(loc)
  | _ => None;

let reportError = exn => {
  let b = Buffer.create(256);
  let ppf = Format.formatter_of_buffer(b);
  Errors.report_error(ppf, exn);
  Format.pp_print_flush(ppf, ());
  Buffer.contents(b);
};

let extractInfo = exn => {
  let locFromExn =
    extractLocation(exn) |> Option.flatMap(Core.Loc.toLocation);

  switch (Location.error_of_exn(exn)) {
  | None => (locFromExn, reportError(exn))
  | Some(`Already_displayed) => (locFromExn, reportError(exn))
  | Some(`Ok({loc, msg, sub, if_highlight})) =>
    let msg =
      msg
      ++ "\n"
      ++ (List.map(({Location.msg, _}) => msg, sub) |> String.concat("\n"));

    (
      switch (locFromExn) {
      | None => loc |> Core.Loc.toLocation
      | Some(loc) => Some(loc)
      },
      msg,
    );
  };
};
