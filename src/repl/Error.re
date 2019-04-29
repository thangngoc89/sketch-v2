let reportError = exn => {
  let b = Buffer.create(256);
  let ppf = Format.formatter_of_buffer(b);
  Errors.report_error(ppf, exn);
  Format.pp_print_flush(ppf, ());
  Buffer.contents(b);
};

let extractInfo = exn => {
  switch (Location.error_of_exn(exn)) {
  | None => (None, reportError(exn))
  | Some(`Already_displayed) => (None, reportError(exn))
  | Some(`Ok({loc, msg, sub, if_highlight})) =>
    sub
    |> List.iter(({Location.msg, loc, _}) => {
         Console.log(loc |> Core.Loc.toLocation);
         Console.log(msg);
       });
    (loc |> Core.Loc.toLocation, msg);
  };
};
