/* let print = (~title, msg) => {
     "\n"
     ++ Pastel.(
          <Pastel>
            <Pastel bold=true backgroundColor=Green color=Black> title </Pastel>
            {"\n" ++ msg}
          </Pastel>
        )
     ++ "\n"
     |> Console.log;
   };
   let printError: Location.error => unit =
     ({msg, _}) => {
       print(~title="PrintError", msg) |> Console.log;
     }; */

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
  | Some(`Ok({loc, msg, sub, if_highlight})) => (
      Some(loc |> Core.Loc.toLocation),
      msg,
    )
  };
};
