let to_string_hum = (~ctx_size: _, exn) => {
  /* let report = (ppf, err) => pp_print_error(~ctx_size, ppf, (exn, err));
     Location.error_reporter := report; */
  let b = Buffer.create(256);
  let ppf = Format.formatter_of_buffer(b);
  Errors.report_error(ppf, exn);
  Format.pp_print_flush(ppf, ());
  Buffer.contents(b);
};
