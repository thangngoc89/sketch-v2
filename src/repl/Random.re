type loc = {
  loc_start: (int, int),
  loc_end: (int, int),
};

type error = {
  msg: string,
  locs: list(loc),
  if_highlight: string,
};

type warning = error;

type executeResult('a) =
  | ExecuteSuccess('a, list(warning))
  | ExecuteError(error, list(warning));

let warnings = ref([]);

let return_success = e => ExecuteSuccess(e, warnings^);
let return_error = e => ExecuteError(e, warnings^);

let convert_loc = loc => {
  let (_file1, line1, col1) = Location.get_pos_info(loc.Location.loc_start);
  let (_file2, line2, col2) = Location.get_pos_info(loc.Location.loc_end);
  {loc_start: (line1, col1), loc_end: (line2, col2)};
};

let init_loc = (lb, filename) => {
  Location.input_name := filename;
  Location.input_lexbuf := Some(lb);
  Location.init(lb, filename);
};

let dummy_ppf = Format.make_formatter((_, _, _) => (), () => ());

let rec report_error_rec =
        (hg_ppf, ppf, {Location.loc, msg, sub, if_highlight}) => {
  Location.print(ppf, loc);
  Format.pp_print_string(ppf, msg);
  let hg_ppf =
    if (if_highlight != "") {
      Format.pp_print_string(hg_ppf, if_highlight);
      dummy_ppf;
    } else {
      Format.pp_print_string(hg_ppf, msg);
      hg_ppf;
    };
  let locs =
    List.concat @@
    List.map(
      err => {
        Format.pp_force_newline(ppf, ());
        Format.pp_open_box(ppf, 2);
        let locs = report_error_rec(hg_ppf, ppf, err);
        Format.pp_close_box(ppf, ());
        locs;
      },
      sub,
    );
  [convert_loc(loc), ...locs];
};

let report_error = err => {
  let buf = Buffer.create(503);
  let ppf = Format.formatter_of_buffer(buf);
  let hg_buf = Buffer.create(503);
  let hg_ppf = Format.formatter_of_buffer(hg_buf);
  let locs = report_error_rec(hg_ppf, ppf, err);
  Format.pp_print_flush(ppf, ());
  Format.pp_print_flush(hg_ppf, ());
  let msg = Buffer.contents(buf);
  let if_highlight = Buffer.contents(hg_buf);
  {msg, locs, if_highlight};
};

let error_of_exn = exn =>
  switch (Location.error_of_exn(exn)) {
  | None =>
    let msg = Printexc.to_string(exn);
    {msg, locs: [], if_highlight: msg};
  | Some(`Ok(error)) => report_error(error)
  | Some(`Already_displayed) => failwith("Unhandled case")
  };

let return_exn = exn => return_error(error_of_exn(exn));

let refill_lexbuf = (s, p, ppf, buffer, len) =>
  if (p^ == String.length(s)) {
    0;
  } else {
    let (len', nl) =
      try (String.index_from(s, p^, '\n') - p^ + 1, false) {
      | _ => (String.length(s) - p^, true)
      };
    let len'' = min(len, len');
    String.blit(s, p^, buffer, 0, len'');
    switch (ppf) {
    | Some(ppf) =>
      Format.fprintf(ppf, "%s", Bytes.sub_string(buffer, 0, len''));
      if (nl) {
        Format.pp_print_newline(ppf, ());
      };
      Format.pp_print_flush(ppf, ());
    | None => ()
    };
    p := p^ + len'';
    len'';
  };

let execute = (~ppf_code=?, ~print_outcome=true, ~ppf_answer, code) => {
  let lb =
    switch (ppf_code) {
    | Some(ppf_code) =>
      Lexing.from_function(refill_lexbuf(code, ref(0), Some(ppf_code)))
    | None => Lexing.from_string(code)
    };
  init_loc(lb, "//toplevel//");
  warnings := [];
  let rec loop = () => {
    let phr = Toploop.parse_toplevel_phrase^(lb);
    /* let phr = JsooTopPpx.preprocess_phrase(phr); */
    let success = Toploop.execute_phrase(print_outcome, ppf_answer, phr);
    Format.pp_print_flush(ppf_answer, ());
    if (success) {
      loop();
    } else {
      return_success(false);
    };
  };
  try (
    {
      let res = loop();
      flush_all();
      res;
    }
  ) {
  | End_of_file =>
    flush_all();
    return_success(true);
  | exn =>
    flush_all();
    return_error(error_of_exn(exn));
  };
};

let buffer = Buffer.create(100);
let stdout_buffer = Buffer.create(100);
let stderr_buffer = Buffer.create(100);

let formatter = Format.formatter_of_buffer(buffer);

let _ = {
  SyntaxControl.re();
  let result = execute(~ppf_answer=formatter, "foo");
  switch (result) {
  | ExecuteSuccess(_) => print_endline("success")
  | ExecuteError(_) => print_endline("error")
  };
};
