/*
 * From https://github.com/ocaml/ocaml/blob/9f904f9a5c3752df296e5d04a50970a90addf7a7/tools/caml_tex.ml#L102-L125
 */
let read_stdout = () => {
  let oldstdout = Unix.dup(Unix.stdout);
  let (stdout_out, stdout_in) = Unix.pipe(~cloexec=true, ());
  let () = Unix.dup2(stdout_in, Unix.stdout);

  let size = 50;
  let b = Bytes.create(size);
  let buffer = Buffer.create(100);
  let rec read_toplevel_stdout = () =>
    switch (Unix.select([stdout_out], [], [], 0.)) {
    | ([a], _, _) =>
      let n = Unix.read(stdout_out, b, 0, size);
      Buffer.add_subbytes(buffer, b, 0, n);
      if (n == size) {
        read_toplevel_stdout();
      };
    | _ => ()
    };

  () => {
    flush(stdout);
    read_toplevel_stdout();
    Unix.dup2(oldstdout, Unix.stdout);

    let r = Buffer.contents(buffer);
    Buffer.reset(buffer);
    r;
  };
};
