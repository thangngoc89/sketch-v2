type capture = {
  backup_fd: Unix.file_descr,
  new_out_fd: Unix.file_descr,
  new_in_fd: Unix.file_descr,
};

let size = 50;

let start = () => {
  let backup_fd = Unix.dup(Unix.stdout);
  let (new_out_fd, new_in_fd) = Unix.pipe(~cloexec=true, ());
  Unix.dup2(new_in_fd, Unix.stdout);

  {backup_fd, new_out_fd, new_in_fd};
};

let stop = ({backup_fd, new_out_fd, new_in_fd}) => {
  let b = Bytes.create(size);
  let buffer = Buffer.create(100);
  let rec read_toplevel_stdout = () =>
    switch (Unix.select([new_out_fd], [], [], 0.)) {
    | ([a], _, _) =>
      let n = Unix.read(new_out_fd, b, 0, size);
      Buffer.add_subbytes(buffer, b, 0, n);
      if (n == size) {
        read_toplevel_stdout();
      };
    | _ => ()
    };
  flush(stdout);
  read_toplevel_stdout();
  Unix.dup2(backup_fd, Unix.stdout);

  let r = Buffer.contents(buffer);
  Buffer.reset(buffer);
  r;
};
