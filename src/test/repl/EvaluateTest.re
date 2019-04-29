open TestFramework;
module Protocol = Core.Protocol;

let initialize = () => {
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
};

let success = (msg, block_start, block_end) =>
  Protocol.Reply_ExecBlockContent({
    loc:
      Some({
        loc_start: {
          line: fst(block_start),
          col: snd(block_start),
        },
        loc_end: {
          line: fst(block_end),
          col: snd(block_end),
        },
      }),
    result: Ok(msg),
  });

let error = (msg, block_start, block_end, error_start, error_end) =>
  Protocol.Reply_ExecBlockContent({
    loc:
      Some({
        loc_start: {
          line: fst(block_start),
          col: snd(block_start),
        },
        loc_end: {
          line: fst(block_end),
          col: snd(block_end),
        },
      }),
    result:
      Error({
        loc:
          Some({
            loc_start: {
              line: fst(error_start),
              col: snd(error_start),
            },
            loc_end: {
              line: fst(error_end),
              col: snd(error_end),
            },
          }),
        message: msg,
      }),
  });

let spySend = () => {
  let calls = ref([]);
  let send = reply => calls := [reply, ...calls^];
  (calls, send);
};

describe("success test", ({test, _}) => {
  test("single line, multiple phrases", ({expect}) => {
    initialize();
    let (calls, send) = spySend();

    Repl.Evaluate.eval(~send, "let x = 1; let y = 2; let z = 3;") |> ignore;

    let calls = calls^ |> List.rev;
    expect.int(calls |> List.length).toBe(3);
    expect.equal(
      List.nth(calls, 0),
      success("let x: int = 1;", (0, 0), (0, 8)),
    );
    expect.equal(
      List.nth(calls, 1),
      success("let y: int = 2;", (0, 11), (0, 19)),
    );
    expect.equal(
      List.nth(calls, 2),
      success("let z: int = 3;", (0, 22), (0, 30)),
    );
  });
  test("multiple lines, multiple phrases", ({expect}) => {
    initialize();
    let (calls, send) = spySend();

    Repl.Evaluate.eval(~send, "let x = 1;\nlet y = 2;\nlet z = 3;") |> ignore;

    let calls = calls^ |> List.rev;
    expect.int(calls |> List.length).toBe(3);
    expect.equal(
      List.nth(calls, 0),
      success("let x: int = 1;", (0, 0), (0, 8)),
    );
    expect.equal(
      List.nth(calls, 1),
      success("let y: int = 2;", (1, 0), (1, 8)),
    );
    expect.equal(
      List.nth(calls, 2),
      success("let z: int = 3;", (2, 0), (2, 8)),
    );
  });

  test("single phrases in multiple lines", ({expect}) => {
    initialize();
    let (calls, send) = spySend();

    Repl.Evaluate.eval(~send, "let myFunc = () => {\n  1\n}") |> ignore;

    let calls = calls^ |> List.rev;
    expect.int(calls |> List.length).toBe(1);
    expect.equal(
      List.nth(calls, 0),
      success("let myFunc: unit => int = <fun>;", (0, 0), (2, 0)),
    );
  });
});

describe("error tests", ({test, _}) => {
  test("syntax error", ({expect}) => {
    let send = _ => ();
    let mock = Mock.mock1(send);
    Repl.Evaluate.eval(~send=Mock.fn(mock), {|let a = {|});

    expect.mock(mock).toBeCalledTimes(1);
    expect.mock(mock).toBeCalledWith(
      error(
        "Syntax error: '}' expected\nThis '{' might be unmatched",
        (0, 8),
        (0, 8),
        (0, 8),
        (0, 8),
      ),
    );
  });

  test("single line, error as last phrase", ({expect}) => {
    initialize();
    let (calls, send) = spySend();

    let _execResult =
      Repl.Evaluate.eval(~send, "let a = 1; let b = \"2\"; a + b;");
    let calls = List.rev(calls^);
    expect.int(calls |> List.length).toBe(3);

    expect.equal(
      List.nth(calls, 0),
      success("let a: int = 1;", (0, 0), (0, 8)),
    );

    expect.equal(
      List.nth(calls, 1),
      success("let b: string = \"2\";", (0, 11), (0, 21)),
    );

    expect.equal(
      List.nth(calls, 2),
      error(
        "This expression has type string but an expression was expected of type\n         int\n",
        (0, 24),
        (0, 28),
        (0, 28),
        (0, 28),
      ),
    );
  });
});
