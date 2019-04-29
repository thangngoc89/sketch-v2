open TestFramework;
module Protocol = Core.Protocol;

let initialize = () => {
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
};

let success = (msg, loc_start, loc_end) =>
  Protocol.Reply_ExecBlockContent({
    loc:
      Some({
        loc_start: {
          line: fst(loc_start),
          col: snd(loc_start),
        },
        loc_end: {
          line: fst(loc_end),
          col: snd(loc_end),
        },
      }),
    result: Ok(msg),
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
      success("let x: int = 1;", (0, 0), (0, 9)),
    );
    expect.equal(
      List.nth(calls, 1),
      success("let y: int = 2;", (0, 11), (0, 20)),
    );
    expect.equal(
      List.nth(calls, 2),
      success("let z: int = 3;", (0, 22), (0, 31)),
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
      success("let x: int = 1;", (0, 0), (0, 9)),
    );
    expect.equal(
      List.nth(calls, 1),
      success("let y: int = 2;", (1, 0), (1, 9)),
    );
    expect.equal(
      List.nth(calls, 2),
      success("let z: int = 3;", (2, 0), (2, 9)),
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
      success("let myFunc: unit => int = <fun>;", (0, 0), (2, 1)),
    );
  });
});

describe("error tests", ({test, _}) =>
  test("single line, error as last phrase", ({expect}) => {
    initialize();
    let send = _ => ();
    let mock = Mock.mock1(send);
    let _execResult =
      Repl.Evaluate.eval(
        ~send=Mock.fn(mock),
        {|let a = 1;\nlet b = "2"; a + b;|},
      );

    expect.mock(mock).toBeCalledTimes(3);
  })
);
