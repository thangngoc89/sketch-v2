open TestFramework;

let initialize = () => {
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
};

describe("my first test suite", ({test, _}) => {
  test("it should run", ({expect}) => {
    initialize();
    let send = _ => ();
    let _execResult = Repl.Evaluate.eval(~send, "let a = 1;");

    expect.int(1).toBe(1);
  });
  test("it should error", ({expect}) => {
    initialize();
    let send = _ => ();
    let _execResult =
      Repl.Evaluate.eval(~send, {|let a = 1;\nlet b = "2";a + b;|});

    expect.int(1).toBe(1);
  });
});
