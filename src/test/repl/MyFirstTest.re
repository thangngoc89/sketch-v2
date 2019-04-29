open TestFramework;

describe("my first test suite", ({test, _}) => {
  test("it should run", ({expect}) => {
    let send = () => Console.log("send");
    let execResult = Repl.Evaluate.eval(~send, "let a = 1;");

    execResult |> Repl.Evaluate.show_evalStatus |> Console.log;
    
    expect.int(1).toBe(1);
  });
  test("it should error", ({expect}) => {
    let send = () => Console.log("send");
    let execResult = Repl.Evaluate.eval(~send, "let a: string = 1; ");

    execResult |> Repl.Evaluate.show_evalStatus |> Console.log;
    
    expect.int(1).toBe(1);
  })
});
