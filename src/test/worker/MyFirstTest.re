open TestFramework;

describe("my first test suite", ({test, _}) => {
  test("1 + 1 should equal 2", ({expect}) => {
    WorkerLib.Execute.eval("let a = 1;") |> ignore;
    expect.int(1 + 1).toBe(2);
  });
});
