open TestFramework;

describe("my first test suite", ({test, _}) =>
  test("1 + 1 should equal 2", ({expect}) => {
    expect.string(Repl.Util.hello()).toEqual("Hello");
  })
);
