open TestFramework;
open Core.Evaluate;
open Util;

let initialize = () => {
  Toploop.initialize_toplevel_env();
  Toploop.input_name := "//toplevel//";
  Repl.SyntaxControl.re();
};

let eval = Repl.Evaluate.eval(~readStdout=(module ReadStdoutUnix));

let makeLoc = (locStart, locEnd) => {
  Core.Loc.{
    locStart: {
      line: fst(locStart),
      col: snd(locStart),
    },
    locEnd: {
      line: fst(locEnd),
      col: snd(locEnd),
    },
  };
};

let makeWarning = (~sub=[], ~number, ~msg, block_start, block_end) => {
  {
    warnLoc: Some(makeLoc(block_start, block_end)),
    warnNumber: number,
    warnMsg: msg,
    warnSub: sub,
  };
};

let success = (~warnings=[], ~stdout="", msg, block_start, block_end) =>
  Phrase({
    blockLoc: Some(makeLoc(block_start, block_end)),
    blockContent: BlockSuccess({msg, warnings}),
    blockStdout: stdout,
  });

let error =
    (
      ~warnings=[],
      ~stdout="",
      ~errSub=[],
      msg,
      block_loc,
      error_start,
      error_end,
    ) => {
  Phrase({
    blockLoc:
      block_loc
      |> Util.Option.map(((block_start, block_end)) =>
           makeLoc(block_start, block_end)
         ),
    blockContent:
      BlockError({
        error: {
          errLoc: Some(makeLoc(error_start, error_end)),
          errMsg: msg,
          errSub,
        },
        warnings,
      }),
    blockStdout: stdout,
  });
};

describe("evaluate state test", ({test, _}) =>
  test("simple test", ({expect}) => {
    initialize();

    let mockComplete = Mock.mock1(_ => ());

    eval(
      ~send=_ => (),
      ~complete=Mock.fn(mockComplete),
      "let x = 1;\nlet y = 2;",
    );
    expect.mock(mockComplete).toBeCalledWith(EvalSuccess);
    open Repl.State;

    let beforeLetY = state^ |> IntMap.find_opt(0);
    switch (beforeLetY) {
    | None => ()
    | Some(topState) => Repl.ToploopState.set(topState)
    };

    let mockSend = Mock.mock1(_ => ());
    let mockComplete = Mock.mock1(_ => ());

    eval(
      ~send=Mock.fn(mockSend),
      ~complete=Mock.fn(mockComplete),
      "print_int(y)",
    );
    expect.mock(mockComplete).toBeCalledWith(EvalError);
    expect.mock(mockSend).toBeCalledWith(
      error(
        "Unbound value y",
        Some(((0, 0), (0, 11))),
        (0, 10),
        (0, 10),
      ),
    );
  })
);
