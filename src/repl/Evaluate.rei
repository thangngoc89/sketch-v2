let eval:
  (
    ~send: Core.Evaluate.result => unit,
    ~complete: Core.Evaluate.evalResult => unit,
    ~readStdout: (unit, unit) => string=?,
    string
  ) =>
  unit;
