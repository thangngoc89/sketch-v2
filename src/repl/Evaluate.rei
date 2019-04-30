let eval:
  (
    ~send: Core.Evaluate.blockResult => unit,
    ~complete: Core.Evaluate.evalResult => unit,
    string
  ) =>
  unit;
