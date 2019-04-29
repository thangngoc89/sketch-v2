[@deriving show]
type error = {
  loc: option(Loc.t),
  message: string,
};

[@deriving show]
type reply =
  | Reply_ExecBlockContent{
      loc: option(Loc.t),
      result: result(string, error),
    }
  | Reply_ExecInterupted;
