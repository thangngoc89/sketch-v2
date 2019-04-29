type error = {
  loc: option(Loc.t),
  message: string,
};

type reply =
  | Reply_ExecBlockContent{
      loc: option(Loc.t),
      result: result(string, error),
    }
  | Reply_ExecInterupted;
