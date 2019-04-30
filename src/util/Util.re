module Option = {
  let flatMap = f =>
    fun
    | None => None
    | Some(a) => f(a);

  let map = f =>
    fun
    | None => None
    | Some(a) => Some(f(a));
  let getWithDefault = v =>
    fun
    | None => v
    | Some(v) => v;
};
