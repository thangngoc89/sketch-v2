module Option = {
  let map = f =>
    fun
    | None => None
    | Some(a) => Some(f(a));
};
