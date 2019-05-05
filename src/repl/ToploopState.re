type t = {
  env: Env.t,
  value: string,
};

let flags = [Marshal.Closures];

let get = () => {
  {
    env: Toploop.toplevel_env^,
    value: Marshal.to_string(Toploop.toplevel_value_bindings^, flags),
  };
};

let set = ({env, value}) => {
  Toploop.toplevel_env := env;
  Toploop.toplevel_value_bindings := Marshal.from_string(value, 0);
};
