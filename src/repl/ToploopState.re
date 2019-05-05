type t = {
  env: Env.t,
  value: Toploop.StringMap.t(Obj.t),
};

let get = () => {
  {env: Toploop.toplevel_env^, value: Toploop.toplevel_value_bindings^};
};

let set = ({env, value}) => {
  Toploop.toplevel_env := env;
  Toploop.toplevel_value_bindings := value;
};
