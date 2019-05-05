type t = {
  env: Env.t,
  value: Toploop.StringMap.t(Obj.t),
  global_map: Symtable.global_map,
};

let get = () => {
  {
    env: Toploop.toplevel_env^,
    value: Toploop.toplevel_value_bindings^,
    global_map: Symtable.current_state(),
  };
};

let set = ({env, value, global_map}) => {
  Toploop.toplevel_env := env;
  Toploop.toplevel_value_bindings := value;
  Symtable.restore_state(global_map);
};
