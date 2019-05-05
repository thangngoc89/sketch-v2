module IntMap =
  Map.Make({
    type t = int;
    let compare = Pervasives.compare;
  });

let state = ref(IntMap.empty);

let reset = () => state := IntMap.empty;

let add = lineNum => {
  state := state^ |> IntMap.add(lineNum, ToploopState.get());
};

let printValue = value => Toploop.getvalue(value) |> Console.log;

let printValueCustom = (value, bindings) =>
  Toploop.StringMap.find(value, bindings) |> Console.log;
