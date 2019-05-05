module IntMap =
  Map.Make({
    type t = int;
    let compare = Pervasives.compare;
  });

let state = ref(IntMap.empty);

let add = lineNum => {
  state := state^ |> IntMap.add(lineNum, ToploopState.get());
};

