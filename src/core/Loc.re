module Position = {
  [@deriving show]
  type t = {
    line: int,
    col: int,
  };

  let toPosition: Lexing.position => t =
    pos => {line: pos.Lexing.pos_lnum - 1, col: pos.pos_cnum - pos.pos_bol};
};

[@deriving show]
type t = {
  loc_start: Position.t,
  loc_end: Position.t,
};

let toLocation: Location.t => t =
  ({loc_start, loc_end, _}) => {
    {
      loc_start: loc_start |> Position.toPosition,
      loc_end: loc_end |> Position.toPosition,
    };
  };
