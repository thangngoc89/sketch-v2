type loc = {
  loc_start: (int, int),
  loc_end: (int, int),
};

type exec_result = {
  exres_loc: loc,
  exres_content: result(string, string),
};
