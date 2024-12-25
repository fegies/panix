let
  throw = ___builtin_throw;
  toString = arg:
    if builtins.typeOf arg == "set"
    then
      toString (
        if arg ? __toString
        then arg.__toString arg
        else arg.outPath or (throw "cannot coerce a set to a string")
      )
    else ___builtin_tostring arg;
  map = func: list: ___builtin_map {inherit func list;};
  builtins = {
    inherit builtins throw toString map;
    true = true;
    false = false;
    null = null;
    tryEval = ___builtin_tryeval;
    typeOf = ___builtin_typeof;
  };
in
  __body__
