let
  throw = ___builtin_throw;
  toString = arg:
    if ___builtin_typeof arg == "set"
    then
      toString (
        if arg ? __toString
        then arg.__toString arg
        else arg.outPath or (throw "cannot coerce a set to a string")
      )
    else ___builtin_tostring arg;
  map = func: list: ___builtin_map {inherit func list;};
  import = ___builtin_import;
  abort = ___builtin_abort;
  builtins = import "<<<___builtins>>>";
in
  __body__
