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
  import = arg: ___builtin_import (toString arg);
  abort = ___builtin_abort;
  builtins = {
    inherit builtins throw toString map abort;
    true = true;
    false = false;
    null = null;
    tryEval = ___builtin_tryeval;
    typeOf = ___builtin_typeof;
    nixVersion = "2.24.10";
    split = regex: str: ___builtin_split {inherit regex str;};
  };
in
  __body__
