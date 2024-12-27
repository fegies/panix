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
  builtins = let
    typeOf = ___builtin_typeof;
  in {
    inherit builtins throw toString map abort typeOf;
    true = true;
    false = false;
    null = null;
    tryEval = ___builtin_tryeval;
    nixVersion = "2.24.10";
    split = regex: str: ___builtin_split {inherit regex str;};
    isAttrs = arg: typeOf arg == "set";
    isBool = arg: typeOf arg == "bool";
    isFloat = arg: typeOf arg == "float";
    isFunction = arg: typeOf arg == "lambda";
    isInt = arg: typeOf arg == "int";
    isList = arg: typeOf arg == "list";
    isNull = arg: arg == null;
    isPath = arg: typeOf arg == "path";
    isString = arg: typeOf arg == "string";
    langVersion = 6;
  };
in
  __body__
