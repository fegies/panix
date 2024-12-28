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
    length = ___builtin_length;
    filter = func: list:
      ___builtin_filter_pick {
        inherit list;
        pickSet = map func list;
      };
    isString = arg: typeOf arg == "string";
    isInt = arg: typeOf arg == "int";
    split = regex: str: ___builtin_split {inherit regex str;};
    splitOnVersionSep = split "\\.|-";
    isNotEmpty = a: isString a && a != "";
    pickNonEmpty = filter isNotEmpty;
    splitVersion = vers: pickNonEmpty (splitOnVersionSep vers);
    elemAt = list: idx: ___builtin_elemat {inherit list idx;};
    concatLists = ___builtin_concatLists;
    fromJSON = ___builtin_fromJSON;
    tryEval = ___builtin_tryeval;
    concatMap = f: list: concatLists (map f list);
  in {
    inherit
      builtins
      concatMap
      throw
      toString
      map
      abort
      typeOf
      isInt
      filter
      split
      isString
      elemAt
      splitVersion
      fromJSON
      concatLists
      tryEval
      length
      ;
    true = true;
    false = false;
    null = null;
    nixVersion = "2.24.10";
    isAttrs = arg: typeOf arg == "set";
    isBool = arg: typeOf arg == "bool";
    isFloat = arg: typeOf arg == "float";
    isFunction = arg: typeOf arg == "lambda";
    isList = arg: typeOf arg == "list";
    isNull = arg: arg == null;
    isPath = arg: typeOf arg == "path";
    langVersion = 6;
    hasAttr = s: set: set ? "${s}";
    getAttr = s: set: set."${s}";
    catAttrs = attr:
      concatMap (
        set:
          if set ? "${attr}"
          then [set."${attr}"]
          else []
      );
  };
in
  __body__
