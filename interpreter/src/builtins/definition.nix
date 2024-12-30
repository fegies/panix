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
    isFloat = arg: typeOf arg == "float";
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
      isFloat
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
    partition = predicate: list: let
      pickSetRight = map predicate list;
    in {
      right = ___builtin_filter_pick {
        inherit list;
        pickSet = pickSetRight;
      };
      wrong = ___builtin_filter_pick {
        inherit list;
        pickSet = map (v: !v) pickSetRight;
      };
    };
    lessThan = let
      isNum = a: let type = typeOf a; in type == "int" || type == "float";
    in
      e1:
        if isNum e1
        then
          e2:
            if isNum e1
            then e1 < e2
            else throw "lessThan: e2 must be a number"
        else throw "lessThan: e1 must be a number";

    compareVersions = let
      # a function string -> [string], extension of splitVersion.
      # first splits on the version separators, then splits each item again
      # on leading digits if any available. We do this to match what upstream nix does.
      deepSplitVersion = let
        unpackList = v:
          if typeOf v == "list"
          then elemAt v 0
          else v;
        # isNotEmptyString: any -> bool
        isNotEmptyString = v: v != "";
        # splitLeadingDigits: string -> [string|[string]]
        splitLeadingDigits = split "^([0-9]+)";
        # splitOffAlpha: string -> [string]
        # example: "0123" -> ["0123"]
        # example: "abc123" -> ["abc123"]
        # example: "123abc" -> ["123" "abc"]
        splitOffAlpha = value:
          if value == ""
          then [value]
          else map unpackList (filter isNotEmptyString (splitLeadingDigits value));
      in let
        # already partially applied concatMap
        concatMap_splitOffAlpha = concatMap splitOffAlpha;
      in
        input: concatMap_splitOffAlpha (splitVersion input);
      # simpleCompare: any -> any -> -1|0|1
      simpleCompare = left: right:
        if left < right
        then -1
        else if right < left
        then 1
        else 0;
      # tryParseInt : string -> int|null
      tryParseInt = value: let
        parsed = (tryEval (fromJSON value)).value;
      in
        if typeOf parsed == "int"
        then parsed
        else null;
      # compareComponents: string -> string -> -1|0|1
      compareComponents = left: right:
        if left == right
        then 0
        else let
          left_int = tryParseInt left;
          right_int = tryParseInt right;
        in
          if left_int != null && right_int != null
          then simpleCompare left_int right_int
          else if left == "pre"
          then -1
          else if right == "pre"
          then 1
          else if right_int != null
          then -1
          else if left_int != null
          then 1
          else simpleCompare left right;
    in
      v1: let
        v1_split = deepSplitVersion v1;
        v1_len = length v1_split;
      in
        v2: let
          v2_split = deepSplitVersion v2;
          v2_len = length v2_split;
          max_len =
            if v1_len <= v2_len
            then v1_len
            else v2_len;
          compare_iter = idx:
            if idx == max_len
            then
              (
                if v1_len < v2_len
                then 1
                else if v2_len < v1_len
                then -1
                else 0
              )
            else let
              res = compareComponents (elemAt v1_split idx) (elemAt v2_split idx);
            in
              if res != 0
              then res
              else compare_iter (idx + 1);
        in
          compare_iter 0;
  };
in
  __body__
