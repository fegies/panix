# the purpose of this file is to define
# the builtins attribute set.
# the 4 bindings of the prelude are always available.
let
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
  match = regex: str: ___builtin_match [regex str];
  concatMap = f: list: concatLists (map f list);
  seq = e1: e2: ___builtin_seq [e1 e2];
  deepSeq = e1: e2: ___builtin_deepSeq [e1 e2];
  toString = arg:
    if ___builtin_typeof arg == "set"
    then
      toString (
        if arg ? __toString
        then arg.__toString arg
        else arg.outPath or (throw "cannot coerce a set to a string")
      )
    else ___builtin_tostring arg;
  rawBuiltins = {
    inherit
      concatMap
      throw
      toString
      map
      deepSeq
      abort
      typeOf
      match
      isInt
      filter
      split
      isString
      isFloat
      elemAt
      splitVersion
      fromJSON
      concatLists
      seq
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

    baseNameOf = let
      matcher = match "^.*?([^/]*)/?$";
      result = name: let
        matchResult = matcher name;
      in
        if matchResult == null
        then name
        else elemAt matchResult 0;
    in
      seq matcher result;

    catAttrs = attr:
      concatMap (
        set:
          if set ? "${attr}"
          then [set."${attr}"]
          else []
      );

    mapAttrs = func: attrset: ___builtin_mapAttrs [func attrset];

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

    removeAttrs = set: list: ___builtin_removeAttrs [set list];

    compareVersions = let
      unpackList = v:
        if typeOf v == "list"
        then elemAt v 0
        else v;
      # splitLeadingDigits: string -> [string|[string]]
      splitLeadingDigits = split "^([0-9]+)";
      # isNotEmptyString: any -> bool
      isNotEmptyString = v: v != "";
      # splitOffAlpha: string -> [string]
      # example: "0123" -> ["0123"]
      # example: "abc123" -> ["abc123"]
      # example: "123abc" -> ["123" "abc"]
      splitOffAlpha = value:
        if value == ""
        then [value]
        else map unpackList (filter isNotEmptyString (splitLeadingDigits value));
      # already partially applied concatMap
      concatMap_splitOffAlpha = concatMap splitOffAlpha;
      # a function string -> [string], extension of splitVersion.
      # first splits on the version separators, then splits each item again
      # on leading digits if any available. We do this to match what upstream nix does.
      deepSplitVersion = input: concatMap_splitOffAlpha (splitVersion input);
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

    genList = let
      # we go with radix 8 to improve perf a little and reduce recursion depth.
      # we could go wider, but the cases would need to be hardcoded, or there would be little benefit over just recursing one level deeper.
      genListInner = generator: length: let
        iter = length: offset:
        # seq the offset to avoid a deep thunk chain being carried onto the generated list
          seq offset
          ( # the 0 length case is handled by the outer function already.
            if length <= 8
            then
              ( # base cases.
                if length <= 4 # it is 1 or 2
                then
                  (
                    if length <= 2
                    then
                      (
                        if length == 1
                        then [(generator offset)]
                        else [(generator offset) (generator (offset + 1))] # length 2
                      )
                    else # it must be > 2 and <= 4
                      (
                        if length == 3
                        then [(generator offset) (generator (offset + 1)) (generator (offset + 2))]
                        else #length 4
                          [(generator offset) (generator (offset + 1)) (generator (offset + 2)) (generator (offset + 3))]
                      )
                  )
                else
                  ( # we know it is > 4 and <= 8
                    if length <= 6
                    then
                      (
                        if length == 5
                        then [(generator offset) (generator (offset + 1)) (generator (offset + 2)) (generator (offset + 3)) (generator (offset + 4))]
                        else # length 6
                          [(generator offset) (generator (offset + 1)) (generator (offset + 2)) (generator (offset + 3)) (generator (offset + 4)) (generator (offset + 5))]
                      )
                    else # > 6 and <= 8
                      (
                        if length == 7
                        then [(generator offset) (generator (offset + 1)) (generator (offset + 2)) (generator (offset + 3)) (generator (offset + 4)) (generator (offset + 5)) (generator (offset + 6))]
                        else #length 8
                          [(generator offset) (generator (offset + 1)) (generator (offset + 2)) (generator (offset + 3)) (generator (offset + 4)) (generator (offset + 5)) (generator (offset + 6)) (generator (offset + 7))]
                      )
                  )
              )
            else let
              # recursion....
              step_size = length / 8;
              iter_step_size = iter step_size;
            in
              concatLists [
                (iter_step_size offset)
                (iter_step_size (offset + step_size))
                (iter_step_size (offset + 2 * step_size))
                (iter_step_size (offset + 3 * step_size))
                (iter_step_size (offset + 4 * step_size))
                (iter_step_size (offset + 5 * step_size))
                (iter_step_size (offset + 6 * step_size))
                # the last one may have a slightly smaller step
                (iter (length - 7 * step_size) (offset + 7 * step_size))
              ]
          );
      in
        iter length 0;
    in
      # force the materialization of the inner
      seq genListInner (
        generator: length:
          if length == 0
          then []
          else if typeOf length == "int"
          then genListInner generator length
          else throw "genList: length must be an integer"
      );
  };
in
  # avoid carrying around the thunks for all of our builtins lambdas.
  # we need to add the builtins as a recursive key after the deepseq to avoid
  # an infinite recursion.
  deepSeq rawBuiltins (
    let
      builtins = rawBuiltins // {inherit builtins;};
    in
      builtins
  )
